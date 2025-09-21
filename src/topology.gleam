import gleam/list
import gleam/io
import gleam/int
import node
import gleam/erlang/process.{type Subject}

// Create all actors
pub fn create_actors(count: Int) -> List(Subject(node.ActorMsg)) {
  list.range(1, count)
    |> list.map(node.init_actor)
}

// Establish connectivity based on topology
pub fn wire_up(actors: List(Subject(node.ActorMsg)), shape: String) -> Nil {
  case shape {
    "full" -> full_connect(actors)
    "line" -> line_connect(actors)
    "3D" -> grid3d_connect(actors)
    "imp3D" -> imp3d_connect(actors)
    _ -> {
      io.println("Unknown shape: " <> shape)
      io.println("Valid options: full, line, 3D, imp3D")
    }
  }
  io.println("Topology construction finished!")
}

// Fully connect each actor to all others
fn full_connect(actors: List(Subject(node.ActorMsg))) -> Nil {
  list.each(actors, fn(current) {
    let links = actors |> list.filter(fn(other) { other != current })
    node.assign_links(current, links)
  })
}

// Connect in line fashion
fn line_connect(actors: List(Subject(node.ActorMsg))) -> Nil {
  let total = list.length(actors)
  let with_index = list.index_map(actors, fn(a, i) { #(i, a) })
  
  list.each(with_index, fn(entry) {
    let #(idx, current) = entry
    let links = case idx {
      0 if total > 1 -> {
        case list.drop(actors, 1) {
          [next, ..] -> [next]
          [] -> []
        }
      }
      idx if idx == total - 1 && total > 1 -> {
        case list.drop(actors, total - 2) |> list.take(1) {
          [prev] -> [prev]
          _ -> []
        }
      }
      idx if idx > 0 && idx < total - 1 -> {
        let prev = case list.drop(actors, idx - 1) |> list.take(1) {
          [p] -> [p]
          _ -> []
        }
        let nxt = case list.drop(actors, idx + 1) |> list.take(1) {
          [n] -> [n]
          _ -> []
        }
        list.append(prev, nxt)
      }
      _ -> []
    }
    node.assign_links(current, links)
  })
}

// Connect in 3D grid
fn grid3d_connect(actors: List(Subject(node.ActorMsg))) -> Nil {
  let total = list.length(actors)
  let dim = cube_root(total)
  
  let with_index = list.index_map(actors, fn(a, i) { #(i, a) })
  
  list.each(with_index, fn(entry) {
    let #(idx, current) = entry
    
    let x = idx % dim
    let y = { idx / dim } % dim
    let z = idx / { dim * dim }
    
    let links = list.filter_map(with_index, fn(other) {
      let #(j, candidate) = other
      let ox = j % dim
      let oy = { j / dim } % dim
      let oz = j / { dim * dim }
      
      let xd = int.absolute_value(x - ox)
      let yd = int.absolute_value(y - oy)
      let zd = int.absolute_value(z - oz)
      
      case xd + yd + zd == 1 {
        True -> Ok(candidate)
        False -> Error(Nil)
      }
    })
    
    node.assign_links(current, links)
  })
}

// Cube root approximation
fn cube_root(n: Int) -> Int {
  case n {
    n if n <= 1 -> 1
    n if n <= 8 -> 2
    n if n <= 27 -> 3
    n if n <= 64 -> 4
    n if n <= 125 -> 5
    _ -> int.max(2, n / 10)
  }
}

// Imperfect 3D connection
fn imp3d_connect(actors: List(Subject(node.ActorMsg))) -> Nil {
  let total = list.length(actors)
  let dim = cube_root(total)
  
  let with_index = list.index_map(actors, fn(a, i) { #(i, a) })
  
  list.each(with_index, fn(entry) {
    let #(idx, current) = entry
    
    let x = idx % dim
    let y = { idx / dim } % dim
    let z = idx / { dim * dim }
    
    let base_links = list.filter_map(with_index, fn(other) {
      let #(j, candidate) = other
      let ox = j % dim
      let oy = { j / dim } % dim
      let oz = j / { dim * dim }
      
      let xd = int.absolute_value(x - ox)
      let yd = int.absolute_value(y - oy)
      let zd = int.absolute_value(z - oz)
      
      case xd + yd + zd == 1 {
        True -> Ok(candidate)
        False -> Error(Nil)
      }
    })
    
    let extra = pick_extra_neighbor(with_index, idx, base_links)
    let final_links = case extra {
      Ok(extra_link) -> [extra_link, ..base_links]
      Error(_) -> base_links
    }
    
    node.assign_links(current, final_links)
  })
}

// Pick one extra neighbor to make imperfect grid
fn pick_extra_neighbor(
  all: List(#(Int, Subject(node.ActorMsg))),
  idx: Int,
  existing: List(Subject(node.ActorMsg))
) -> Result(Subject(node.ActorMsg), Nil) {
  let available = list.filter_map(all, fn(entry) {
    let #(j, candidate) = entry
    let is_self = j == idx
    let is_neighbor = list.contains(existing, candidate)
    
    case is_self || is_neighbor {
      True -> Error(Nil)
      False -> Ok(candidate)
    }
  })
  
  case available {
    [first, ..] -> Ok(first)   // pseudo-random for now
    [] -> Error(Nil)
  }
}
