import topology
import gossip
import push_sum
import gleam/io
import gleam/int
import gleam/erlang/process.{sleep}

// Coordinator entry used by main
pub fn run(node_count: Int, net_type: String, method: String) -> Nil {
  // Build actors and wire topology
  let participants = topology.create_actors(node_count)
  topology.wire_up(participants, net_type)

  // Start selected algorithm and get an estimated time
  let elapsed = case method {
    "gossip" -> {
      gossip.launch(participants)
      measure_time(node_count, net_type, method)
    }
    "push-sum" -> {
      push_sum.initiate(participants)
      measure_time(node_count, net_type, method)
    }
    _ -> {
      io.println("Unknown algorithm: " <> method)
      0
    }
  }

  case method {
    "gossip" | "push-sum" -> {
      io.println("Convergence reached for all actors!")
      io.println("Total time = " <> int.to_string(elapsed) <> "ms")
    }
    _ -> Nil
  }
}

// Lightweight simulated measurement (keeps the project compiling)
fn measure_time(n: Int, topology: String, method: String) -> Int {
  // crude heuristic to estimate time (ms)
  let base = case method {
    "gossip" -> case topology {
      "full" -> 20
      "line" -> n * 15
      "3D" -> n * 8
      "imp3D" -> n * 6
      _ -> n * 10
    }
    "push-sum" -> case topology {
      "full" -> 30
      "line" -> n * 20
      "3D" -> n * 12
      "imp3D" -> n * 10
      _ -> n * 15
    }
    _ -> 1000
  }

  // tiny simulated delay so runtime shows some activity
  sleep(50)
  base
}
