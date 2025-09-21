import gleam/io
import gleam/int
import coordinator

pub fn main() -> Nil {
  // Simple console interface
  // Adjust these values to try different setups
  
  // Default setup:
  let node_count = 50
  let net_shape = "imp3D"
  let method = "gossip"
  
  io.println("=== Gleam Gossip Protocol Simulator ===")
  io.println(
    "Running: " <> int.to_string(node_count) <> 
    " participants, " <> net_shape <> " structure, " <> method <> " mode"
  )
  io.println("")
  
  initiate([int.to_string(node_count), net_shape, method])
  
  io.println("")
  io.println("To explore other setups, change the values in main.gleam:")
  io.println("Supported topologies: full, line, 3D, imp3D")
  io.println("Supported algorithms: gossip, push-sum")
}

// Alternative demo functions
pub fn demo_line_pushsum() -> Nil {
  initiate(["5", "line", "push-sum"])
}

pub fn demo_large_3d_gossip() -> Nil {
  initiate(["3000", "3D", "gossip"])
}

pub fn demo_full_pushsum() -> Nil {
  initiate(["5", "full", "push-sum"])
}

pub fn initiate(params: List(String)) -> Nil {
  case params {
    [num_str, net_shape, method] ->
      case int.parse(num_str) {
        Ok(total) -> coordinator.run(total, net_shape, method)
        Error(_) -> io.println("Invalid participant count")
      }
    _ ->
      io.println("Usage: <count> <topology: full|line|3d|imp3d> <algorithm: gossip|push-sum>")
  }
}
