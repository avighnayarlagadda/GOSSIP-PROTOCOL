// src/entry.gleam
import coordinator

pub fn main() -> Nil {
  // Default run configuration (same defaults your previous main used)
  // Change these values if you want a different default when running the project
  let default_nodes = 50
  let default_topology = "imp3D"
  let default_algorithm = "gossip"

  coordinator.run(default_nodes, default_topology, default_algorithm)
}
