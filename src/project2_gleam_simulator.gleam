import argv
import gleam/io
import gleam/int

pub fn main() {
  case argv.load().arguments {
    [num_nodes_str, topology, algorithm] -> {
      // avoid unused-variable warning by using an underscore if you don't need the integer yet
      let assert Ok(_num_nodes) = int.parse(num_nodes_str)

      io.println("Running project2...")
      io.println("Nodes: " <> num_nodes_str)
      io.println("Topology: " <> topology)
      io.println("Algorithm: " <> algorithm)

      // TODO: wire your protocol starter here:
      // case algorithm {
      //   "gossip" -> start_gossip(_num_nodes, topology)
      //   "push-sum" -> start_pushsum(_num_nodes, topology)
      //   _ -> io.println("Unknown algorithm. Use: gossip or push-sum")
      // }
    }
    _ -> {
      io.println("Usage: gleam run -- <numNodes> <topology> <algorithm>")
      io.println("Example: gleam run -- 100 full gossip")
    }
  }
}
