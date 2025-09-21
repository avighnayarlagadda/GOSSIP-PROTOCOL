// src/gossip.gleam
import gleam/erlang/process.{type Subject}
import node

pub fn launch(peers: List(Subject(node.ActorMsg))) -> Nil {
  case peers {
    [head, ..] -> {
      // Start gossip by sending to the first actor
      node.dispatch_spread(head)
    }
    [] -> Nil
  }
}
