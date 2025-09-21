// src/push_sum.gleam
import gleam/erlang/process.{type Subject}
import node

pub fn initiate(actors: List(Subject(node.ActorMsg))) -> Nil {
  case actors {
    [primary, ..] -> {
      // Start push-sum by sending initial values to the primary actor
      node.dispatch_weighted(primary, 0.0, 0.0)
    }
    [] -> Nil
  }
}
