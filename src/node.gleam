import gleam/erlang/process.{type Subject, send, new_subject}

pub type ActorMsg {
  AssignLinks(List(Subject(ActorMsg)))
  Spread
  WeightedSum(Float, Float)
  Inspect(Subject(StatusReply))
  Shutdown
}

pub type StatusReply {
  ActorStatus(Int, Float, Float, Bool)
}

pub type ActorState {
  ActorState(
    ident: Int,
    links: List(Subject(ActorMsg)),
    spread_count: Int,
    val_s: Float,
    val_w: Float,
    stopped: Bool,
    prev_ratios: List(Float)
  )
}

pub fn init_actor(_ident: Int) -> Subject(ActorMsg) {
  new_subject()
}

pub fn assign_links(actor: Subject(ActorMsg), links: List(Subject(ActorMsg))) -> Nil {
  send(actor, AssignLinks(links))
}

pub fn dispatch_spread(actor: Subject(ActorMsg)) -> Nil {
  send(actor, Spread)
}

pub fn dispatch_weighted(actor: Subject(ActorMsg), s_val: Float, w_val: Float) -> Nil {
  send(actor, WeightedSum(s_val, w_val))
}

pub fn inspect_status(actor: Subject(ActorMsg), reply_to: Subject(StatusReply)) -> Nil {
  send(actor, Inspect(reply_to))
}

pub fn shutdown_actor(actor: Subject(ActorMsg)) -> Nil {
  send(actor, Shutdown)
}
