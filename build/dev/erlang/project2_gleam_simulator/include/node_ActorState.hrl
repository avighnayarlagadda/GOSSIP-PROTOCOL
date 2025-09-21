-record(actor_state, {
    ident :: integer(),
    links :: list(gleam@erlang@process:subject(node:actor_msg())),
    spread_count :: integer(),
    val_s :: float(),
    val_w :: float(),
    stopped :: boolean(),
    prev_ratios :: list(float())
}).
