-module(node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\node.gleam").
-export([init_actor/1, assign_links/2, dispatch_spread/1, dispatch_weighted/3, inspect_status/2, shutdown_actor/1]).
-export_type([actor_msg/0, status_reply/0, actor_state/0]).

-type actor_msg() :: {assign_links,
        list(gleam@erlang@process:subject(actor_msg()))} |
    spread |
    {weighted_sum, float(), float()} |
    {inspect, gleam@erlang@process:subject(status_reply())} |
    shutdown.

-type status_reply() :: {actor_status, integer(), float(), float(), boolean()}.

-type actor_state() :: {actor_state,
        integer(),
        list(gleam@erlang@process:subject(actor_msg())),
        integer(),
        float(),
        float(),
        boolean(),
        list(float())}.

-file("src\\node.gleam", 27).
-spec init_actor(integer()) -> gleam@erlang@process:subject(actor_msg()).
init_actor(_) ->
    gleam@erlang@process:new_subject().

-file("src\\node.gleam", 31).
-spec assign_links(
    gleam@erlang@process:subject(actor_msg()),
    list(gleam@erlang@process:subject(actor_msg()))
) -> nil.
assign_links(Actor, Links) ->
    gleam@erlang@process:send(Actor, {assign_links, Links}).

-file("src\\node.gleam", 35).
-spec dispatch_spread(gleam@erlang@process:subject(actor_msg())) -> nil.
dispatch_spread(Actor) ->
    gleam@erlang@process:send(Actor, spread).

-file("src\\node.gleam", 39).
-spec dispatch_weighted(
    gleam@erlang@process:subject(actor_msg()),
    float(),
    float()
) -> nil.
dispatch_weighted(Actor, S_val, W_val) ->
    gleam@erlang@process:send(Actor, {weighted_sum, S_val, W_val}).

-file("src\\node.gleam", 43).
-spec inspect_status(
    gleam@erlang@process:subject(actor_msg()),
    gleam@erlang@process:subject(status_reply())
) -> nil.
inspect_status(Actor, Reply_to) ->
    gleam@erlang@process:send(Actor, {inspect, Reply_to}).

-file("src\\node.gleam", 47).
-spec shutdown_actor(gleam@erlang@process:subject(actor_msg())) -> nil.
shutdown_actor(Actor) ->
    gleam@erlang@process:send(Actor, shutdown).
