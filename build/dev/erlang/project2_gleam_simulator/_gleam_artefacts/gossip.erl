-module(gossip).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\gossip.gleam").
-export([launch/1]).

-file("src\\gossip.gleam", 5).
-spec launch(list(gleam@erlang@process:subject(node:actor_msg()))) -> nil.
launch(Peers) ->
    case Peers of
        [Head | _] ->
            node:dispatch_spread(Head);

        [] ->
            nil
    end.
