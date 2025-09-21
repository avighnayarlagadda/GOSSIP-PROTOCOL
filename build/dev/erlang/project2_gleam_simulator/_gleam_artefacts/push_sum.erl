-module(push_sum).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\push_sum.gleam").
-export([initiate/1]).

-file("src\\push_sum.gleam", 5).
-spec initiate(list(gleam@erlang@process:subject(node:actor_msg()))) -> nil.
initiate(Actors) ->
    case Actors of
        [Primary | _] ->
            node:dispatch_weighted(Primary, +0.0, +0.0);

        [] ->
            nil
    end.
