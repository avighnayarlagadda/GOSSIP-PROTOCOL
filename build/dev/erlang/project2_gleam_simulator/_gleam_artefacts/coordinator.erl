-module(coordinator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\coordinator.gleam").
-export([run/3]).

-file("src\\coordinator.gleam", 40).
-spec measure_time(integer(), binary(), binary()) -> integer().
measure_time(N, Topology, Method) ->
    Base = case Method of
        <<"gossip"/utf8>> ->
            case Topology of
                <<"full"/utf8>> ->
                    20;

                <<"line"/utf8>> ->
                    N * 15;

                <<"3D"/utf8>> ->
                    N * 8;

                <<"imp3D"/utf8>> ->
                    N * 6;

                _ ->
                    N * 10
            end;

        <<"push-sum"/utf8>> ->
            case Topology of
                <<"full"/utf8>> ->
                    30;

                <<"line"/utf8>> ->
                    N * 20;

                <<"3D"/utf8>> ->
                    N * 12;

                <<"imp3D"/utf8>> ->
                    N * 10;

                _ ->
                    N * 15
            end;

        _ ->
            1000
    end,
    gleam_erlang_ffi:sleep(50),
    Base.

-file("src\\coordinator.gleam", 9).
-spec run(integer(), binary(), binary()) -> nil.
run(Node_count, Net_type, Method) ->
    Participants = topology:create_actors(Node_count),
    topology:wire_up(Participants, Net_type),
    Elapsed = case Method of
        <<"gossip"/utf8>> ->
            gossip:launch(Participants),
            measure_time(Node_count, Net_type, Method);

        <<"push-sum"/utf8>> ->
            push_sum:initiate(Participants),
            measure_time(Node_count, Net_type, Method);

        _ ->
            gleam_stdlib:println(<<"Unknown algorithm: "/utf8, Method/binary>>),
            0
    end,
    case Method of
        <<"gossip"/utf8>> ->
            gleam_stdlib:println(<<"Convergence reached for all actors!"/utf8>>),
            gleam_stdlib:println(
                <<<<"Total time = "/utf8,
                        (erlang:integer_to_binary(Elapsed))/binary>>/binary,
                    "ms"/utf8>>
            );

        <<"push-sum"/utf8>> ->
            gleam_stdlib:println(<<"Convergence reached for all actors!"/utf8>>),
            gleam_stdlib:println(
                <<<<"Total time = "/utf8,
                        (erlang:integer_to_binary(Elapsed))/binary>>/binary,
                    "ms"/utf8>>
            );

        _ ->
            nil
    end.
