-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\main.gleam").
-export([initiate/1, main/0, demo_line_pushsum/0, demo_large_3d_gossip/0, demo_full_pushsum/0]).

-file("src\\main.gleam", 42).
-spec initiate(list(binary())) -> nil.
initiate(Params) ->
    case Params of
        [Num_str, Net_shape, Method] ->
            case gleam_stdlib:parse_int(Num_str) of
                {ok, Total} ->
                    coordinator:run(Total, Net_shape, Method);

                {error, _} ->
                    gleam_stdlib:println(<<"Invalid participant count"/utf8>>)
            end;

        _ ->
            gleam_stdlib:println(
                <<"Usage: <count> <topology: full|line|3d|imp3d> <algorithm: gossip|push-sum>"/utf8>>
            )
    end.

-file("src\\main.gleam", 5).
-spec main() -> nil.
main() ->
    Node_count = 50,
    Net_shape = <<"imp3D"/utf8>>,
    Method = <<"gossip"/utf8>>,
    gleam_stdlib:println(<<"=== Gleam Gossip Protocol Simulator ==="/utf8>>),
    gleam_stdlib:println(
        <<<<<<<<<<<<"Running: "/utf8,
                                (erlang:integer_to_binary(Node_count))/binary>>/binary,
                            " participants, "/utf8>>/binary,
                        Net_shape/binary>>/binary,
                    " structure, "/utf8>>/binary,
                Method/binary>>/binary,
            " mode"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    initiate([erlang:integer_to_binary(Node_count), Net_shape, Method]),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"To explore other setups, change the values in main.gleam:"/utf8>>
    ),
    gleam_stdlib:println(<<"Supported topologies: full, line, 3D, imp3D"/utf8>>),
    gleam_stdlib:println(<<"Supported algorithms: gossip, push-sum"/utf8>>).

-file("src\\main.gleam", 30).
-spec demo_line_pushsum() -> nil.
demo_line_pushsum() ->
    initiate([<<"5"/utf8>>, <<"line"/utf8>>, <<"push-sum"/utf8>>]).

-file("src\\main.gleam", 34).
-spec demo_large_3d_gossip() -> nil.
demo_large_3d_gossip() ->
    initiate([<<"3000"/utf8>>, <<"3D"/utf8>>, <<"gossip"/utf8>>]).

-file("src\\main.gleam", 38).
-spec demo_full_pushsum() -> nil.
demo_full_pushsum() ->
    initiate([<<"5"/utf8>>, <<"full"/utf8>>, <<"push-sum"/utf8>>]).
