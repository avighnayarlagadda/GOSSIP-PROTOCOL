-module(project2_gleam_simulator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\project2_gleam_simulator.gleam").
-export([main/0]).

-file("src\\project2_gleam_simulator.gleam", 5).
-spec main() -> nil.
main() ->
    case erlang:element(4, argv:load()) of
        [Num_nodes_str, Topology, Algorithm] ->
            case gleam_stdlib:parse_int(Num_nodes_str) of
                {ok, _} -> nil;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"project2_gleam_simulator"/utf8>>,
                                function => <<"main"/utf8>>,
                                line => 9,
                                value => _assert_fail,
                                start => 241,
                                'end' => 293,
                                pattern_start => 252,
                                pattern_end => 266})
            end,
            gleam_stdlib:println(<<"Running project2..."/utf8>>),
            gleam_stdlib:println(<<"Nodes: "/utf8, Num_nodes_str/binary>>),
            gleam_stdlib:println(<<"Topology: "/utf8, Topology/binary>>),
            gleam_stdlib:println(<<"Algorithm: "/utf8, Algorithm/binary>>);

        _ ->
            gleam_stdlib:println(
                <<"Usage: gleam run -- <numNodes> <topology> <algorithm>"/utf8>>
            ),
            gleam_stdlib:println(
                <<"Example: gleam run -- 100 full gossip"/utf8>>
            )
    end.
