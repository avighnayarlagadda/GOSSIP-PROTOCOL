-module(entry).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\entry.gleam").
-export([main/0]).

-file("src\\entry.gleam", 4).
-spec main() -> nil.
main() ->
    Default_nodes = 50,
    Default_topology = <<"imp3D"/utf8>>,
    Default_algorithm = <<"gossip"/utf8>>,
    coordinator:run(Default_nodes, Default_topology, Default_algorithm).
