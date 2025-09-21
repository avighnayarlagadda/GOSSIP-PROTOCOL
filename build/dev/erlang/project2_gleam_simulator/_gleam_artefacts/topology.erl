-module(topology).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\topology.gleam").
-export([create_actors/1, wire_up/2]).

-file("src\\topology.gleam", 8).
-spec create_actors(integer()) -> list(gleam@erlang@process:subject(node:actor_msg())).
create_actors(Count) ->
    _pipe = gleam@list:range(1, Count),
    gleam@list:map(_pipe, fun node:init_actor/1).

-file("src\\topology.gleam", 29).
-spec full_connect(list(gleam@erlang@process:subject(node:actor_msg()))) -> nil.
full_connect(Actors) ->
    gleam@list:each(
        Actors,
        fun(Current) ->
            Links = begin
                _pipe = Actors,
                gleam@list:filter(_pipe, fun(Other) -> Other /= Current end)
            end,
            node:assign_links(Current, Links)
        end
    ).

-file("src\\topology.gleam", 37).
-spec line_connect(list(gleam@erlang@process:subject(node:actor_msg()))) -> nil.
line_connect(Actors) ->
    Total = erlang:length(Actors),
    With_index = gleam@list:index_map(Actors, fun(A, I) -> {I, A} end),
    gleam@list:each(
        With_index,
        fun(Entry) ->
            {Idx, Current} = Entry,
            Links = case Idx of
                0 when Total > 1 ->
                    case gleam@list:drop(Actors, 1) of
                        [Next | _] ->
                            [Next];

                        [] ->
                            []
                    end;

                Idx@1 when (Idx@1 =:= (Total - 1)) andalso (Total > 1) ->
                    case begin
                        _pipe = gleam@list:drop(Actors, Total - 2),
                        gleam@list:take(_pipe, 1)
                    end of
                        [Prev] ->
                            [Prev];

                        _ ->
                            []
                    end;

                Idx@2 when (Idx@2 > 0) andalso (Idx@2 < (Total - 1)) ->
                    Prev@1 = case begin
                        _pipe@1 = gleam@list:drop(Actors, Idx@2 - 1),
                        gleam@list:take(_pipe@1, 1)
                    end of
                        [P] ->
                            [P];

                        _ ->
                            []
                    end,
                    Nxt = case begin
                        _pipe@2 = gleam@list:drop(Actors, Idx@2 + 1),
                        gleam@list:take(_pipe@2, 1)
                    end of
                        [N] ->
                            [N];

                        _ ->
                            []
                    end,
                    lists:append(Prev@1, Nxt);

                _ ->
                    []
            end,
            node:assign_links(Current, Links)
        end
    ).

-file("src\\topology.gleam", 108).
-spec cube_root(integer()) -> integer().
cube_root(N) ->
    case N of
        N@1 when N@1 =< 1 ->
            1;

        N@2 when N@2 =< 8 ->
            2;

        N@3 when N@3 =< 27 ->
            3;

        N@4 when N@4 =< 64 ->
            4;

        N@5 when N@5 =< 125 ->
            5;

        _ ->
            gleam@int:max(2, N div 10)
    end.

-file("src\\topology.gleam", 74).
-spec grid3d_connect(list(gleam@erlang@process:subject(node:actor_msg()))) -> nil.
grid3d_connect(Actors) ->
    Total = erlang:length(Actors),
    Dim = cube_root(Total),
    With_index = gleam@list:index_map(Actors, fun(A, I) -> {I, A} end),
    gleam@list:each(
        With_index,
        fun(Entry) ->
            {Idx, Current} = Entry,
            X = case Dim of
                0 -> 0;
                Gleam@denominator -> Idx rem Gleam@denominator
            end,
            Y = case Dim of
                0 -> 0;
                Gleam@denominator@2 -> (case Dim of
                    0 -> 0;
                    Gleam@denominator@1 -> Idx div Gleam@denominator@1
                end) rem Gleam@denominator@2
            end,
            Z = case (Dim * Dim) of
                0 -> 0;
                Gleam@denominator@3 -> Idx div Gleam@denominator@3
            end,
            Links = gleam@list:filter_map(
                With_index,
                fun(Other) ->
                    {J, Candidate} = Other,
                    Ox = case Dim of
                        0 -> 0;
                        Gleam@denominator@4 -> J rem Gleam@denominator@4
                    end,
                    Oy = case Dim of
                        0 -> 0;
                        Gleam@denominator@6 -> (case Dim of
                            0 -> 0;
                            Gleam@denominator@5 -> J div Gleam@denominator@5
                        end) rem Gleam@denominator@6
                    end,
                    Oz = case (Dim * Dim) of
                        0 -> 0;
                        Gleam@denominator@7 -> J div Gleam@denominator@7
                    end,
                    Xd = gleam@int:absolute_value(X - Ox),
                    Yd = gleam@int:absolute_value(Y - Oy),
                    Zd = gleam@int:absolute_value(Z - Oz),
                    case ((Xd + Yd) + Zd) =:= 1 of
                        true ->
                            {ok, Candidate};

                        false ->
                            {error, nil}
                    end
                end
            ),
            node:assign_links(Current, Links)
        end
    ).

-file("src\\topology.gleam", 160).
-spec pick_extra_neighbor(
    list({integer(), gleam@erlang@process:subject(node:actor_msg())}),
    integer(),
    list(gleam@erlang@process:subject(node:actor_msg()))
) -> {ok, gleam@erlang@process:subject(node:actor_msg())} | {error, nil}.
pick_extra_neighbor(All, Idx, Existing) ->
    Available = gleam@list:filter_map(
        All,
        fun(Entry) ->
            {J, Candidate} = Entry,
            Is_self = J =:= Idx,
            Is_neighbor = gleam@list:contains(Existing, Candidate),
            case Is_self orelse Is_neighbor of
                true ->
                    {error, nil};

                false ->
                    {ok, Candidate}
            end
        end
    ),
    case Available of
        [First | _] ->
            {ok, First};

        [] ->
            {error, nil}
    end.

-file("src\\topology.gleam", 120).
-spec imp3d_connect(list(gleam@erlang@process:subject(node:actor_msg()))) -> nil.
imp3d_connect(Actors) ->
    Total = erlang:length(Actors),
    Dim = cube_root(Total),
    With_index = gleam@list:index_map(Actors, fun(A, I) -> {I, A} end),
    gleam@list:each(
        With_index,
        fun(Entry) ->
            {Idx, Current} = Entry,
            X = case Dim of
                0 -> 0;
                Gleam@denominator -> Idx rem Gleam@denominator
            end,
            Y = case Dim of
                0 -> 0;
                Gleam@denominator@2 -> (case Dim of
                    0 -> 0;
                    Gleam@denominator@1 -> Idx div Gleam@denominator@1
                end) rem Gleam@denominator@2
            end,
            Z = case (Dim * Dim) of
                0 -> 0;
                Gleam@denominator@3 -> Idx div Gleam@denominator@3
            end,
            Base_links = gleam@list:filter_map(
                With_index,
                fun(Other) ->
                    {J, Candidate} = Other,
                    Ox = case Dim of
                        0 -> 0;
                        Gleam@denominator@4 -> J rem Gleam@denominator@4
                    end,
                    Oy = case Dim of
                        0 -> 0;
                        Gleam@denominator@6 -> (case Dim of
                            0 -> 0;
                            Gleam@denominator@5 -> J div Gleam@denominator@5
                        end) rem Gleam@denominator@6
                    end,
                    Oz = case (Dim * Dim) of
                        0 -> 0;
                        Gleam@denominator@7 -> J div Gleam@denominator@7
                    end,
                    Xd = gleam@int:absolute_value(X - Ox),
                    Yd = gleam@int:absolute_value(Y - Oy),
                    Zd = gleam@int:absolute_value(Z - Oz),
                    case ((Xd + Yd) + Zd) =:= 1 of
                        true ->
                            {ok, Candidate};

                        false ->
                            {error, nil}
                    end
                end
            ),
            Extra = pick_extra_neighbor(With_index, Idx, Base_links),
            Final_links = case Extra of
                {ok, Extra_link} ->
                    [Extra_link | Base_links];

                {error, _} ->
                    Base_links
            end,
            node:assign_links(Current, Final_links)
        end
    ).

-file("src\\topology.gleam", 14).
-spec wire_up(list(gleam@erlang@process:subject(node:actor_msg())), binary()) -> nil.
wire_up(Actors, Shape) ->
    case Shape of
        <<"full"/utf8>> ->
            full_connect(Actors);

        <<"line"/utf8>> ->
            line_connect(Actors);

        <<"3D"/utf8>> ->
            grid3d_connect(Actors);

        <<"imp3D"/utf8>> ->
            imp3d_connect(Actors);

        _ ->
            gleam_stdlib:println(<<"Unknown shape: "/utf8, Shape/binary>>),
            gleam_stdlib:println(
                <<"Valid options: full, line, 3D, imp3D"/utf8>>
            )
    end,
    gleam_stdlib:println(<<"Topology construction finished!"/utf8>>).
