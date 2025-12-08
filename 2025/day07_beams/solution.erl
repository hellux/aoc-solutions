get_lines() ->
    case io:get_line("") of
        eof -> [];
        Line -> [string:trim(Line) | get_lines()]
    end.
get_input() -> list_to_tuple(lists:map(fun list_to_tuple/1, get_lines())).

positions(V, Xs) -> positions(V, Xs, 1).
positions(_, [], _) -> [];
positions(V, [V | Xs], I) -> [I | positions(V, Xs, I+1)];
positions(V, [_ | Xs], I) -> positions(V, Xs, I+1).

reaches_start(_, {Y, X}) when Y < 1, X < 1 -> false;
reaches_start(M, {1, X}) -> element(X, element(1, M)) =:= $S;
reaches_start(M, {Y, X}) ->
    W = tuple_size(element(1, M)),
    SplitFromLeft = X > 2 andalso element(X-1, element(Y, M)) =:= $^,
    SplitFromRight = X < W andalso element(X+1, element(Y, M)) =:= $^,
    FreeAbove = element(X, element(Y-1, M)) =/= $^,
    (SplitFromLeft andalso reaches_start(M, {Y-1, X-1})) orelse
    (SplitFromRight andalso reaches_start(M, {Y-1, X+1})) orelse
    (FreeAbove andalso reaches_start(M, {Y-1, X})).

p1(M) ->
    Splitters = lists:flatmap(
        fun({Y, Row}) ->
            lists:map(fun(X) -> {Y, X} end, positions($^, tuple_to_list(Row)))
        end,
        lists:enumerate(tuple_to_list(M))
    ),
    length(lists:filter(fun(S) -> reaches_start(M, S) end, Splitters)).

main(_) ->
    Manifold = get_input(),
    io:format("~p~n", [p1(Manifold)]).
