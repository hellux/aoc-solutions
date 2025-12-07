get_lines() ->
    case io:get_line("") of
        eof -> [];
        Line -> [Line | get_lines()]
    end.

split_on(_, []) -> [];
split_on(Pred, Chars) ->
    case Pred(hd(Chars)) of
        true -> split_on(Pred, tl(Chars));
        false ->
            {X, Xs} = lists:splitwith(fun(C) -> not Pred(C) end, Chars),
            [X | split_on(Pred, Xs)]
    end.

transpose([]) -> [];
transpose([[] | _]) -> [];
transpose(Rows) ->
    Col = lists:map(fun([R | _]) -> R end, Rows),
    [ Col | transpose(lists:map(fun([_ | Rs]) -> Rs end, Rows))].

eval({"*", Nums}) -> lists:foldl(fun(X, Y) -> X * Y end, 1, Nums);
eval({"+", Nums}) -> lists:foldl(fun(X, Y) -> X + Y end, 0, Nums).

is_space(C) -> C =:= $ .

eval_sheet(OpsStr, Cols) ->
    Ops = split_on(fun is_space/1, string:trim(OpsStr)),
    lists:sum(lists:map(fun eval/1, lists:zip(Ops, Cols))).

p1(Lines) ->
    {RowsStrs, [OpsStr]} = lists:split(length(Lines) - 1, Lines),
    Cols = transpose(lists:map(
        fun(Row) ->
            lists:map(
                fun list_to_integer/1,
                split_on(fun is_space/1, string:trim(Row))
            )
        end,
        RowsStrs
    )),
    eval_sheet(OpsStr, Cols).

p2(Lines) ->
    {RowsStrs, [OpsStr]} = lists:split(length(Lines) - 1, Lines),
    Cols = lists:map(
        fun(Row) -> lists:map(fun(C) -> list_to_integer(string:trim(C)) end, Row) end,
        split_on(
            fun(Col) -> length(string:trim(Col)) =:= 0 end,
            transpose(RowsStrs)
        )
    ),
    eval_sheet(OpsStr, Cols).

main(_) ->
    Lines = get_lines(),
    io:format("~p~n~p~n", [p1(Lines), p2(Lines)]).
