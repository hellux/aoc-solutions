split_on(_, []) -> [];
split_on(Delim, [Delim | Chars]) -> split_on(Delim, Chars);
split_on(Delim, Chars) ->
    {X, Xs} = lists:splitwith(fun(C) -> C =/= Delim end, Chars),
    [X | split_on(Delim, Xs)].

get_input() ->
    lists:map(
        fun(Range) ->
            {A, [$- | B]} = lists:splitwith(fun(C) -> C =/= $- end, Range),
            {list_to_integer(A), list_to_integer(B)}
        end,
        split_on($,, lists:filter(fun(C) -> C =/= $\n end, io:get_chars("", 999999)))
    ).

all_ids(Ranges) -> lists:flatmap(fun({A, B}) -> lists:seq(A, B) end, Ranges).

p1(Ranges) ->
    InvalidIds = lists:filter(
        fun(Id) ->
            Str = integer_to_list(Id),
            {A, B} = lists:split(length(Str) div 2, Str),
            A =:= B
        end,
        all_ids(Ranges)
    ),
    lists:sum(InvalidIds).

main(_) ->
    Ranges = get_input(),
    io:format("~p~n", [p1(Ranges)]).
