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
        fun(Id) -> repeats_n_times(2, integer_to_list(Id)) end,
        all_ids(Ranges)
    ),
    lists:sum(InvalidIds).

% divide into lists of len N, e.g.
%   [[a, b, c], [d, e, f]] = split_all(3, [a, b, c, d, e, f])
split_all(_, []) -> [];
split_all(N, List) ->
    {X, Xs} = lists:split(N, List),
    [X | split_all(N, Xs)].

repeats_n_times(N, List) ->
    Len = length(List),
    if
        Len rem N =:= 0 ->
            Xs = split_all(length(List) div N, List),
            lists:all(fun(X) -> X =:= lists:nth(1, Xs) end, Xs);
        true -> false
    end.

p2(Ranges) ->
    InvalidIds = lists:filter(
        fun(Id) ->
            Str = integer_to_list(Id),
            lists:any(
                fun(N) -> repeats_n_times(N, Str) end,
                lists:seq(2, length(Str))
            )
        end,
        all_ids(Ranges)
    ),
    lists:sum(InvalidIds).

main(_) ->
    Ranges = get_input(),
    io:format("~p~n~p~n", [p1(Ranges), p2(Ranges)]).
