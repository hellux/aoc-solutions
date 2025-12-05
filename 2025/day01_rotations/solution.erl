get_input() ->
    case io:get_line("") of
        eof -> [];
        Line -> [
            case string:trim(Line) of
                [$L | V] -> -list_to_integer(V);
                [$R | V] -> list_to_integer(V)
            end
            | get_input()
        ]
    end.

p1z(_, []) -> 0;
p1z(Dial, [Rot | Ations]) ->
    DialNext = Dial + Rot,
    (if DialNext rem 100 =:= 0 -> 1; true -> 0 end) +
    p1z(DialNext, Ations).
p1(Rotations) -> p1z(50, Rotations).

p2z(_, []) -> 0;
p2z(Dial, [Rot | Ations]) ->
    DialNext = Dial + Rot,
    Step = if Dial < DialNext -> 1; true -> -1 end,
    AllDials = lists:seq(Dial+Step, DialNext, Step),
    length(lists:filter(fun(N) -> N rem 100 =:= 0 end, AllDials)) +
    p2z(DialNext rem 100, Ations).
p2(Rotations) -> p2z(50, Rotations).

main(_) ->
    Rotations = get_input(),
    io:format("~p~n", [p1(Rotations)]),
    io:format("~p~n", [p2(Rotations)]).
