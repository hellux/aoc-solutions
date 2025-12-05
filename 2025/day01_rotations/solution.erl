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

main(_) ->
    Rotations = get_input(),
    io:format("~p~n", [p1(Rotations)]).
