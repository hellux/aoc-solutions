let numbers = In_channel.input_all In_channel.stdin
    |> String.split_on_char '\n'
    |> List.filter (fun l -> (String.length l) > 0)
    |> List.map (fun l -> List.map int_of_string (String.split_on_char ' ' l))

let init l = List.rev (List.tl (List.rev l))

let inc_or_dec l =
    let inc = (List.nth l 0) <= (List.nth l 1) in
    let f a b = (a <= b) == inc in
    List.for_all2 f (init l) (List.tl l)
let small_diff l =
    let f a b = let d = abs (a - b) in 1 <= d && d <= 3 in
    List.for_all2 f (init l) (List.tl l)
let safe l = (inc_or_dec l) && (small_diff l)

let part1 = List.length (List.filter safe numbers)

let damp_safe l = let f i = List.filteri (fun j x -> j != i) l |> safe in
    List.init (List.length l) f
    |> List.exists (fun x -> x)
let part2 = List.length (List.filter damp_safe numbers)

let () = Printf.printf "%d\n" part1
let () = Printf.printf "%d\n" part2
