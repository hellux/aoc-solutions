let rec read_pairs xs =
    try read_pairs ((Scanf.scanf "%d %d\n" (fun x y -> (x, y))) :: xs)
    with End_of_file -> xs
let (l, r) = read_pairs [] |> List.split
let (l, r) = (List.sort compare l, List.sort compare r)

let part1 = (List.combine l r)
    |> List.map (fun (a, b) -> abs (a - b))
    |> List.fold_left (+) 0
let part2 = l
    |> List.map (fun a -> a * (r |> List.filter (fun b -> b == a) |> List.length))
    |> List.fold_left (+) 0

let () = Printf.printf "%d\n%d\n" part1 part2
