let rec inf x = inf x in
    let fst = fun t -> match t with (x, y) -> x in
    let snd = fun t -> match t with (x, y) -> y in
    (fst (3, inf 3), snd (inf 3, 3))
