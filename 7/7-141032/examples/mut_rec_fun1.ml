let rec f1 n = if n <= 1 then 1 else f2 (n + 2) and f2 n = (f1 (n - 3)) + 4 in f1 5
