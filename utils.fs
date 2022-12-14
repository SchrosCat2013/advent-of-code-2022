module Utils

let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x ->
    match cache.TryGetValue x with
    | true, v -> v
    | false, _ -> let v = fn (x)
                  cache.Add(x,v)
                  v)

let (|StartsWith|_|) (pattern: string) (str: string) =
    if str.StartsWith pattern
    then Some <| str[pattern.Length..].Trim()
    else None

type Direction = Up | Down | Left | Right

type Point = (int * int)

let move direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let isOutOfBounds grid (x, y) =
    (x < 0) || (x >= Array2D.length2 grid)
    || (y < 0) || (y >= Array2D.length1 grid)

