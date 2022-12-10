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
