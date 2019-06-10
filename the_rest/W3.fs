module W3

open System
open System.IO
open Extensions
open Microsoft.FSharp.Reflection

// ++++++++++
// + Day 16 +
// ++++++++++

module Day16 =
  type RegisterSet(a, b, c, d) =
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable d = d

    member this.Clone() =
      this.MemberwiseClone() :?> RegisterSet

    member this.Item
      with get r  =
        match r with
        | 0 -> a
        | 1 -> b
        | 2 -> c
        | 3 -> d
        | _ -> failwithf "Error @ RegisterSet.Item: %A" r

      and set r x =
        match r with
        | 0 -> a <- x
        | 1 -> b <- x
        | 2 -> c <- x
        | 3 -> d <- x
        | _ -> failwithf "Error @ RegisterSet.Item: %A" r

  type Op =
    | Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr

  module Op =
    let all =
      let instantiate info = FSharpValue.MakeUnion(info, [||]) :?> Op
      FSharpType.GetUnionCases typeof<Op>
      |> Array.map instantiate

  type Instruction =
    { op : Op
    ; l  : int
    ; r  : int
    ; d  : int
    }

  module Instruction =
    let make op l r d  = {op = op; l = l; r = r; d = d}

  let interpret (rset : RegisterSet) ins =
    let rset      = rset.Clone()
    let li        = ins.l
    let lr        = rset.[ins.l]
    let ri        = ins.r
    let rr        = rset.[ins.r]
    let asn x     = rset.[ins.d] <- x
    let (|>|) a b = if a > b then 1 else 0
    let (|=|) a b = if a > b then 1 else 0

    match ins.op with
    | Addr -> asn (lr  +  rr)
    | Addi -> asn (lr  +  ri)
    | Mulr -> asn (lr  *  rr)
    | Muli -> asn (lr  *  ri)
    | Banr -> asn (lr &&& rr)
    | Bani -> asn (lr &&& ri)
    | Borr -> asn (lr ||| rr)
    | Bori -> asn (lr ||| ri)

    | Gtir -> asn (li |>| rr)
    | Gtri -> asn (lr |>| ri)
    | Gtrr -> asn (lr |>| rr)
    | Eqir -> asn (li |=| rr)
    | Eqri -> asn (lr |=| ri)
    | Eqrr -> asn (lr |=| rr)

    | Setr -> asn lr
    | Seti -> asn li

    rset

  let interpretAs op rset (_, l, r, d) =
    Instruction.make op l r d
    |> interpret rset

  let isOp op (before, insTuple, after) =
    interpretAs op before insTuple = after

  module Parse =
    let registerSet (s : string) =
      s.Split [|'['; ']'; ','; ' '|]
      |> Array.filter (String.IsNullOrEmpty >> not)
      |> function
         | [|_; Int a; Int b; Int c; Int d|] -> RegisterSet(a, b, c, d)
         | ss  -> failwithf "Error @ Parse.registerSet: %A" ss

    let insTuple (s : string) =
      match s.Split ' ' with
      | [|Int opcode; Int l; Int r; Int d|] -> opcode, l, r, d
      | ss -> failwithf "Error @ Parse.insTuple: %A" ss

    let sample s s2 s3 =
      registerSet s, insTuple s2, registerSet s3

    let sampleMany ss =
      [
        for i = 0 to Array.length ss / 4 - 1 do
          let i = i * 4
          yield sample ss.[i] ss.[i + 1] ss.[i + 2]
      ]

  let readAndRun () =
    let samples =
      File.ReadAllLines "input/day161.txt"
      |> Parse.sampleMany

    Seq.take 4 samples
    |> Seq.iter (printfn "%A")

// ++++++++++
// + Day 17 +
// ++++++++++

module Day17 =
  type Line =
    | Vertical   of int * (int * int)
    | Horizontal of (int * int) * int

  module Line =
    let getXs =
      function
      | Vertical   (x, _)       -> [x]
      | Horizontal ((x, x2), _) -> [x; x2]

    let getYs =
      function
      | Horizontal (_, y)       -> [y]
      | Vertical   (_, (y, y2)) -> [y; y2]

    let parse (s : string) =
      s.Split [|','; ' '; '='; '.'|]
      |> Array.filter String.notNullOrEmpty
      |> function
         | [|"x"; Int x; "y"; Int y; Int y2|] -> Vertical (x, (y, y2))
         | [|"y"; Int y; "x"; Int x; Int x2|] -> Horizontal ((x, x2), y)
         | ss -> failwithf "Error @ parseLine: %A" ss

  let buildMap lines =
    let maxX =
      Seq.collect Line.getXs lines
      |> Seq.max

    let maxY =
      Seq.collect Line.getYs lines
      |> Seq.max

    let n   = maxY + 1
    let m   = maxX + 2
    let map = Array2D.init n m (fun _ _ -> '.')

    for l in lines do
      match l with
      | Vertical (x, (y, y2)) ->
          for i = y to y2 do
            map.[i, x] <- '#'

      | Horizontal ((x, x2), y) ->
          for j = x to x2 do
            map.[y, j] <- '#'

    map

  type WaterState =
    | Still
    | Flowing

  module WaterState =
    let product st st2 =
      match st, st2 with
      | Still, Still -> Still
      | _            -> Flowing

  let countReachableTiles map start =
    let rec flow ((i, j) as pos) =
      if not (Array2D.inBounds map pos) then (Flowing, (j, j), 0)
      else
        match map.[i, j] with
        | '#' -> Still,   (j, j), 0
        | '~' -> Still,   (j, j), 0
        | '*' -> Still,   (j, j), 0
        | '|' -> Flowing, (j, j), 0
        | '.' ->
            map.[i, j] <- '*'

            match flow (i + 1, j) with
            | Flowing, (lo, hi), x ->
                for j = lo + 1 to hi - 1 do
                  map.[i + 1, j] <- '|'

                Flowing, (j - 1, j + 1), x + 1

            | Still, (lo, hi), x ->
                for j = lo + 1 to hi - 1 do
                  map.[i + 1, j] <- '~'

                let (st , (lo2, _  ), y) = flow (i, j - 1)
                let (st2, (_  , hi3), z) = flow (i, j + 1)

                WaterState.product st st2, (lo2, hi3), x + y + z + 1

        | c -> failwithf "Error @ countReachableTiles: %A" c

    let _, _, count = flow start
    count

  let writeMapToFile path map =
    let rows =
      Array2D.rows map
      |> Seq.map String.ofArray

    let start = seq ["<pre>"]
    let stop  = seq ["</pre>"]

    (path, Seq.concat [start; rows; stop])
    |> File.WriteAllLines

  let readAndRun () =
    let lines =
      File.ReadAllLines "input/day17.txt"
      |> Seq.map Line.parse

    let map = buildMap lines

    // This one gives 32441, but it should be 32439 - off by 2.
    printfn "%A" (countReachableTiles map (1, 500))

    printfn "%A" (Array2D.sumBy (function '~' -> 1 | _ -> 0) map)

    writeMapToFile "output/day17.html" map
