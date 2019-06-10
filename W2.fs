module W2

open System
open System.IO
open Extensions
open System.Text
open System.Collections.Generic

// +++++++++
// + Day 8 +
// +++++++++

module Day8 =
  type Tree =
    { subtrees : Tree list
    ; metadata : int []
    }

  let parseTree nums =
    // n = number of children, m = number of metadata entries
    let rec go =
      function
      | (n :: m :: xs) ->
          let goSubtree (acc, xs2) _ =
            let t, xs3 = go xs2
            t :: acc, xs3

          let ts, xs2 = List.fold goSubtree ([], xs) [1 .. n]
          let ms, xs3 = List.splitAt m xs2

          {subtrees = List.rev ts; metadata = List.toArray ms}, xs3

      | xs -> failwithf "Error! Could not parse tree in\n%A" xs

    fst (go nums)

  let rec sumMetadata t =
    List.sumBy sumMetadata t.subtrees + (Array.sum t.metadata)

  let rec value =
    function
    | {subtrees = []; metadata = ms} -> Array.sum ms
    | {subtrees = ts; metadata = ms} ->
        let n = List.length ts
        [
          for i in ms do
            if 1 <= i && i <= n then
              yield value ts.[i - 1]
        ]
        |> Seq.sum

  let readAndRun () =
    let tree =
      (File.ReadAllLines "input/day8.txt").[0].Split ' '
      |> Array.map Int32.Parse
      |> Array.toList
      |> parseTree

    printfn "%A" (sumMetadata tree)
    printfn "%A" (value tree)

// +++++++++
// + Day 9 +
// +++++++++

module Day9 =
  let parseGameConfig (s : string) =
    s.Split ' '
    |> Array.choose (function Int x -> Some x | _ -> None)
    |> function
       | [|players; marbles|] -> players, marbles
       | xs -> failwithf "Error! Could not parse game in\n%A" xs

  let isMultipleOf x y = 0 = (y % x)

  let playGame numPlayers lastMarble =
    let scores : int64 [] = Array.zeroCreate numPlayers
    let marbles           = new LinkedList<int>()

    marbles.AddFirst 0 |> ignore
    let mutable currPly = 0

    for i = 1 to lastMarble do
      if isMultipleOf 23 i then
        marbles.Rotate 7
        let m = marbles.Last.Value
        marbles.RemoveLast ()
        marbles.Rotate -1

        scores.[currPly] <- scores.[currPly] + int64 m + int64 i
      else
        marbles.Rotate -1
        marbles.AddLast i |> ignore

      currPly <- (currPly + 1) % numPlayers

    Array.max scores

  let readAndRun () =
    let numPlayers, lastMarble =
      (File.ReadAllLines "input/day9.txt").[0]
      |> parseGameConfig

    printfn "%A" (playGame numPlayers lastMarble)
    printfn "%A" (playGame numPlayers (lastMarble * 100))

// ++++++++++
// + Day 11 +
// ++++++++++

module Day11 =
  let hundredDigit x =
    if x < 100
      then None
      else Some ((x / 100) % 10)

  let computePowerLevel serial x y =
    let rackId = x + 10
    hundredDigit ((rackId * y + serial) * rackId)
    |> Option.map (fun z -> z - 5)
    |> Option.defaultValue -5

  let computeFuelCells n serial =
    Array2D.init n n
      (fun x y -> computePowerLevel serial (x + 1) (y + 1))

  let findFuelCell serial =
    let cells = computeFuelCells 300 serial

    seq {
      for i = 0 to Array2D.length1 cells - 3 do
        for j = 0 to Array2D.length2 cells - 3 do
          yield Seq.product [0 .. 2] [0 .. 2]
                |> Seq.sumBy (fun (di, dj) -> cells.[i + di, j + dj])
                |> (fun total -> (i + 1, j + 1), total)
    }
    |> Seq.maxBy snd

  let findMaxDimStartingAt (cells : _ [,]) x y =
    let max (k, m) n =
      Seq.concat
        [ [ for i = x + n downto x do yield cells.[i, n] ]
        ; [ for i = y + n downto y do yield cells.[n, i] ]
        ] |> Seq.sum
          |> function
             | k2 when k < k + k2 -> k + k2, n
             | _                  -> k     , m

    [1 .. min (Array2D.length1 cells - x - 1) (Array2D.length2 cells - y - 1)]
    |> Seq.fold max (Int32.MinValue, 0)

  let findFuelCellAndDim serial =
    let n     = 301
    let cells = Array2D.zeroCreate n n

    for i = 1 to n - 1 do
      for j = 1 to n - 1 do
        cells.[i, j] <- computePowerLevel serial i j

    for i = 1 to n - 1 do
      for j = 1 to n - 1 do
        cells.[i, j] <- cells.[i, j]
                      + cells.[i - 1, j]
                      + cells.[i    , j - 1]
                      - cells.[i - 1, j - 1]

    let mutable bestV = Int32.MinValue
    let mutable bestX = Int32.MinValue
    let mutable bestY = Int32.MinValue
    let mutable bestD = Int32.MinValue

    for d = 1 to n - 1 do
      for i = d to n - 1 do
        for j = d to n - 1 do
          let v = cells.[i, j] - cells.[i - d, j]
                               - cells.[i    , j - d]
                               + cells.[i - d, j - d]

          if bestV < v then
            bestV <- v
            bestX <- i - d + 1
            bestY <- j - d + 1
            bestD <- d

    (bestX, bestY), bestD

  let readAndRun () =
    let serial = 9424

    printfn "%A" (findFuelCell serial)
    printfn "%A" (findFuelCellAndDim serial)

// ++++++++++
// + Day 13 +
// ++++++++++

module Day13 =
  type Direction =
    | Up
    | Down
    | Left
    | Right

  type Cart =
    { x     : int
    ; y     : int
    ; dir   : Direction
    ; moves : Direction option list
    }

  module Direction =
    let ofChar =
      function
      | '^' -> Some Up
      | '>' -> Some Right
      | 'v' -> Some Down
      | '<' -> Some Left
      | _   -> None

    let toChar =
      function
      | Up    -> '^'
      | Right -> '>'
      | Down  -> 'v'
      | Left  -> '<'

    let turnLeft =
      function
      | Right -> Up
      | Up    -> Left
      | Left  -> Down
      | Down  -> Right

    let turnRight =
      function
      | Left  -> Up
      | Right -> Down
      | Down  -> Left
      | Up    -> Right

  module Cart =
    let make x y d =
      { x = x
      ; y = y
      ; dir = d
      ; moves = [Some Left; None; Some Right]
      }

    let sameXY cart cart2 = cart.x = cart2.x && cart.y = cart2.y

    let getXY cart = cart.x, cart.y

    let getY = getXY >> snd

    let getX = getXY >> fst

    let sortMany carts =
      Seq.sortBy getX carts
      |> Seq.sortBy getY

    let sortManyInPlace carts =
      Array.sortInPlaceBy getX carts
      Array.sortInPlaceBy getY carts

    let getNextMove =
      function
      | [m; m2; m3] -> m, [m2; m3; m]
      | ms -> failwithf "Error @ getNextMove: %A" ms

    let move cart =
      let x, y = getXY cart
      match cart.dir with
      | Up    -> {cart with x = x    ; y = y - 1}
      | Right -> {cart with x = x + 1; y = y    }
      | Down  -> {cart with x = x    ; y = y + 1}
      | Left  -> {cart with x = x - 1; y = y    }

    let tick c cart =
      let d  = cart.dir
      let ms = cart.moves

      match c with
      | '+' ->
          match getNextMove ms with
          | None   , ms2 -> move {cart with moves = ms2}
          | Some d2, ms2 ->
              let d3 =
                match d2 with
                | Left  -> Direction.turnLeft d
                | Right -> Direction.turnRight d
                | d3    -> failwithf "Error @ Cart.tick: %A" d3

              move {cart with moves = ms2; dir = d3}

      | '/' ->
          match d with
          | Up    -> move {cart with dir = Right}
          | Down  -> move {cart with dir = Left}
          | Left  -> move {cart with dir = Down}
          | Right -> move {cart with dir = Up }

      | '\\' ->
          match d with
          | Down  -> move {cart with dir = Right}
          | Left  -> move {cart with dir = Up}
          | Up    -> move {cart with dir = Left}
          | Right -> move {cart with dir = Down}

      | _ -> move cart

  let parseRowAndCarts y s =
    let cs = String.toArray s

    [
      for x = 0 to Array.length cs - 1 do
        match Direction.ofChar cs.[x] with
        | None   -> ()
        | Some d ->
            match d with
            | Up    -> cs.[x] <- '|'
            | Left  -> cs.[x] <- '-'
            | Down  -> cs.[x] <- '|'
            | Right -> cs.[x] <- '-'

            yield Cart.make x y d
    ]
    |> Pair.lr cs

  let parseGridAndCarts lines =
    let lines = Seq.toArray lines
    let grid = List<char []>()

    [
      for y = 0 to Array.length lines - 1 do
        let cs, carts = parseRowAndCarts y lines.[y]
        grid.Add cs
        yield carts
    ]
    |> List.concat
    |> Pair.lr (array2D grid)

  let tryFindCollision cart carts =
    let xy = Cart.getXY cart
    [
      for i = 0 to Array.length carts - 1 do
        if xy = Cart.getXY carts.[i]
          then yield Some i
          else yield None
    ]
    |> Seq.tryPick id

  let prettyGrid grid carts collisions =
    let grid = Array2D.copy grid

    for cart in carts do
      let x, y = Cart.getXY cart
      grid.[y, x] <- Direction.toChar cart.dir

    for x, y in collisions do
      grid.[y, x] <- 'X'

    let sb = StringBuilder()

    for r in Array2D.rows grid do
      sb.AppendLine (String.ofArray r) |> ignore

    sb.ToString()

  let write i grid carts collisions =
    prettyGrid grid carts collisions
    |> Pair.lr ("data/step" + string i + ".txt")
    |> File.WriteAllText

  type SimulationInfo =
    { collisions : (int * int) []
    ; finalPos   : int * int
    }

  let runSimulation (grid : char [,]) carts =
    let collisions = List<int * int>()

    let rec go step =
      function
      | [|cart|] -> Cart.getXY cart

      | carts ->
          Cart.sortManyInPlace carts

          let idcs =
            [
              for i = 0 to Array.length carts - 1 do
                let x, y  = Cart.getXY carts.[i]
                let cart2 = Cart.tick grid.[y, x] carts.[i]

                match tryFindCollision cart2 carts with
                | None   -> carts.[i] <- cart2
                | Some j ->
                    carts.[i] <- cart2
                    collisions.Add (Cart.getXY cart2)
                    yield [i; j]
            ]

          match idcs with
          | []  -> go (step + 1) carts
          | idcs ->
              Array.removeAtMany (Seq.concat idcs) carts
              |> go (step + 1)

    let finalPos = go 1 (Seq.toArray carts)
    {collisions = Seq.toArray collisions; finalPos = finalPos}

  let clearLog () =
    let di = DirectoryInfo("data")

    for file in di.EnumerateFiles() do
      file.Delete()

  let readAndRun () =
    let lines = File.ReadAllLines "day13.txt"

    let grid, carts = parseGridAndCarts lines

    let info = runSimulation grid carts

    printfn "collisions:\n%A" info.collisions
    printfn "finalPos:\n%A" info.finalPos

