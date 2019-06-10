module W1

open System
open System.IO
open Extensions
open PriorityQueue
open System.Collections.Generic

// +++++++++
// + Day 1 +
// +++++++++

module Day1 =
  let findDuplicate (xs : int []) =
    let rec go i freq set =
      if Array.length xs <= i then go 0 freq set
      else
        let freq2 = freq + xs.[i]

        if Set.contains freq2 set
          then freq2
          else go (i + 1) freq2 (Set.add freq2 set)

    go 0 0 (Set.singleton 0)

  let readAndRun () =
    let lines =
      File.ReadAllLines "input/day1.txt"
      |> Array.map Int32.Parse

    printfn "%A" (Array.sum lines)
    printfn "%A" (findDuplicate lines)

// +++++++++
// + Day 2 +
// +++++++++

module Day2 =
  let toInt =
    function
    | false -> 0
    | true  -> 1

  let count xs =
    let freqs =
      Array.groupBy id xs
      |> Array.map (snd >> Array.length)

    (Array.contains 2 freqs, Array.contains 3 freqs)
    |> (fun (hasTwo, hasThree) -> toInt hasTwo, toInt hasThree)

  let checkSum =
    Array.map count
    >> Array.fold (fun (a, b) (a2, b2) -> a + a2, b + b2) (0, 0)
    >> (fun (numTwos, numThrees) -> numTwos * numThrees)

  let differByOne cs cs2 =
    Array.map2 (<>) cs cs2
    |> Array.sumBy toInt
    |> ((=) 1)

  let common cs cs2 =
    Array.zip cs cs2
    |> Array.filter (fun (a, b) -> a = b)
    |> Array.map fst

  let findCommon (all : char [] []) =
    let picker cs =
      Array.tryFind (differByOne cs) all
      |> Option.map (fun cs2 -> common cs cs2)

    Array.tryPick picker all
    |> Option.map (fun cs -> new String(cs))

  let readAndRun () =
    let lines =
      File.ReadAllLines "input/day2.txt"
      |> Array.map Array.ofSeq

    printfn "%A" (checkSum lines)
    printfn "%A" (findCommon lines)

// +++++++++
// + Day 3 +
// +++++++++

module Day3 =
  type Rectangle = int * int * int * int * int

  let getId (id, _, _, _, _) = id

  let computeOverlappingCells (rects : Rectangle []) =
    let grid : byte [,] = Array2D.zeroCreate 1000 1000

    for _, x, y, w, h in rects do
      for i = 0 to w - 1 do
        for j = 0 to h - 1 do
          grid.[x + i, y + j] <- min 2uy (grid.[x + i, y + j] + 1uy)

    grid

  let findOverlappingArea : Rectangle [] -> int =
    computeOverlappingCells
    >> Array2D.sumBy (function 2uy -> 1 | _ -> 0)

  let isNotOverlapping (grid : byte [,]) (_, x, y, w, h) =
    [
      for i = 0 to w - 1 do
        for j = 0 to h - 1 do
          yield grid.[x + i, y + j]
    ]
    |> Seq.forall ((=) 1uy)

  let findIdOfNonOverlapping (rects : Rectangle []) =
    let grid = computeOverlappingCells rects

    Array.tryFind (isNotOverlapping grid) rects
    |> Option.map getId

  let parseRect (s : string) : Rectangle =
    let id, xy, wh =
      match s.Split () with
      | [|a; _; b; c|] -> a.[1..], b, c
      | _ -> failwith "Error1 @ Day3.parseRect"

    let x, y =
      match xy.Split [|','; ':'|] with
      | [|x; y; _|] -> x, y
      | _ -> failwith "Error2 @ Day3.parseRect"

    let w, h =
      match wh.Split 'x' with
      | [|w; h|] -> w, h
      | _ -> failwith "Error3 @ Day3.parseRect"

    Int32.Parse id, Int32.Parse x, Int32.Parse y,
    Int32.Parse w, Int32.Parse h

  let readAndRun () =
    let rects =
      File.ReadAllLines "input/day3.txt"
      |> Array.map parseRect

    printfn "%A" (findOverlappingArea rects)
    printfn "%A" (findIdOfNonOverlapping rects)

// +++++++++
// + Day 4 +
// +++++++++

module Day4 =
  type Id = int

  type Event =
    | WakesUp
    | FallsAsleep
    | BeginsShift of Id

  type Record =
    { time  : DateTime
    ; event : Event
    }

  module Record =
    let getTime {time = dt; event = _} = dt

  let parseRecord (s : string) =
    match s.Split [| '['; ']'|] with
    | [|_; DateTime dt; event|] ->
        match event.Split [|' '; '#'|] with
        | [|_; "wakes"; "up"|]     -> {time = dt; event = WakesUp}
        | [|_; "falls"; "asleep"|] -> {time = dt; event = FallsAsleep}

        | [|_; "Guard"; _; Int id; "begins"; "shift"|] ->
            {time = dt; event = BeginsShift id}

        | ss -> failwithf "Error! Could not parse event in\n%A" ss

    | ss -> failwithf "Error! Could not parse timestamp in\n%A" ss

  let readAndRun () =
    let records =
      File.ReadAllLines "input/day4.txt"
      |> Array.map parseRecord

    records.[.. 25]
    |> Array.sortBy Record.getTime
    |> Array.map (fun x -> x.time.ToString "yyyy-MM-dd HH:mm")
    |> Array.iter (printfn "%A")

// +++++++++
// + Day 6 +
// +++++++++

module Day6 =
  type Point = (int * int)

  module Point =
    let getX (x, _) = x

    let getY (_, y) = y

  let minMaxBy f =
    let g (minX, maxX) x =
      let x2 = f x
      min minX x2, max maxX x2

    Array.fold g (Int32.MaxValue, Int32.MinValue)

  let canonicalize points =
    let minX, maxX = minMaxBy Point.getX points
    let minY, maxY = minMaxBy Point.getY points

    maxX, maxY, Array.map (fun (x, y) -> x - minX, y - minY) points

  let neighbors (x, y) =
    [ x - 1, y
    ; x    , y - 1
    ; x + 1, y
    ; x    , y + 1
    ]

  let inBounds n m (x, y) =
    0 <= x && x < n && 0 <= y && y < m

  let distance (x, y) (x2, y2) = abs (x - x2) + abs (y - y2)

  let tryFindClosestPoint points p =
    let p2 = Array.minBy (distance p) points
    let d  = distance p p2
    Array.tryFind (fun p3 -> p2 <> p3 && distance p p3 = d) points
    |> function
       | Some _ -> None
       | None   -> Some p2

  let findMaxAreaBy pred (n, m) points =
    let visited = Array2D.zeroCreate n m

    let rec visit pred =
      function
      | p    when not (inBounds n m p) -> None
      | x, y when visited.[x, y] -> Some 0
      | x, y as p ->
          let visitNeighbor acc p2 = Option.monad {
            let! area  = acc
            let! area2 = visit pred p2
            return area + area2
            }

          if not (pred p) then Some 0
          else
            visited.[x, y] <- true

            List.fold visitNeighbor (Some 1) (neighbors p)

    Array.map (fun p -> visit (pred p) p) points
    |> Array.choose id
    |> Array.max

  let findMaxAreaByClosestPoint points =
    let n, m, points = canonicalize points

    let pred p p2 =
      match tryFindClosestPoint points p2 with
      | Some p3 when p = p3 -> true
      | _                   -> false

    findMaxAreaBy pred (n, m) points

  let findMaxAreaByClosestPoint2 points =
    let n, m, points = canonicalize points
    let areas        = Array2D.zeroCreate n m

    for i = -1 to n do
      for j = -1 to m do
        match tryFindClosestPoint points (i, j) with
        | Some (i2, j2) when i = -1 || j = -1 || i = n || j = m ->
            areas.[i2, j2] <- Int32.MinValue

        | Some (i2, j2) ->
            areas.[i2, j2] <- areas.[i2, j2] + 1

        | None -> ()

    Array2D.max areas

  let findMaxAreaByCloseness points =
    let n, m, points = canonicalize points

    let pred _ p2 = Array.sumBy (distance p2) points < 10000

    findMaxAreaBy pred (n, m) points

  let parsePoint (s : string) =
    match s.Split [|','; ' '|] with
    | [|Int x; _; Int y|] -> x, y
    | ss -> failwithf "Error! Could not parse point in\n%A" ss

  let readAndRun () =
    let points =
      File.ReadAllLines "input/day6.txt"
      |> Array.map parsePoint

    points
    |> findMaxAreaByClosestPoint
    |> printfn "%A"

    points
    |> findMaxAreaByClosestPoint2
    |> printfn "%A"

    points
    |> findMaxAreaByCloseness
    |> printfn "%A"

// +++++++++
// + Day 7 +
// +++++++++

module Day7 =
  type Edge = char * char

  let parseEdge =
   Seq.filter Char.IsUpper
   >> Seq.toList
   >> function
      | [_; c; c2] -> c, c2
      | cs         -> failwithf "Error! Could not parse edge in \n%A" cs

  let buildGraph (edges : seq<Edge>) =
    let graph =
      Seq.collect (fun (a, b) -> [a, []; b, []]) edges
      |> Seq.map Map.singleton
      |> Seq.fold (Map.unionWith (@)) Map.empty

    Seq.fold (fun g (a, b) -> Map.adjust (List.cons b) a g) graph edges

  let findOrder (graph : Map<char, char list>) =
    // setup dependencies
    let deps = new Dictionary<char, int>()

    for node, edges in Map.toSeq graph do
      if not (deps.ContainsKey node) then deps.Add(node, 0)

      for node2 in edges do
        if not (deps.ContainsKey node2)
          then deps.Add(node2, 1)
          else deps.[node2] <- deps.[node2] + 1

    // algorithm
    let order = new List<char>(Map.count graph)

    let rec visit q =
      match PriorityQueue.tryPop q with
      | None -> ()
      | Some (node, q) ->
          order.Add node

          [
            for node2 in Map.find node graph do
              let n = deps.[node2] - 1
              deps.[node2] <- n
              if n <= 0 then yield node2
          ]
          |> fun xs -> PriorityQueue.pushMany xs q
          |> visit

    Seq.filter (fun (KeyValue (_, v)) -> v = 0) deps
    |> Seq.map (fun (KeyValue (k, _)) -> k)
    |> PriorityQueue.ofSeq
    |> visit

    order.ToArray ()

  module Task =
    let make c = c, 60 + (int c - 64)

    let tick (id, t) = id, t - 1

    let isCompleted (id, t) = t <= 0

  let computeCompletionTime totalWorkers (graph : Map<char, char list>) =

    // setup dependencies
    let deps = new Dictionary<char, int>()

    for u, edges in Map.toSeq graph do
      if not (deps.ContainsKey u) then deps.Add(u, 0)

      for v in edges do
        if not (deps.ContainsKey v)
          then deps.Add(v, 1)
          else deps.[v] <- deps.[v] + 1

    // algorithm
    let rec visit tasks workers time tasksLeft (pq : PriorityQueue<char>) =
      // return time taken if all tasks are done
      if tasksLeft <= 0 then time
      else
        let newTasks, pq =
          PriorityQueue.tryPopN workers pq
          |> fun (newTasks, pq) -> List.map Task.make newTasks, pq

        // update tasks and group them by state
        let completed, active =
          tasks @ newTasks
          |> List.map Task.tick
          |> List.partition Task.isCompleted

        // add nodes to pq if their dependencies are ready
        let pq =
          [
            for node, _ in completed do
              for node2 in Map.find node graph do
                let n = deps.[node2] - 1
                deps.[node2] <- n
                if n <= 0 then yield node2
          ]
          |> Seq.fold (fun pq node -> PriorityQueue.push node pq) pq

        visit
          active
          (totalWorkers - Seq.length active)
          (time + 1)
          (tasksLeft - Seq.length completed)
          pq

    Seq.filter (fun (KeyValue (_, v)) -> v = 0) deps
    |> Seq.map (fun (KeyValue (k, _)) -> k)
    |> PriorityQueue.ofSeq
    |> visit [] totalWorkers 0 (Map.count graph)

  // GKRVWBESYAMZDPTIUCFXQJLHNO
  // 903
  let readAndRun () =
    let graph =
      File.ReadAllLines "input/day7.txt"
      |> Array.map parseEdge
      |> buildGraph

    findOrder graph
    |> String.ofSeq
    |> printfn "%s"

    computeCompletionTime 5 graph
    |> printfn "%A"
