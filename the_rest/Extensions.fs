module Extensions

open System
open System.Collections.Generic

let (|Int|_|) (s : string) =
    match Int32.TryParse s with
    | true, x -> Some x
    | _       -> None

let (|DateTime|_|) (s : string) =
    match DateTime.TryParse s with
    | true, dt -> Some(dt)
    | _        -> None

module Array =
  let removeAtMany idcs xs =
    [|
      for i = 0 to Array.length xs - 1 do
        if not (Seq.contains i idcs) then yield xs.[i]
    |]

module Array2D =
  let sumBy f xss =
    let mutable n = 0
    Array2D.iter (fun x -> n <- n + f x) xss
    n

  let max (xss : _ [,]) =
    let mutable x = xss.[0, 0]
    Array2D.iter (fun x2 -> x <- max x x2) xss
    x

  let rows (xss : _ [,]) =
    let n  = Array2D.length1 xss
    let m  = Array2D.length2 xss
    let rs = List<_>()

    for i = 0 to n - 1 do
      rs.Add (Array.zeroCreate m)
      for j = 0 to m - 1 do
        rs.[i].[j] <- xss.[i, j]

    seq rs

  let inBounds (xss : _ [,]) (i, j) =
    0 <= i && i < Array2D.length1 xss &&
    0 <= j && j < Array2D.length2 xss

module Option =
  type OptionBuilder() =

      member this.Bind(x, f) =
          match x with
          | None -> None
          | Some a -> f a

      member this.Return(x) =
          Some x

  let monad = new OptionBuilder()

  let ofResult =
    function
    | Ok x    -> Some x
    | Error _ -> None

module List =
  let cons x xs = x :: xs

module Map =
  let singleton (x, y) = Map.ofList [x, y]

  let adjust f k m =
    match Map.tryFind k m with
    | Some v -> Map.add k (f v) m
    | None   -> m

  // Union by key. It resolves collisions with onCollision.
  let unionWith onCollision m =
    let f acc (x, y) =
      match Map.tryFind x acc with
      | None   -> Map.add x y acc
      | Some z -> Map.add x (onCollision z y) acc

    Map.toSeq >> Seq.fold f m

module Pair =
  let lr x y = x, y

module String =
  let ofSeq cs = new string (Seq.toArray cs)

  let toArray : string -> char [] = Seq.toArray

  let ofArray (cs : char []) = new string(cs)

  let notNullOrEmpty s = String.IsNullOrEmpty s |> not

module Seq =
  let product xs ys = seq {
      for x in xs do
        for y in ys do
          yield x, y
    }

type LinkedList<'a> with
  member x.Rotate n =
    let go n (xs : LinkedList<_>) =
      if n = 0 then ()
      else if n < 0 then
        let n = (abs n) % xs.Count

        for i = 0 to n - 1 do
          let x = xs.First
          xs.RemoveFirst()
          xs.AddLast(x)

      else
        let n = n % xs.Count

        for i = 0 to n - 1 do
          let x = xs.Last
          xs.RemoveLast()
          xs.AddFirst(x)

    go n x
