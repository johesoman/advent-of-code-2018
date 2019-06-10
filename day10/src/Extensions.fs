module Extensions



open Combinators



module Pair =
  let rightLeft x y = y, x



  let leftRight x y = x, y



  let map f (x, y) = f x, f y



  let mapLeft f (x, y) = f x, y



  let mapRight f (x, y) = x, f y



module String =



  let toLines (s : string) : string [] = s.Split('\n')



  let ofCharArray (xs : char []) = new string(xs)



  let toCharArray (s: string) = s.ToCharArray()



module Map =
  let singleton (k : 'Key) (v : 'Value) =
    Map.add k v Map.empty



  // combines two maps, resolving collisions with onCollision
  let unionWith onCollision m =
    let f acc (x, y) =
      match Map.tryFind x acc with
      | None   -> Map.add x y acc
      | Some z -> Map.add x (onCollision z y) acc

    Map.toSeq >> Seq.fold f m



  let union (m1 : Map<_, _>) (m2 : Map<_, _>) =
    unionWith giveLeft m1 m2



  let mapValues f = Map.map (fun k v -> f v)



  let findWithDefault k v1 m =
    match Map.tryFind k m with
    | Some v2 -> v2
    | None    -> v1



module Seq =
  // returns the first duplicate
  // let tryFindDuplicate (xs : _ seq) =
  //     let s = System.Collections.Generic.HashSet(HashIdentity.Structural)
  //     let e = xs.GetEnumerator()
  //     let mutable found  = false
  //     let mutable result = None

  //     while e.MoveNext() && not found do
  //       let x = e.Current
  //       if s.Contains x
  //         then
  //           result <- Some x
  //           found  <- true
  //         else
  //           ignore (s.Add x)

  //     result



  // returns the first x in xs not present in ys
  let tryFindMissing (xs : _ seq) (ys : _ seq) =
    let s = Seq.fold (fun acc k -> Set.add k acc) Set.empty ys
    let e = xs.GetEnumerator()
    let mutable found = false
    let mutable ret   = None

    while e.MoveNext() && not found do
      let x = e.Current
      if Set.contains x s
        then ()
        else
          ret   <- Some x
          found <- true

    ret



module Array =
  let initial (xs : _ []) =
    match Array.length xs with
    | n when n < 2 -> [||]
    | n            -> xs.[.. n - 2]



  let tail =
    function
    | [||] -> [||]
    | xs   -> xs.[1 ..]



  let removeBy f = Array.filter (f >> not)


  let collectOption f = Array.collect (f >> Option.defaultValue [||])


  let toArrayOption =
    function
    | [||] -> None
    | xs   -> Some xs



  let ofArrayOption =
    function
    | Some xs -> xs
    | None    -> [||]



  let mapOption f =
     Array.collect (f >> Option.toArray)
     >> toArrayOption



  let foldOption f init xs =
    let         n    = Array.length xs
    let mutable i    = 0
    let mutable ret  = Some init
    let mutable loop = true

    while i < n && loop do
      match ret with
      | Some acc -> ret  <- f acc xs.[i]; i <- i + 1
      | None     -> loop <- false

    ret



  let sequenceOption (xs : _ option []) : _ [] option =
    mapOption id xs



  let intersperse sep xs =
    match Array.length xs with
    | n when n < 2 -> xs

    | n ->
        let ys : _ [] = Array.zeroCreate (n + n - 1)

        ys.[0] <- xs.[0]

        let mutable i = 1
        let mutable j = 1

        while i < n do
          ys.[j]     <- sep
          ys.[j + 1] <- xs.[i]

          i <- i + 1
          j <- j + 2

        ys



  let first xs = Array.get xs 0



  let cons x xs = Array.append [|x|] xs



  let tryUnCons =
    function
    | [| |] -> None
    | [|x|] -> Some (x, [||])
    | xs    -> Some ( xs.[0]
                    , xs.[1 ..]
                    )



  let snoc xs x = Array.append xs [|x|]



  let tryUnSnoc =
    function
    | [| |] -> None
    | [|x|] -> Some ([||], x)
    | xs    -> Some ( xs.[.. Array.length xs - 2]
                    , Array.last xs
                    )



  let intercalate sep = intersperse sep >> Array.concat


