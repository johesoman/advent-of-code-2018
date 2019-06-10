module PriorityQueue

type PriorityQueue<'a when 'a : comparison> =
  { nextId: uint64
  ; elements: Set<('a * uint64)>
  }

let empty = { nextId = 0uL; elements = Set.empty }

let push x { nextId = id; elements = set} =
  { nextId = id + 1uL
  ; elements = Set.add (x, id) set
  }

let pushMany xs q = Seq.fold (fun q x -> push x q) q xs

let ofSeq xs = Seq.fold (fun pq x -> push x pq) empty xs

let tryPop pq =
  if Set.isEmpty pq.elements then None
  else
    let (e, id) = Set.minElement pq.elements
    Some (e, { pq with elements = Set.remove (e, id) pq.elements })

let rec tryPopN n pq =
  if n <= 0 then [], pq
  else
    match tryPop pq with
    | None -> [], pq
    | Some (x, pq) ->
        let xs, pq = tryPopN (n - 1) pq
        x :: xs, pq
