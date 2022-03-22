type heap = EMPTY
          | NODE of rank * value * heap * heap
and  rank = int
and value = int

exception EmptyHeap

let rank h =
  match h with
  | EMPTY -> -1
  | NODE (r,_,_,_) -> r

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let deleteMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE( ,x,lh,rh) -> merge(lh,rh)

let shake (x,lh,rh) =
  if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge: heap * heap -> heap = fun (h1, h2) ->
  match (h1, h2) with
  | EMPTY, h -> h
  | h, EMPTY -> h
  | NODE (_, x1, a1, b1), NODE (_, x2, a2, b2) ->
    if (x1 < x2) then
      shake (x1, a1, (merge b1 h2))
    else
      shake (x2, a2, (merge h1 b2))
