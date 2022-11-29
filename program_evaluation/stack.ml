type 'a stack = Stack of 'a list;;

let empty_stack = Stack([]);;

let push (s: 'a stack) (e: 'a): 'a stack = match s with Stack(l) -> Stack(e::l);;

let pop (s: 'a stack): 'a stack = match s with Stack([]) -> Stack([]) | Stack(_::rest) -> Stack(rest);;

let seek (s: 'a stack): 'a option = match s with Stack([]) -> None | Stack(e::_) -> Some(e);;

let rec seeki (s: 'a stack) (i: int): 'a option = 
  match i with
  | 0 -> (match s with 
          | Stack([]) -> None
          | Stack (e::_) -> Some(e))
  | i' when i' > 0-> (match s with 
          | Stack([]) -> None
          | Stack(_::rest) -> seeki (Stack(rest)) (i'-1) )
  | _ -> None;;