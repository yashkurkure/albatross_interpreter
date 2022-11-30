open Stack_frame

type stack = Stack of frame list;;

let empty_stack = Stack([]);;

let push (s: stack) (f: frame): stack = match s with Stack(l) -> Stack(f::l);;

let pop (s: stack): stack = match s with Stack([]) -> Stack([]) | Stack(_::rest) -> Stack(rest);;

let seektop (s: stack): frame option = match s with Stack([]) -> None | Stack(e::_) -> Some(e);;

let rec seeki (s: stack) (i: int): frame option = 
  match i with
  | 0 -> (match s with 
          | Stack([]) -> None
          | Stack (e::_) -> Some(e))
  | i' when i' > 0-> (match s with 
          | Stack([]) -> None
          | Stack(_::rest) -> seeki (Stack(rest)) (i'-1) )
  | _ -> None;;

let rec seekbottom (s: stack): frame option = match s with Stack([]) -> None| Stack([f]) -> Some(f) | Stack(_::rest) -> seekbottom (Stack(rest))