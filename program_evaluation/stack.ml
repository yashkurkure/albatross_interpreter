open Stack_frame



type stack = Stack of frame list;;

let (+::+) (f:frame) (s:stack) : stack = match s with Stack(l) -> Stack(f::l)

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

let rec update_glob_vartab (s: stack) (x: string) (v: albatross_type): stack = 
  match s with 
  | Stack([]) -> s
  | Stack([Frame(Glob_ct,vt,res)]) -> Stack([Frame(Glob_ct, (update_vartab vt x v), res)])
  | Stack(e::rest) -> match (update_glob_vartab (Stack(rest)) x v) with Stack(l) -> Stack(e::l)

let rec look_glob_vartab (s: stack) (x: string) : albatross_type option = 
  match s with 
  | Stack([]) -> None
  | Stack([Frame(_,vt,_)]) -> lookup_vartab vt x
  | Stack(_::rest) -> look_glob_vartab (Stack(rest)) x