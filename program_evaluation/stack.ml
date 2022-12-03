open Stack_frame


(* Defines the stack *)
type stack = Stack of frame list;;

let empty_stack = Stack([]);;

(* Push a frame to the stack. *)
let push (s: stack) (f: frame): stack = match s with Stack(l) -> Stack(f::l);;

(* Pop a frame from the stack. *)
let pop (s: stack): stack = match s with Stack([]) -> Stack([]) | Stack(_::rest) -> Stack(rest);;

(* Get whats on top of the stack, returns None when empty. *)
let seektop (s: stack): frame option = match s with Stack([]) -> None | Stack(e::_) -> Some(e);;

(* Get whats on the bottom of the stack, returns None when empty. *)
let rec seekbottom (s: stack): frame option = match s with Stack([]) -> None| Stack([f]) -> Some(f) | Stack(_::rest) -> seekbottom (Stack(rest))

(* Update the globale vairable table, at the bottom of the stack. *)
(* TODO: This can be aviaded if a global vairable table is mainted for the recursive tree. *)
let rec update_glob_vartab (s: stack) (x: string) (v: albatross_type): stack = 
  match s with 
  | Stack([]) -> s
  | Stack([Frame(Glob_ct,vt,res)]) -> Stack([Frame(Glob_ct, (update_vartab vt x v), res)])
  | Stack(e::rest) -> match (update_glob_vartab (Stack(rest)) x v) with Stack(l) -> Stack(e::l)

(* Look in the global vairable table, at the bottom of the stack. *)
let rec look_glob_vartab (s: stack) (x: string) : albatross_type option = 
  match s with 
  | Stack([]) -> None
  | Stack([Frame(_,vt,_)]) -> lookup_vartab vt x
  | Stack(_::rest) -> look_glob_vartab (Stack(rest)) x