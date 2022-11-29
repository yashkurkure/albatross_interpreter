type 'a stack = Stack of 'a list

let empty_stack = Stack([])

let push (s: 'a stack) (e: 'a): 'a stack = match s with Stack(l) -> Stack(e::l)

let pop (s: 'a stack): 'a stack = match s with Stack([]) -> Stack([]) | Stack(_::rest) -> Stack(rest)

let seek (s: 'a stack): 'a option = match s with Stack([]) -> None | Stack(e::_) -> Some(e)
