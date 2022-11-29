let surroundParens s:string = "("^s^")"

let rec contains (l: 'b list) (elem: 'a) (decider: 'b->'a -> bool): bool = 
  match l with
  | e::rest -> if decider e elem then true else contains rest elem decider
  | [] -> false;;