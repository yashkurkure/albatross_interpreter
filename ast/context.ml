
(*  Defines the context where a statement/expression is defined.
Glob_ct: refers to the global context.
Func_ct(x): refers to the context of the function x 

Using this value the interpreter can decides what it needs
to push/pop on the stack/ where to look up values/ how to 
resolve variable shadowing etc.
*)
type context = Glob_ct | Func_ct of string