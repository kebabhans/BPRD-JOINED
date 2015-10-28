// Author: Morten

type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string * expr * expr    (* (f, x, fBody, letBody) *)
  | Call of expr * expr

let rec lookup env x = match env with
    | []        -> false
    | y::xs     -> if y = x then true else lookup xs x


let rec evaluate expr (environment : string list) = match expr with
    | CstI i                      ->  true
    | CstB b                      ->  true
    | Let(name,e1,e2)             ->  let extEnvironment = name :: environment
                                      (evaluate e1 extEnvironment && evaluate e2 extEnvironment)              // Might return true or false
    | Prim(_,e1,e2)               ->  (evaluate e1 environment && evaluate e2 environment)
    | Var s                       ->  lookup environment s
    | If(e1,e2,e3)                ->  (evaluate e1 environment && evaluate e2 environment && evaluate e3 environment)
    | Letfun(f,name, e1,e2)       ->  let extEnvironment = f :: name :: environment
                                      (evaluate e1 extEnvironment && evaluate e2 extEnvironment)
    | Call(e1,e2)                 ->  evaluate e1 environment && evaluate e2 environment



let rec check expr = 
    let environment : string list = []
    evaluate expr environment

// Samples
let s2 = CstI(12)
let s4 = CstB(false)
let s5 = Let("x",Prim("+",Var "x", Var "x"),Call(Var "x",CstI(2)))

let s12 = Letfun ("twice", "f",
                  Letfun ("g", "x", Call (Var "f", Call (Var "f", Var "x")), Var "g"),
                          Letfun ("mul3", "z", Prim ("*", Var "z", CstI 3),
                                  Call (Call (Var "twice",Var "mul3"),CstI 2)))

let s13 = Let("x",Prim("+",Var "y", Var "x"),Call(Var "x",CstI(2)))
check s13     // Should produce false

let s14 = Call(Var "f",CstI 2)
check s14   // Should produce false

check s1    // Should produce true
check s2
