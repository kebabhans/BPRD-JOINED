let rec exists env x = match env with 
    | []        -> false
    | (y, v)::r -> if x=y then true else exists r x



let rec eval (e : expr) (env : value env) : value = match e with
    | CstI i -> Int i
    | CstB b -> Int (if b then 1 else 0)
    | Var x  -> lookup env x
    | Prim(ope, e1, e2) -> 
      let v1 = eval e1 env
      let v2 = eval e2 env
      match (ope, v1, v2) with
      | ("*", Int i1, Int i2) -> Int (i1 * i2)
      | ("+", Int i1, Int i2) -> Int (i1 + i2)
      | ("-", Int i1, Int i2) -> Int (i1 - i2)
      | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
      | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
      |  _ -> failwith "unknown primitive or wrong type"
    | Let(x, eRhs, letBody) -> 
      let xVal = eval eRhs env
      let letEnv = (x, xVal) :: env 
      eval letBody letEnv
    | If(e1, e2, e3) -> 
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"
    | Letfun(f, x, fBody, letBody) -> 
      let bodyEnv = (f, Closure(f, x, fBody, env)) :: env
      eval letBody bodyEnv
    | Fun(x, funBody) -> 
      Clos(x, funBody, env)
    | Call(eFun, eArg) -> 
      let fClosure = eval eFun env
      match fClosure with
      | Closure (f, x, fBody, fDeclEnv) ->
        let xVal = eval eArg env
        let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
        in eval fBody fBodyEnv
      | Clos (x, funBody, closEnv) ->
        let xVal = eval eArg env
        let newEnv = (x, xVal) :: closEnv
        eval funBody closEnv
      | _ -> failwith "eval Call: not a function";;


let rec check expr = match expr with
  | CstI i										  -> 	true
  | CstB b										  -> 	true
  | Var s										    -> 	exists s
  | Let(s,e1,e2)								-> 	true // check e1 && check e2 + extend environment
  | Prim(op,e1,e2)							-> 	true // Check e1 && e2 exists
  | If(cond,e2,e3)							-> 	true // Check e1 && e2 exists 
  | Letfun(name,e1,e2)					->  true // extend environment
  | Call(e1,e2)									-> 	let env : string list = []