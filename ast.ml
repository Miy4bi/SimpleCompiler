(* The definition of the abstract syntax tree *)
type id = string
type var = Var of id | IndexedVar of var * exp
and stmt = Assign of var * exp
        | CallProc of id * (exp list)
        | CallInit of var * exp
        | CallAdd of var * exp
        | CallInc of var
        | Block of (dec list) * (stmt list)
        | If of exp * stmt * (stmt option)
        | While of exp * stmt
        | Do of stmt * exp
        | For of var * exp * exp * stmt
        | NilStmt
and exp = VarExp of var | StrExp of string | IntExp of int
        | CallFunc of id * (exp list) | CallInc of var | CallAdd of var * exp
and dec = FuncDec of id * ((typ*id) list) * typ * stmt
        | TypeDec of id * typ
        | VarDec of typ * id
and init = InitVar of id * exp
and typ = NameTyp of string
        | ArrayTyp of int * typ
        | IntTyp
        | VoidTyp

