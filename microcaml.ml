
(* Token Types *)

exception InvalidInputException of string

type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi

let string_of_token (t : token) : string = match t with
  | Tok_Sub -> "Tok_Sub"
  | Tok_RParen -> "Tok_RParen"
  | Tok_Add -> "Tok_Add"
  | Tok_Or -> "Tok_Or"
  | Tok_NotEqual -> "Tok_NotEqual"
  | Tok_Not -> "Tok_Not"
  | Tok_Mult -> "Tok_Mult"
  | Tok_LessEqual -> "Tok_LessEqual"
  | Tok_Less -> "Tok_Less"
  | Tok_LParen -> "Tok_LParen"
  | Tok_Int(i) -> "Tok_Int(" ^ (string_of_int i) ^ ")"
  | Tok_If -> "Tok_If"
  | Tok_ID(id) -> "Tok_ID(\"" ^ id ^ "\")"
  | Tok_String(s) -> "Tok_String(\"" ^ s ^ "\")"
  | Tok_GreaterEqual -> "Tok_GreaterEqual"
  | Tok_Greater -> "Tok_Greater"
  | Tok_Equal -> "Tok_Equal"
  | Tok_Then -> "Tok_Then"
  | Tok_Else -> "Tok_Else"
  | Tok_Div -> "Tok_Div"
  | Tok_Bool(b) -> "Tok_Bool(" ^ (string_of_bool b) ^ ")"
  | Tok_And -> "Tok_And"
  | Tok_Concat -> "Tok_Concat"
  | Tok_Let -> "Tok_Let"
  | Tok_Def -> "Tok_Def"
  | Tok_In -> "Tok_In"
  | Tok_Rec -> "Tok_Rec"
  | Tok_Arrow -> "Tok_Arrow"
  | Tok_Fun -> "Tok_Fun"
  | Tok_DoubleSemi -> "Tok_DoubleSemi"

let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

(* Microcaml Types *)

type op = Add | Sub | Mult | Div | Concat | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual | Or | And

type var = string

type value =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of environment * var * expr

and environment = (var * value ref) list

and expr =
  | Value of value
  | ID of var
  | Fun of var * expr
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | FunctionCall of expr * expr
  | Let of var * bool * expr * expr

(*
type mutop = 
  | Def of var * expr
  | Expr of expr
  | NoOp
*)

(* Lexer *)

let kw = [ ( "not",  Tok_Not  ); ( "if",  Tok_If  ); ( "then", Tok_Then );
           ( "else", Tok_Else ); ( "let", Tok_Let ); ( "def",  Tok_Def  );
           ( "in",   Tok_In   ); ( "rec", Tok_Rec ); ( "fun",  Tok_Fun  );
           ( "true", (Tok_Bool true) ); ( "false", (Tok_Bool false) )      ]

let re_id  = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_neg = Str.regexp "(\\(-[0-9]+\\))"
let re_pos = Str.regexp "[0-9]+"
let re_str = Str.regexp "\"\\([^\"]*\\)\""

let tokenize input =
  let n = String.length input in
  let rec tok i acc =
    if i >= n then acc
    else match input.[i] with
      | ' ' | '\t' | '\n' -> tok (i+1) acc
      | '=' -> tok (i+1) (Tok_Equal  :: acc)
      | '+' -> tok (i+1) (Tok_Add    :: acc)
      | '*' -> tok (i+1) (Tok_Mult   :: acc)
      | '/' -> tok (i+1) (Tok_Div    :: acc)
      | '^' -> tok (i+1) (Tok_Concat :: acc)
      | ')' -> tok (i+1) (Tok_RParen :: acc)
      | '|' when (i+1) < n && input.[i+1] = '|' ->
        tok (i+2) (Tok_Or :: acc)
      | '&' when (i+1) < n && input.[i+1] = '&' ->
        tok (i+2) (Tok_And :: acc)
      | ';' when (i+1) < n && input.[i+1] = ';' ->
        tok (i+2) (Tok_DoubleSemi :: acc)
      | '-' when (i+1) < n && input.[i+1] = '>' ->
        tok (i+2) (Tok_Arrow :: acc)
      | '-' ->
        tok (i+1) (Tok_Sub :: acc)
      | '>' when (i+1) < n && input.[i+1] = '=' ->
        tok (i+2) (Tok_GreaterEqual :: acc)
      | '>' ->
        tok (i+1) (Tok_Greater :: acc)
      | '<' when (i+1) < n && input.[i+1] = '>' ->
        tok (i+2) (Tok_NotEqual :: acc)
      | '<' when (i+1) < n && input.[i+1] = '=' ->
        tok (i+2) (Tok_LessEqual :: acc)
      | '<' ->
        tok (i+1) (Tok_Less :: acc)
      | '(' when Str.string_match re_neg input i ->
        let x = Str.matched_group 1 input in
        let y = int_of_string x in
        tok ( i + 2 + String.length x ) ( (Tok_Int y) :: acc )
      | '(' ->
        tok (i+1) (Tok_LParen :: acc)
      | '0'..'9' ->
        let _ = Str.string_match re_pos input i in
        let x = Str.matched_string input in
        let y = int_of_string x in
          tok ( i + String.length x ) ( (Tok_Int y) :: acc )
      | 'a'..'z' | 'A'..'Z' ->
        let _ = Str.string_match re_id input i in
        let x = Str.matched_string input in (
          match List.assoc_opt x kw with
          | Some t -> tok ( i + String.length x ) (t :: acc)
          | None   -> tok ( i + String.length x ) ( (Tok_ID x) :: acc )
        )
      | '"' when Str.string_match re_str input i ->
        let x = Str.matched_group 1 input in
          tok ( i + 2 + String.length x ) ( (Tok_String x) :: acc )
      | c ->
        raise ( InvalidInputException ("unknown token " ^ String.make 1 c) )
  in
  tok 0 [] |> List.rev

(* Parser *)

let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

let rec parse_expr toks =
  match toks with
  | Tok_Let :: _ -> let_expr toks
  | Tok_Fun :: _ -> fun_expr toks
  | Tok_If  :: _ -> if_expr  toks
  | _            -> or_expr  toks
and let_expr toks =
  let aux x r s =
    let (t0, e0) = parse_expr x in
    let (t1, e1) = match_token t0 Tok_In |> parse_expr in
    t1, Let (s, r, e0, e1)
  in
  match toks with
  | Tok_Let :: Tok_Rec :: (Tok_ID s0) :: Tok_Equal :: a -> aux a true  s0
  | Tok_Let ::            (Tok_ID s1) :: Tok_Equal :: b -> aux b false s1
  | _ -> raise (InvalidInputException "parser error: let_expr")
and fun_expr = function
  | Tok_Fun :: (Tok_ID s) :: Tok_Arrow :: x ->
    let (t, e) = parse_expr x in t, Fun (s, e)
  | _ -> raise (InvalidInputException "parser error: fun_expr")
and if_expr toks =
  let (t0, e0) = match_token toks Tok_If   |> parse_expr in
  let (t1, e1) = match_token t0   Tok_Then |> parse_expr in
  let (t2, e2) = match_token t1   Tok_Else |> parse_expr in
    t2, If (e0, e1, e2)
and or_expr toks =
  let (t0, e0) = and_expr toks in
  match t0 with
  | Tok_Or :: x -> 
    let (t1, e1) = or_expr x in t1, Binop (Or, e0, e1)
  | _ -> t0, e0
and and_expr toks =
  let (t0, e0) = eq_expr toks in
  match t0 with
  | Tok_And :: x ->
    let (t1, e1) = and_expr x in t1, Binop (And, e0, e1)
  | _ -> t0, e0
and eq_expr toks =
  let (t0, e0) = rel_expr toks in
  match t0 with
  | Tok_Equal :: x ->
    let (t1, e1) = eq_expr x in t1, Binop (Equal, e0, e1)
  | Tok_NotEqual :: y ->
    let (t2, e2) = eq_expr y in t2, Binop (NotEqual, e0, e2)
  | _ -> t0, e0
and rel_expr toks =
  let (t0, e0) = add_expr toks in
  match t0 with
  | Tok_Less :: a ->
    let (t1, e1) = rel_expr a in t1, Binop (Less, e0, e1)
  | Tok_Greater :: b ->
    let (t2, e2) = rel_expr b in t2, Binop (Greater, e0, e2)
  | Tok_LessEqual :: c ->
    let (t3, e3) = rel_expr c in t3, Binop (LessEqual, e0, e3)
  | Tok_GreaterEqual :: d ->
    let (t4, e4) = rel_expr d in t4, Binop (GreaterEqual, e0, e4)
  | _ -> t0, e0
and add_expr toks =
  let (t0, e0) = mul_expr toks in
  match t0 with
  | Tok_Add :: x ->
    let (t1, e1) = add_expr x in t1, Binop (Add, e0, e1)
  | Tok_Sub :: y ->
    let (t2, e2) = add_expr y in t2, Binop (Sub, e0, e2)
  | _ -> t0, e0
and mul_expr toks =
  let (t0, e0) = concat_expr toks in
  match t0 with
  | Tok_Mult :: x ->
    let (t1, e1) = mul_expr x in t1, Binop (Mult, e0, e1)
  | Tok_Div :: y ->
    let (t2, e2) = mul_expr y in t2, Binop (Div, e0, e2)
  | _ -> t0, e0
and concat_expr toks =
  let (t0, e0) = uni_expr toks in
  match t0 with
  | Tok_Concat :: x ->
    let (t1, e1) = concat_expr x in t1, Binop (Concat, e0, e1)
  | _ -> t0, e0
and uni_expr = function
  | Tok_Not :: x ->
    let (t, e) = uni_expr x in t, Not e
  | toks -> fncall_expr toks
and fncall_expr toks =
  let (t0, e0) = prim_expr toks in
  match t0 with
  | (Tok_Int _) :: _
  | (Tok_Bool _) :: _
  | (Tok_String _) :: _
  | (Tok_ID _) :: _
  | Tok_LParen :: _ ->
    let (t1, e1) = prim_expr t0 in t1, FunctionCall (e0, e1)
  | _ -> t0, e0
and prim_expr = function
  | (Tok_Int i)    :: d -> d, Value (Int i)
  | (Tok_Bool b)   :: c -> c, Value (Bool b)
  | (Tok_String s) :: b -> b, Value (String s)
  | (Tok_ID id)    :: a -> a, ID id
  | Tok_LParen     :: x ->
    let (t, e) = parse_expr x in (match_token t Tok_RParen), e
  | _ -> raise (InvalidInputException "expect primary expression")

(*
let rec parse_mutop toks =
  match toks with
  | [ Tok_DoubleSemi ] -> [], NoOp
  | Tok_Def :: (Tok_ID id) :: Tok_Equal :: x ->
    let (t0, e0) = parse_expr x in
    (match_token t0 Tok_DoubleSemi), Def (id, e0)
  | _ ->
    let (t1, e1) = parse_expr toks in
    (match_token t1 Tok_DoubleSemi), Expr e1
*)


(* Code Generator: translate microcaml to javascript *)

(* javascript statements *)
type jstmt =
  | JS_assign of string * string
  | JS_typecheck of string * string
  | JS_return of string
  | JS_vardec of string
  | JS_fun of string * string * jstmt list
  | JS_if of string * jstmt list
  | JS_else of jstmt list

let tab = "  "

let rec jstmt_to_str ind stmt =
  let inner = (fun acc st -> acc ^ jstmt_to_str (ind ^ tab) st) in
  match stmt with
  | JS_assign (lhs, rhs) ->
    Printf.sprintf "%s%s = %s;\n" ind lhs rhs
  | JS_typecheck (var, typ) ->
    Printf.sprintf "%s$assert( typeof %s == \"%s\" );\n" ind var typ
  | JS_return var ->
    Printf.sprintf "%sreturn %s;\n" ind var
  | JS_vardec var ->
    Printf.sprintf "%slet %s;\n" ind var
  | JS_fun (id, arg, body) ->
    Printf.sprintf "%sfunction %s(%s) {\n%s%s}\n"
      ind id arg (List.fold_left inner "" body) ind
  | JS_if (cond, body) ->
    Printf.sprintf "%sif(%s) {\n%s%s}\n"
      ind cond (List.fold_left inner "" body) ind
  | JS_else body ->
    Printf.sprintf "%selse {\n%s%s}\n"
      ind (List.fold_left inner "" body) ind

exception CompileError of string

let cnt = ref (-1)
let gen_var () = cnt := !cnt + 1; "$t" ^ string_of_int !cnt

let js_op = [
  (Add, "$add"); (Sub, "$sub"); (Mult, "$mul"); (Div, "$div");
  (Greater, "$gt"); (Less, "$lt"); (GreaterEqual, "$ge");
  (LessEqual, "$le"); (Concat, "$concat"); (Equal, "$eq");
  (NotEqual, "$neq")
]

let rec jsgen sym var = function
  | Value (Int x)    -> [ JS_assign (var, (string_of_int  x)) ]
  | Value (Bool x)   -> [ JS_assign (var, (string_of_bool x)) ]
  | Value (String x) -> [ JS_assign (var, ("`" ^ x ^ "`"))    ]
  | ID id -> (
    match List.assoc_opt id sym with
    | Some s -> [ JS_assign (var, s) ]
    | None   -> raise ( CompileError ("unbound variable \"" ^ id ^ "\"") ) )
  | Not e ->
    let t = gen_var () in
      [ JS_vardec t ] @
      (jsgen sym t e) @ [
        JS_typecheck (t, "boolean") ;
        JS_assign (var, ("!" ^ t))
      ]
  | Fun (param, body) ->
    let t0, t1, t2 = gen_var (), gen_var (), gen_var() in
      let x =
        [ JS_vardec t2 ] @
        (jsgen ((param, t1) :: sym) t2 body) @
        [ JS_return t2 ]
      in
      [ JS_fun (t0, t1, x); JS_assign (var, t0) ]
  | Binop (Add          as bop, e0, e1)
  | Binop (Sub          as bop, e0, e1)
  | Binop (Mult         as bop, e0, e1)
  | Binop (Div          as bop, e0, e1)
  | Binop (Greater      as bop, e0, e1)
  | Binop (GreaterEqual as bop, e0, e1)
  | Binop (Less         as bop, e0, e1)
  | Binop (LessEqual    as bop, e0, e1)
  | Binop (Concat       as bop, e0, e1)
  | Binop (Equal        as bop, e0, e1)
  | Binop (NotEqual     as bop, e0, e1) ->
    let t0, t1 = gen_var (), gen_var () in
      [ JS_vardec (t0 ^ ", " ^ t1) ] @
      (jsgen sym t0 e0) @
      (jsgen sym t1 e1) @
      [ JS_assign (var, (List.assoc bop js_op) ^ "(" ^ t0 ^ ", " ^ t1 ^ ")") ]
  | Binop (Or  as bop, e0, e1)
  | Binop (And as bop, e0, e1) ->
    let t0, t1 = gen_var (), gen_var () in
    let x = (jsgen sym t1 e1) @ [ JS_typecheck (t1, "boolean") ] in
      [ JS_vardec (t0 ^ ", " ^ t1) ] @
      (jsgen sym t0 e0) @
      [ JS_typecheck (t0, "boolean") ;
        JS_if ( (if bop = Or then "!" ^ t0 else t0), x);
        JS_assign (var, (t0 ^ (if bop = Or then "||" else "&&") ^ t1))
      ]
  | If (cond, tbr, fbr) ->
    let t = gen_var () in
      [ JS_vardec t ] @
      (jsgen sym t cond) @
      [ JS_typecheck (t, "boolean") ;
        JS_if (t, (jsgen sym var tbr)) ;
        JS_else (jsgen sym var fbr)
      ]
  | FunctionCall (e0, e1) ->
    let t0, t1 = gen_var (), gen_var () in
      [ JS_vardec (t0 ^ ", " ^ t1) ] @
      (jsgen sym t0 e0) @
      (jsgen sym t1 e1) @
      [ JS_typecheck (t0, "function") ;
        JS_assign (var, t0 ^ "(" ^ t1 ^ ")")
      ]
  | Let (id, r, e0, e1) ->
    let t = gen_var () in
      [ JS_vardec t ] @
      (jsgen (if r then ((id, t) :: sym) else sym) t e0) @
      (jsgen ((id, t) :: sym) var e1)
  | _ -> raise ( CompileError "unknown expression" )


let prologue =
"(function(){ \"use strict\";
function $assert(cond, msg) { if( !cond ) throw new Error(msg); }
function $add(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a + b;
}
function $sub(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a - b;
}
function $mul(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a * b;
}
function $div(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return Math.floor(a/b);
}
function $concat(a, b) {
  $assert( typeof a == \"string\" && typeof b == \"string\" );
  return a + b;
}
function $gt(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a > b;
}
function $lt(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a < b;
}
function $ge(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a >= b;
}
function $le(a, b) {
  $assert( typeof a == \"number\" && typeof b == \"number\" );
  return a <= b;
}
function $eq(a, b) {
  $assert( typeof a == typeof b && typeof a != \"function\" );
  return a == b;
}
function $neq(a, b) {
  $assert( typeof a == typeof b && typeof a != \"function\" );
  return a != b;
}
"

let jsprog expr =
  let t = gen_var () in
    prologue ^
    "let $t0;\n" ^
    (jsgen [] t expr |> List.map (jstmt_to_str "") |> List.fold_left (^) "") ^
    "return $t0; })();"


(* Interpreter *)

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
let b0 = [ (Add, (+)); (Sub, (-)); (Mult, ( * ))]
let b1 = [ (Greater, (>)); (Less, (<)); (GreaterEqual, (>=)); (LessEqual, (<=)) ]
let b2 = [ (Equal, (=)); (NotEqual, (<>)) ]
let b3 = [ (Or, (||)); (And, (&&)) ]

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Value x -> x
  | ID x    -> lookup env x
  | Not x   -> (
    match eval_expr env x with
    | Bool b -> Bool (not b)
    | _      -> raise (TypeError ("expect type bool"))   )
  | Fun (x, y) -> Closure (env, x, y)
  | Binop (Add  as z, x, y)
  | Binop (Sub  as z, x, y)
  | Binop (Mult as z, x, y) -> (
    match (eval_expr env x, eval_expr env y) with
    | (Int a, Int b) -> let c = List.assoc z b0 in Int (c a b)
    | _              -> raise (TypeError ("expect type int"))   )
  | Binop (Div, x, y) -> (
    match (eval_expr env x, eval_expr env y) with
    | (Int a, Int b) -> if b > 0 then Int (a/b) else raise DivByZeroError
    | _              -> raise (TypeError ("expect type int"))   )
  | Binop (Greater      as z, x, y)
  | Binop (GreaterEqual as z, x, y)
  | Binop (Less         as z, x, y)
  | Binop (LessEqual    as z, x, y) -> (
    match (eval_expr env x, eval_expr env y) with
    | (Int a, Int b) -> let c = List.assoc z b1 in Bool (c a b)
    | _              -> raise (TypeError ("expect type int"))   )
  | Binop (Concat, x, y) -> (
    match (eval_expr env x, eval_expr env y) with
    | (String a, String b) -> String (a^b)
    | _                    -> raise (TypeError ("expect type string"))   )
  | Binop (Equal    as z, x, y)
  | Binop (NotEqual as z, x, y) -> (
    match (eval_expr env x, eval_expr env y) with
    | (Int a, Int b)       -> let c = List.assoc z b2 in Bool (c a b)
    | (Bool a, Bool b)     -> let c = List.assoc z b2 in Bool (c a b)
    | (String a, String b) -> let c = List.assoc z b2 in Bool (c a b)
    | _                    -> raise (TypeError ("cannot compare types"))   )
  | Binop (Or  as z, x, y)
  | Binop (And as z, x, y) -> (
    match (eval_expr env x, eval_expr env y) with
    | (Bool a, Bool b) -> let c = List.assoc z b3 in Bool (c a b)
    | _                -> raise (TypeError ("expect type bool"))   )
  | If (z, x, y) -> (
    match eval_expr env z with
    | Bool true  -> eval_expr env x
    | Bool false -> eval_expr env y
    | _          -> raise (TypeError ("expect type bool"))   )
  | FunctionCall (x, y) -> (
    match eval_expr env x with
    | Closure (a, b, c) ->
      let d = eval_expr env y in
      eval_expr (extend a b d) c
    | _ -> raise (TypeError ("expect type closure"))   )
  | Let (x, false, y, z) ->
    let a = eval_expr env y in eval_expr (extend env x a) z
  | Let (x, true, y, z) ->
    let a = extend_tmp env x in
    let b = eval_expr a y in
    let _ = update a x b in eval_expr a z

let print_val = function
  | Int    i -> print_endline (string_of_int  i)
  | Bool   b -> print_endline (string_of_bool b)
  | String s -> print_endline s
  | _        -> print_endline "[function]"




type action = Js | Eval

let () =
  let action = ref Eval in
  let set_action a () = action := a in
  let msg_j = "compile microcaml code to javascript" in
  let msg_e = "evaluate microcaml code using the interpreter" in
  let speclist = [
    ( "-j", Arg.Unit (set_action Js),   msg_j );
    ( "-e", Arg.Unit (set_action Eval), msg_e )
  ] in
  let usage_msg = "usage: ./_build/default/microcaml.exe [-j|-e] [filename]" in
  let ch = ref stdin in
  Arg.parse speclist (fun fname -> ch := open_in_bin fname) usage_msg;
  let str = really_input_string !ch (in_channel_length !ch) in
  close_in !ch;
  match !action with
  | Js   -> str |> tokenize |> parse_expr |> snd |> jsprog |> print_endline
  | Eval -> str |> tokenize |> parse_expr |> snd |> eval_expr [] |> print_val

