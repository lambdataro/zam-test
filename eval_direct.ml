(*========================================*
 * ZAM テスト実装
 * eval_direct.ml
 *========================================*)

open Syntax

(* 値 *)
type value =
  | CLOSURE of id * expr * env
  | INT of int

(* 環境 *)
and env = (id * value ref) list

(* 評価 *)
let rec eval env = function
  | Int n ->
    INT n
  | BinApp (op, e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    eval_binop op v1 v2
  | Var x ->
    !(List.assoc x env)
  | Fun (x, e) ->
    CLOSURE (x, e, env)
  | App [] ->
    failwith "app"
  | App (e :: es) ->
    let v = eval env e in
    eval_app env v es
  | If (e1, e2, e3) ->
    begin
      match eval env e1 with
      | INT 0 -> eval env e3
      | INT _ -> eval env e2
      | _ -> failwith "if"
    end
  | LetRec (x, e1, e2) ->
    let r = ref (INT 0) in
    let env' = (x, r) :: env in
    let v = eval env' e1 in
    r := v;
    eval env' e2

and eval_binop op v1 v2 =
  match op, v1, v2 with
  | Add, INT n1, INT n2 -> INT (n1 + n2)
  | Sub, INT n1, INT n2 -> INT (n1 - n2)
  | Mul, INT n1, INT n2 -> INT (n1 * n2)
  | Lt,  INT n1, INT n2 ->
    if n1 < n2 then INT 1 else INT 0
  | Le,  INT n1, INT n2 ->
    if n1 <= n2 then INT 1 else INT 0
  | _ -> failwith "eval_binop"

and eval_app env f = function
  | [] -> f
  | e :: rest ->
    let v = eval env e in
    let f2 = 
      match f with
      | CLOSURE (arg, body, fenv) ->
        eval ((arg, ref v) :: fenv) body
      | _ ->
        failwith "app"
    in
    eval_app env f2 rest

(* 実行 *)
let run expr =
  let r = Util.print_time (fun () -> eval [] expr) in
  match r with
  | INT n -> Printf.printf "result = %d\n" n
  | CLOSURE _ -> Printf.printf "result = <closure>\n"

(* ベンチマーク *)
let run_bench expr n =
  Util.benchmark n (fun () -> eval [] expr)


