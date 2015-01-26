(*========================================*
 * ZAM テスト実装
 * zam_fast1.ml
 * - 環境をリストで表現
 * - 命令を関数ポインタで表現
 * - ifのオーバーヘッドを削除
 *========================================*)

open Util
open Syntax

(*========================================*
 * データ構造
 *========================================*)

(* 値 *)
type value =
  | INT of int
  | CLOSURE of code * env
  | REF of value ref
  | MARK

(* 状態 *)
and state = code * env * stack * dump

(* コード *)
and code = inst list

(* 環境 *)
and env = value list

(* スタック *)
and stack = value list

(* ダンプ *)
and dump = (code * env) list

(* 命令 *)
and inst = Inst of (state -> state)

(*========================================*
 * 補助関数
 *========================================*)

(* refを外す *)
let rec deref = function
  | REF r -> deref !r
  | v -> v

(* 環境から値を取り出す *)
let getenv e n = deref (List.nth e n)

(*========================================*
 * 基本命令
 *========================================*)

(* access 命令 *)
let access n = Inst (function
  | c, e, s, r ->
    (c, e, getenv e n :: s, r)
  )

(* closure 命令 *)
let closure c' = Inst (function
  | c, e, s, r ->
    (c, e, CLOSURE (c', e) :: s, r)
  )

(* tailapply 命令 *)
let tailapply = Inst (function
  | c, e, CLOSURE (c', e') :: s, r ->
    (c', e', s, r)
  | _ ->
    failwith "tailapply"
  )

(* apply 命令 *)
let apply = Inst (function
  | c, e, CLOSURE (c', e') :: s, r ->
    (c', e', s, (c, e) :: r)
  | _ ->
    failwith "apply"
  )

(* pushmark 命令 *)
let pushmark = Inst (function
  | c, e, s, r ->
    (c, e, MARK :: s, r)
  )

(* grab 命令 *)
let rec grab = Inst (function
  | c, e, MARK :: s, (c', e') :: r ->
    let v = CLOSURE (grab :: c, e) in
    (c', e', v :: s, r)
  | c, e, v :: s, r ->
    (c, v :: e, s, r)
  | _ ->
    failwith "grab"
  )

(* return 命令 *)
let return = Inst (function
  | c, e, v :: MARK :: s, (c', e') :: r ->
    (c', e', v :: s, r)
  | c, e, CLOSURE (c', e') :: s, r ->
    (c', e', s, r)
  | _ ->
    failwith "return"
  )

(*========================================*
 * let, let rec
 *========================================*)

(* dummy 命令 *)
let dummy = Inst (function
  | c, e, s, r ->
    let v = REF (ref MARK) in
    (c, v :: e, s, r)
  )

(* update 命令 *)
let update = Inst (function
  | c, e, v :: s, r ->
    begin
      match e with
      | REF f :: _ -> f := v
      | _ -> failwith "update"
    end;
    (c, e, v :: s, r)
  | _ -> failwith "update"
  )

(* endlet 命令 *)
let endlet = Inst (function
  | c, _ :: e, s, r ->
    (c, e, s, r)
  | _ -> failwith "endlet"
  )

(*========================================*
 * コントロール
 *========================================*)

(* if 命令 *)
let ifthen c2 = Inst (function
  | c, e, INT 0 :: s, r ->
    (c, e, s, r)
  | c, e, INT _ :: s, r ->
    (c2, e, s, r)
  | _ -> failwith "ifthen"
  )

(*========================================*
 * 定数
 *========================================*)

(* 整数定数 *)
let const_int n = Inst (function
  | c, e, s, r ->
    (c, e, INT n :: s, r)
  )

(*========================================*
 * 演算
 *========================================*)

(* 2項演算子 *)
let binop f = Inst (function
  | c, e, v1 :: v2 :: s, r ->
    (c, e, f v1 v2 :: s, r)
  | _ -> failwith "binop"
  )

(* 加算 *)
let iadd v2 v1 =
  match v1, v2 with
  | INT n1, INT n2 -> INT (n1 + n2)
  | _ -> failwith "iadd"

(* 減算 *)
let isub v2 v1 =
  match v1, v2 with
  | INT n1, INT n2 -> INT (n1 - n2)
  | _ -> failwith "isub"

(* 乗算 *)
let imul v2 v1 =
  match v1, v2 with
  | INT n1, INT n2 -> INT (n1 * n2)
  | _ -> failwith "imul"

(* 以下 *)
let le v2 v1 =
  match v1, v2 with
  | INT n1, INT n2 ->
    if n1 <= n2 then INT 1 else INT 0
  | _ -> failwith "le"

(* より小さい *)
let lt v2 v1 =
  match v1, v2 with
  | INT n1, INT n2 ->
    if n1 < n2 then INT 1 else INT 0
  | _ -> failwith "lt"

(*========================================*
 * コンパイル
 *========================================*)

let rec comp_c env inst rest =
  match inst with
  | Int n -> 
    [const_int n] @ rest
  | BinApp (op, e1, e2) ->
    comp_c env e1 (comp_c env e2 ([binop (binop_fun op)] @ rest)) 
  | Var x ->
    [access (index x env)] @ rest
  | Fun (_, _) as e ->
    [closure (comp_t env e [])] @ rest
  | App es ->
    pushmark :: concat_rev_map (fun x -> comp_c env x []) es @ [apply] @ rest
  | If (e1, e2, e3) ->
    comp_c env e1 (ifthen (comp_c env e2 rest) :: (comp_c env e3 rest))
  | LetRec (x, e1, e2) ->
    dummy :: comp_c (x :: env) e1 (update :: comp_c (x :: env) e2 (endlet :: rest))

and binop_fun = function
  | Add -> iadd
  | Sub -> isub
  | Mul -> imul
  | Le -> le
  | Lt -> lt

and comp_t env inst rest =
  match inst with
  | Fun (x, e) ->
    grab :: comp_t (x :: env) e rest
  | App es ->
    concat_rev_map (fun x -> comp_c env x []) es @ (tailapply :: rest)
  | If (e1, e2, e3) ->
    comp_c env e1 (ifthen (comp_t env e2 rest) :: (comp_t env e3 rest))
  | LetRec (x, e1, e2) ->
    dummy :: comp_c (x :: env) e1 (update :: comp_t (x :: env) e2 rest)
  | e ->
    comp_c env e [return]

(*========================================*
 * 実行
 *========================================*)

(* コードの実行 *)
let execute code =
  let rec loop (c, e, s, r) =
    match c with
    | [] -> List.hd s
    | Inst f :: cs -> loop (f (cs, e, s, r))
  in
  loop (code, [], [], [])

(* コンパイル *)
let run expr =
  let code = comp_c [] expr [] in
  let v = print_time (fun () -> execute code) in
  match deref v with
  | INT n -> Printf.printf "result = %d\n" n
  | CLOSURE _ -> print_endline "result = <closure>"
  | REF _ -> print_endline "result = <ref>"
  | MARK -> print_endline "result = <mark>"

(* ベンチマーク *)
let run_bench expr n =
  let code = comp_c [] expr [] in
  benchmark n (fun () -> execute code)

