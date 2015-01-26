(*========================================*
 * ZAM テスト実装
 * zam_array.ml
 * - zam_listの環境を配列に変更
 *========================================*)

open Util
open Syntax

(* 命令 *)
type inst =
  | Access of int
  | Closure of inst list
  | TailApply
  | Apply
  | PushMark
  | Grab
  | Return
  | ConstInt of int
  | IAdd
  | ISub
  | IMul
  | ILe
  | ILt
  | IfThen of inst list * inst list
  | Dummy
  | Update
  | EndLet

(* 値 *)
type value =
  | CLOSURE of inst list * env
  | MARK
  | INT of int
  | REF of value ref

(* 環境 *)
and env = value array

(* 値の参照を剥がす *)
let rec deref = function
  | REF r -> deref !r
  | v -> v

(* 環境の拡張 *)
let extenv v e =
  let len = Array.length e in
  let e' = Array.make (len + 1) MARK in
  Array.blit e 0 e' 1 len;
  e'.(0) <- v;
  e'

(* 環境からの値の取り出し *)
let getenv e n =
  let rec loop = function
    | REF r -> loop !r
    | v -> v
  in
  loop e.(n)

(* 環境から値をpopする *)
let popenv e =
  let len = Array.length e in
  let e' = Array.make (len - 1) MARK in
  Array.blit e 1 e' 0 (len - 1);
  e'

(*========================================*
 * 命令
 *========================================*)

(* access 命令 *)
let access n = function
  | c, e, s, r ->
    (c, e, getenv e n :: s, r)

(* closure 命令 *)
let closure c' = function
  | c, e, s, r ->
    let v = CLOSURE (c', e) in
    (c, e, v :: s, r)

(* tailapply 命令 *)
let tailapply = function
  | c, e, CLOSURE (c', e') :: s, r ->
    (c', e', s, r)
  | _ ->
    failwith "tailapply"

(* apply 命令 *)
let apply = function
  | c, e, CLOSURE (c', e') :: s, r ->
    (c', e', s, (c, e) :: r)
  | _ ->
    failwith "apply"

(* pushmark 命令 *)
let pushmark = function
  | c, e, s, r ->
    (c, e, MARK :: s, r)

(* grab 命令 *)
let grab = function
  | c, e, MARK :: s, (c', e') :: r ->
    let v = CLOSURE (Grab :: c, e) in
    (c', e', v :: s, r)
  | c, e, v :: s, r ->
    (c, extenv v e, s, r)
  | _ ->
    failwith "grab"

(* pushmark 命令 *)
let return = function
  | c, e, v :: MARK :: s, (c', e') :: r ->
    (c', e', v :: s, r)
  | c, e, CLOSURE (c', e') :: s, r ->
    (c', e', s, r)
  | _ ->
    failwith "return"

(* const_int 命令 *)
let const_int n = function
  | c, e, s, r ->
    (c, e, INT n :: s, r)

(* iadd 命令 *)
let iadd = function
  | c, e, INT n2 :: INT n1 :: s, r ->
    let v = INT (n1 + n2) in
    (c, e, v :: s, r)
  | _ ->
    failwith "iadd"

(* isub 命令 *)
let isub = function
  | c, e, INT n2 :: INT n1 :: s, r ->
    let v = INT (n1 - n2) in
    (c, e, v :: s, r)
  | _ ->
    failwith "iadd"

(* imul 命令 *)
let imul = function
  | c, e, INT n2 :: INT n1 :: s, r ->
    let v = INT (n1 * n2) in
    (c, e, v :: s, r)
  | _ ->
    failwith "imul"

(* ile 命令 *)
let ile = function
  | c, e, INT n2 :: INT n1 :: s, r ->
    let v = 
      if n1 <= n2 then INT 1
      else INT 0
    in
    (c, e, v :: s, r)
  | _ ->
    failwith "ile"


(* ilt 命令 *)
let ilt = function
  | c, e, INT n2 :: INT n1 :: s, r ->
    let v = 
      if n1 < n2 then INT 1
      else INT 0
    in
    (c, e, v :: s, r)
  | _ ->
    failwith "ilt"

(* ifthen 命令 *)
(* ifを実行するたびに命令の連結を行うのは効率が良くない *)
let ifthen c2 c3 = function
  | c, e, INT 0 :: s, r ->
    (c3 @ c, e, s, r)
  | c, e, INT _ :: s, r ->
    (c2 @ c, e, s, r)
  | _ ->
    failwith "ifthen"

(* dummy 命令 *)
let dummy = function
  | c, e, s, r ->
    let v = REF (ref MARK) in
    (c, extenv v e, s, r)

(* update 命令 *)
let update = function
  | c, e, v :: s, r ->
    begin
      match e.(0) with
      | REF f -> f := v
      | _ -> failwith "update"
    end;
    (c, e, v :: s, r)
  | _ ->
    failwith "update"

(* endlet 命令 *)
let endlet = function
  | c, e, s, r ->
    (c, popenv e, s, r)

(* 命令の実行 *)
(* 命令を実行する関数をlookupするのは効率が悪い *)
let execute code =
  let rec loop (code, e, s, r) =
    match code with
    | [] -> List.hd s
    | c :: cs ->
      let f =
        match c with
        | Access n -> access n
        | Closure c' -> closure c'
        | TailApply -> tailapply
        | Apply -> apply
        | PushMark -> pushmark
        | Grab -> grab
        | Return -> return
        | ConstInt n -> const_int n
        | IAdd -> iadd
        | ISub -> isub
        | IMul -> imul
        | ILe -> ile
        | ILt -> ilt
        | IfThen (c2, c3) -> ifthen c2 c3
        | Dummy -> dummy
        | Update -> update
        | EndLet -> endlet
      in
      loop (f (cs, e, s, r))
  in
  loop (code, [| |], [], [])

(*========================================*
 * コンパイル
 *========================================*)
 
let rec comp_c env = function
  | Int n ->
    [ConstInt n]
  | BinApp (Add, e1, e2) ->
    comp_c env e1 @ comp_c env e2 @ [IAdd]
  | BinApp (Sub, e1, e2) -> 
    comp_c env e1 @ comp_c env e2 @ [ISub]
  | BinApp (Mul, e1, e2) -> 
    comp_c env e1 @ comp_c env e2 @ [IMul]
  | BinApp (Lt, e1, e2) -> 
    comp_c env e1 @ comp_c env e2 @ [ILt]
  | BinApp (Le, e1, e2) -> 
    comp_c env e1 @ comp_c env e2 @ [ILe]
  | Var x ->
    [Access (index x env)]
  | Fun (_, _) as e ->
    [Closure (comp_t env e)]
  | App es ->
    PushMark ::
    List.concat (List.rev_map (comp_c env) es) @
    [Apply]
  | If (e1, e2, e3) ->
    comp_c env e1 @ [IfThen (comp_c env e2, comp_c env e3)]
  | LetRec (x, e1, e2) ->
    Dummy :: comp_c (x::env) e1 @ Update :: comp_c (x::env) e2 @ [EndLet]

and comp_t env = function
  | Fun (x, e) -> Grab :: comp_t (x :: env) e
  | App es ->
    List.concat (List.rev_map (comp_c env) es) @
    [TailApply]
  | LetRec (x, e1, e2) ->
    Dummy :: comp_c (x::env) e1 @ Update :: comp_t (x::env) e2
  | If (e1, e2, e3) ->
    comp_c env e1 @ [IfThen (comp_t env e2, comp_t env e3)]
  | e -> comp_c env e @ [Return]

(*========================================*
 * 実行
 *========================================*)

let run expr =
  let code = comp_c [] expr in
  let v = print_time (fun () -> execute code) in
  match deref v with
  | INT n -> Printf.printf "result = %d\n" n
  | CLOSURE _ -> print_endline "result = <closure>"
  | REF _ -> print_endline "result = <ref>"
  | MARK -> print_endline "result = <mark>"

let run_bench expr n =
  let code = comp_c [] expr in
  benchmark n (fun () -> execute code)

