(*========================================*
 * ZAM テスト実装
 * zam_pure.ml
 * pureな実装 その2
 * (実験資料準拠, Grab2無しバージョン)
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
  | ZamLet
  | EndLet

(* 値 *)
type value =
  | CLOSURE of inst list * env
  | MARK
  | INT of int
  | DUMMY

(* 環境 *)
and env = value list

(*========================================*
 * 命令
 *========================================*)

(* access 命令 *)
let access n = function
  | c, e, s, r ->
    (c, e, List.nth e n :: s, r)

(* closure 命令 *)
let closure c' = function
  | c, e, s, r ->
    let v = CLOSURE (c', e) in
    (c, e, v :: s, r)

(* tailapply 命令 *)
let tailapply = function
  | c, e, CLOSURE (c', e') :: v :: s, r ->
    (c', v :: CLOSURE (c', e') :: e', s, r)
  | _ ->
    failwith "tailapply"

(* apply 命令 *)
let apply = function
  | c, e, CLOSURE (c', e') :: v :: s, r ->
    (c', v :: CLOSURE (c', e') :: e', s, (c, e) :: r)
  | _ ->
    failwith "apply"

(* pushmark 命令 *)
let pushmark = function
  | c, e, s, r ->
    (c, e, MARK :: s, r)

(* grab 命令 *)
let grab = function
  | c, e, MARK :: s, (c', e') :: r ->
    let v = CLOSURE (c, e) in
    (c', e', v :: s, r)
  | c, e, v :: s, r ->
    (c, v :: CLOSURE (c, e) :: e, s, r)
  | _ ->
    failwith "grab"

(* return 命令 *)
let return = function
  | c, e, v :: MARK :: s, (c', e') :: r ->
    (c', e', v :: s, r)
  | c, e, CLOSURE (c', e') :: v :: s, r ->
    (c', v :: CLOSURE (c', e') :: e', s, r)
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

(* zemlet 命令 *)
let zamlet = function
  | c, e, v :: s, r ->
    (c, v :: e, s, r)
  | _ ->
    failwith "zamlet"

(* endlet 命令 *)
let endlet = function
  | c, _ :: e, s, r ->
    (c, e, s, r)
  | _ ->
    failwith "endlet"

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
        | ZamLet -> zamlet
        | EndLet -> endlet
      in
      loop (f (cs, e, s, r))
  in
  loop (code, [], [], [])

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
  | Fun (x, e) ->
    [Closure (comp_t (x :: "_" :: env) e)]
  | App es ->
    PushMark ::
    List.concat (List.rev_map (comp_c env) es) @
    [Apply]
  | If (e1, e2, e3) ->
    comp_c env e1 @ [IfThen (comp_c env e2, comp_c env e3)]
  | LetRec (f, Fun (x, e1), e2) ->
    Closure (comp_t (x :: f :: env) e1)
      :: ZamLet :: comp_c (f :: env) e2 @ [EndLet]
  | LetRec (f, _, _) -> failwith "not implemented"

and comp_t env = function
  | Fun (x, e) -> Grab :: comp_t (x :: "_" :: env) e
  | App es ->
    List.concat (List.rev_map (comp_c env) es) @
    [TailApply]
  | LetRec (f, Fun (x, e1), e2) ->
    Closure (comp_t (x :: f :: env) e1)
      :: ZamLet :: comp_t (f :: env) e2
  | LetRec (f, _, _) -> failwith "not implemented"
  | If (e1, e2, e3) ->
    comp_c env e1 @ [IfThen (comp_t env e2, comp_t env e3)]
  | e -> comp_c env e @ [Return]

(*========================================*
 * 実行
 *========================================*)

let run expr =
  let code = comp_c [] expr in
  let v = print_time (fun () -> execute code) in
  match v with
  | INT n -> Printf.printf "result = %d\n" n
  | CLOSURE _ -> print_endline "result = <closure>"
  | MARK -> print_endline "result = <mark>"
  | DUMMY -> print_endline "result = <dummy>"

let run_bench expr n =
  let code = comp_c [] expr in
  benchmark n (fun () -> execute code)

