(*========================================*
 * ZAM テスト実装
 * syntax.ml
 *========================================*)

type id = string

type binop =
  | Add
  | Sub
  | Mul
  | Lt
  | Le

type expr =
  | Int of int
  | BinApp of binop * expr * expr
  | Var of id
  | Fun of id * expr
  | App of expr list
  | If of expr * expr * expr
  | LetRec of id * expr * expr

