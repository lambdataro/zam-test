(*========================================*
 * ZAM テスト実装
 * lexer.mll
 *========================================*)

{
open Syntax
open Parser
open Lexing
}

let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z''_']
let id = alpha (digit | alpha | '\'')*

rule token = parse
  (* 記号 *)
  | '+'       { PLS }
  | '-'       { MNS }
  | '*'       { AST }
  | "<="      { LE }
  | '<'       { LT }
  | '='       { EQ }
  | "->"      { ARROW }
  | '('       { LP }
  | ')'       { RP }
  
  (* キーワード *)
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "fun"     { FUN }
  
  (* コメント *)
  | "(*"          { comment lexbuf; token lexbuf }
  
  (* リテラル *)
  | digit+ as str { INT (int_of_string str) }
  | id as str     { ID str }
  
  (* その他 *)
  | [' ''\t']+    { token lexbuf }
  | '\n'          { new_line lexbuf; token lexbuf }
  | eof           { EOF }
  
and comment = parse
  | "*)"            { () }
  | "(*"            { comment lexbuf; comment lexbuf }
  | '\n'            { new_line lexbuf; comment lexbuf }
  | _               { comment lexbuf }

