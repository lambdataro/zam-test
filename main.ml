(*========================================*
 * ZAM テスト実装
 * main.ml
 *========================================*)

type mode_type =
  | EvalDirect
  | ZamList
  | ZamArray
  | ZamFast1
  | ZamPure
  | ZamPure2

let mode = ref ZamPure
let loop = ref 1

let run_file fname =
  let ich = open_in fname in
  let lexbuf = Lexing.from_channel ich in
  let expr = Parser.main Lexer.token lexbuf in
  if !loop = 1 then
    match !mode with
    | EvalDirect -> Eval_direct.run expr
    | ZamList -> Zam_list.run expr
    | ZamArray -> Zam_array.run expr
    | ZamFast1 -> Zam_fast1.run expr
    | ZamPure -> Zam_pure.run expr
    | ZamPure2 -> Zam_pure2.run expr
  else
    match !mode with
    | EvalDirect -> Eval_direct.run_bench expr !loop 
    | ZamList -> Zam_list.run_bench expr !loop 
    | ZamArray -> Zam_array.run_bench expr !loop 
    | ZamFast1 -> Zam_fast1.run_bench expr !loop
    | ZamPure -> Zam_pure.run_bench expr !loop
    | ZamPure2 -> Zam_pure2.run_bench expr !loop

let main () =
  Arg.parse [
    ("-eval_direct", Arg.Unit (fun () -> mode := EvalDirect), "eval_direct.ml を実行");
    ("-zam_list", Arg.Unit (fun () -> mode := ZamList), "zam_list.ml を実行");
    ("-zam_array", Arg.Unit (fun () -> mode := ZamArray), "zam_array.ml を実行");
    ("-zam_fast1", Arg.Unit (fun () -> mode := ZamFast1), "zam_fast1.ml を実行");
    ("-zam_pure", Arg.Unit (fun () -> mode := ZamPure), "zam_pure.ml を実行");
    ("-zam_pure2", Arg.Unit (fun () -> mode := ZamPure2), "zam_pure2.ml を実行");
    ("-n", Arg.Int (fun n -> loop := n), "繰り返し数")
  ] run_file "./mincaml option filename"

let () = main ()

