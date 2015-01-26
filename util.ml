(*========================================*
 * ZAM テスト実装
 * util.ml
 *========================================*)

(* 時間計測する *)
let print_time f =
  let t1 = Unix.gettimeofday () in
  let v = f () in
  let t2 = Unix.gettimeofday () in
  Printf.printf "rtime = %f\n" (t2 -. t1);
  v

(* n回実行する *)
let benchmark n f =
  let t1 = Unix.gettimeofday () in
  for i = 1 to n do
    f ()
  done;
  let t2 = Unix.gettimeofday () in
  Printf.printf "rtime = %f\n" (t2 -. t1)

(* リストのインデックスを求める *)
let rec index x = function
  | [] -> failwith "index"
  | y :: _ when x = y -> 0
  | _ :: rest -> 1 + index x rest

(* concat . rev . map *)
let concat_rev_map f es =
  List.concat (List.rev_map f es)

