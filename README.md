# zam-test
ZAM (Zinc Abstract Machine) の実験コード

以下の4パターンを実装
- 再帰で書いたインタプリタ (-eval_direct)
- 環境をリストで表したZAM (-zam_list)
- 環境を配列で表したZAM (-zam_array)
- 簡単な工夫で高速化したZAM (-zam_fast1)
- 再帰にrefを使わないZAM (-zam_pure)
- 講義資料に準拠したZAM (-zam_pure2)
- 
## 実行方法
- `make` でバイトコードコンパイル．
- `make nc` でネイティブコードコンパイル．
- `minicaml` オプション ファイル名 で実行．

例:
```
$ ./minicaml -eval_direct test/tarai.miniml 
rtime = 0.864892
result = 7
$ ./minicaml -zam_fast1 test/tarai.miniml 
rtime = 1.360640
result = 7
```
時間は評価にかかった実時間で，コンパイル時間は含まない．

例: n回実行した合計時間を返す．
```
$ ./minicaml -n 10 -eval_direct test/fib_cps.miniml 
rtime = 2.337510
$ ./minicaml -n 10 -zam_fast1 test/fib_cps.miniml 
rtime = 2.079954
```

## メモ
- バイトコードコンパイルでは，eval_direct < zam_fast1 < zam_list < zam_array の順に高速．
- 現時点で バイトコードコンパイルでeval_direct よりも高速な ZAM を作れていない．
- ネイティブコードコンパイルすると，zam_fast1 < zam_list < zam_array < eval_direct の順に高速になる．

## 言語
```
e ::= x                    変数
    | fun x -> e           関数抽象
    | e1 e2 ... en         関数適用
    | n                    整数
    | e + e                加算
    | e - e                減算
    | e * e                乗算
    | e < e                より小さい
    | e <= e               以下
    | if e then e else e   条件分岐
    | let rec x = e in e   再帰
```
コメントは `(* ... *)`．ifの条件は0以外の時真．
