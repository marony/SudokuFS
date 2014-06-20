// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

let MAX_NUMBER = 9;
let COLUMNS = 9;
let ROWS = 9;

/// 初期盤面
//let initialBoard =
//  [ 5; 3; 0; 0; 7; 0; 0; 0; 0;
//    6; 0; 0; 1; 9; 5; 0; 0; 0;
//    0; 9; 8; 0; 0; 0; 0; 6; 0;
//    8; 0; 0; 0; 6; 0; 0; 0; 3;
//    4; 0; 0; 8; 0; 3; 0; 0; 1;
//    7; 0; 0; 0; 2; 0; 0; 0; 6;
//    0; 6; 0; 0; 0; 0; 2; 8; 0;
//    0; 0; 0; 4; 1; 9; 0; 0; 5;
//    0; 0; 0; 0; 8; 0; 0; 7; 9 ]
//let initialBoard =
//  [ 9; 0; 0; 0; 0; 8; 0; 0; 4;
//    0; 1; 0; 0; 5; 0; 0; 2; 0;
//    0; 0; 3; 0; 0; 0; 7; 0; 0;
//    0; 0; 0; 2; 0; 9; 0; 0; 3;
//    0; 6; 0; 0; 0; 0; 0; 8; 0;
//    5; 0; 0; 7; 0; 4; 0; 0; 0;
//    0; 0; 8; 0; 0; 0; 1; 0; 0;
//    0; 2; 0; 0; 4; 0; 0; 6; 0;
//    7; 0; 0; 3; 0; 0; 0; 0; 5 ]
let initialBoard =
    [ 0; 1; 0; 0; 9; 0; 0; 0; 0 
      5; 8; 2; 0; 0; 3; 0; 0; 4 
      0; 0; 0; 2; 0; 0; 3; 0; 0 
      0; 0; 0; 0; 0; 1; 0; 7; 0 
      3; 7; 8; 0; 2; 0; 5; 0; 0 
      0; 0; 4; 0; 0; 0; 0; 0; 2 
      0; 0; 1; 3; 8; 0; 0; 0; 6 
      7; 6; 9; 0; 0; 0; 0; 0; 0 
      0; 3; 0; 0; 4; 0; 0; 2; 7 ]

/// 全マスの座標リスト
/// (0, 0); (1, 0); .. (8, 8)
let xys = Seq.init (COLUMNS * ROWS) (fun index -> (index % ROWS, (index / COLUMNS) % COLUMNS))

/// 座標からインデックス
let indexFromPos x y =
  y * COLUMNS + x

let printfn' = printfn

/// 盤面を表示
let print board =
  // TODO: もっと綺麗にならない？
  let print' board =
    let n1 = if ((Seq.nth 0 board) = 0) then "." else (Seq.nth 0 board).ToString()
    let n2 = if ((Seq.nth 1 board) = 0) then "." else (Seq.nth 1 board).ToString()
    let n3 = if ((Seq.nth 2 board) = 0) then "." else (Seq.nth 2 board).ToString()
    printf "%s %s %s|" n1 n2 n3
    board |> Seq.skip 3
  let print'' board =
    printf "|"
    let r = board |> print' |> print' |> print'
    printf "\n|"
    let r = r |> print' |> print' |> print'
    printf "\n|"
    let r = r |> print' |> print' |> print'
    printfn' "\n-------------------"
    r
  printfn' "-------------------"
  board |> print'' |> print'' |> print'' |> ignore

/// ゲームが終わったかどうか
let isFinish (board : int list) =
  List.forall (fun piece -> piece <> 0) board // 0のマスがなければ終わっている

/// 同じ3x3の中にあるマスを返す
let getSameBoxPieces x y (board : int list) =
  // 3x3ますの左上の座標
  let x' = x /3 * 3
  let y' = y / 3 * 3
  // 左上の座標に下記を足すと3x3全マス
  let xs = [0; 1; 2; 9; 10; 11; 18; 19; 20]
  let index = indexFromPos x' y'
  [ for i in xs -> List.nth board (index + i) ]

/// 同じ列のマスを返す
let getSameColumnPieces x y (board : int list) =
  let xs = [ for i in [0..8] -> i * 9 ]
  [ for i in xs -> List.nth board (x + i) ]

/// 同じ行のマスを返す
let getSameRowPieces x y (board : int list) =
  board
  |> Seq.skip (ROWS * y)  // y座標分行を飛ばす
  |> Seq.take COLUMNS     // 9個取る
  |> Seq.toList

/// マスごとにインデックスを付加する
let indexedBoard (board : int list) =
  board
  |> List.zip (Seq.toList xys)  // 座標のリストとマスのリストをくっつける
  |> List.map (fun ((x, y), piece) -> (x, y, piece))  // ((x, y), p)から(x, y, p)に変換

/// ゲームが不正な状態かどうか
let isInvalid board =
  let board = board
              |> indexedBoard
              |> List.filter (fun (x, y, piece) -> piece <> 0)
  // 3x3関数・縦方向関数・横方向関数のリスト
  let fs = [(fun (x, y, piece) -> (x / 3 + 100, y / 3 + 100, piece)); // 同じ3x3マス
            (fun (x, y, piece) -> (x, -1, piece));                    // 同じ列
            (fun (x, y, piece) -> (-1, y, piece))]                    // 同じ行
  List.exists (fun f -> board
                        |> Seq.groupBy f                                  // 同じ3x3マス・列・行でグループ化
                        |> Seq.filter (fun (_, ys) -> Seq.length ys <> 1) // グループの中に同じ数字が複数あったらおかしい
                        |> Seq.length
                        |> (<) 0) fs                                      // ひとつでも存在したらinvalid

/// 置ける数値のリスト
let safetyNumbers x y board =
  // マス取得関数のリスト
  let fs = [getSameBoxPieces; getSameColumnPieces; getSameRowPieces]
  // 全部の一覧を1..9から引く
  let rs = List.map (fun f -> f x y board) fs // その座標の3x3マス・列・行を全部取得
           |> List.concat
           |> List.filter (fun r' ->r' <> 0)  // まだ決まってないマスはいらない
           |> Seq.distinct                    // 同じ数値は削除
           |> Seq.toList
  Set.difference (Set.ofList [1..9]) (Set.ofList rs) // [1..9]から上記リストを引く
  |> Set.toList

/// あるマスの数値を置き換える
let replacePiece x y n board =
  // その座標より前のリストとその座標より後のリストの間に数値を入れる
  let index = indexFromPos x y
  let prev = (board |> Seq.take index |> Seq.toList)
  let post = (board |> Seq.skip index |> Seq.toList |> List.tail)
  prev @ (n :: post)

/// 単純ロジック
let checkBoard (board : int list) =
  // List.fold用関数
  let rec checkBoard' (board, acc) ((x, y, piece) as p) =
    print board
    if piece <> 0 then
      // 既に数値が決定している
      (board, p :: acc)
    else
      // 可能性のある数値のリスト
      let rs = safetyNumbers x y board
      if List.length rs = 1 then
        // 1個しかなければ決定
        let n = List.head rs
        let board' = replacePiece x y n board
        (board', (x, y, n) :: acc)
      else
        // 候補が絞り切れない
        (board, p :: acc)
  // 再帰用関数
  let rec checkBoard'' board =
    print board
    let board' = board
                  |> indexedBoard
                  |> List.fold checkBoard' (board, [])
                  |> (fun (board, xs) -> xs)
                  |> List.map (fun (x, y, piece) -> piece)
                  |> List.rev
    // 変化がなくなるまで繰り返す
    if (board = board') then
      board
    else // 多分一度で決まらないこともあると予想
      checkBoard'' board'
  print board
  checkBoard'' board

/// 総当たり
let rec searchBoard (board : int list) =
  // 座標ひとつひとつについて、searchBoard''を呼ぶ
  let rec searchBoard' nest (b, board) xs =
    print board
    match xs with
    | (x, y) :: xss ->
      // 座標から候補リストを取得
      let ns = safetyNumbers x y board
      match ns with
      // 候補がない
      | [] -> (b, board)
      | _ ->
        // 候補があった
        let b', board' = searchBoard'' (nest + 1) x y (b, board) ns
        if (b') then (b', board') else (b', board)
    | [] ->
      // 座標がなくなった
      (b, board)
  // 座標に数値のリストを全部入れてみる
  and searchBoard'' nest x y (b, board) ns =
    print board
    match ns with
    | n :: nss ->
      // 座標に数値を入れてみる
      let index = indexFromPos x y
      let board' = replacePiece x y n board
      if (isInvalid board') then
        // 入れられない
        printfn' "Invalid"
        print board'
        let b', board' = searchBoard'' nest x y (b, board) nss
        if (b') then (b', board') else (b', board)
      elif (isFinish board') then
        // 完成！！
        printfn' "Finish"
        print board'
        (true, board')
      else
        // 入ったので次の座標で試す
        let xys' = Seq.filter (fun (x, y) -> (List.nth board' (y * COLUMNS + x)) = 0) xys |> Seq.toList
        let b', board'' = searchBoard' (nest + 1) (b, board') xys'
        if (b') then
          (b', board'')
        else
          // 同じ座標で次の数値を試す
          let b'', board''' = searchBoard'' nest x y (b, board) nss
          if (b'') then (b'', board''') else (b', board)
    | [] ->
      // 数値がなくなった
      (b, board)
  // 開始
  print board
  let xys' = Seq.filter (fun (x, y) -> (List.nth board (y * COLUMNS + x)) = 0) xys |> Seq.toList
  let b, board' = searchBoard' 0 (false, board) xys'
  board'

[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    // 単純ロジックのあと、残りは総当たりで
    printfn' "%A" (searchBoard (checkBoard initialBoard))

    printfn' "[END] : %A" sw.ElapsedMilliseconds
    System.Console.ReadKey()
    0 // 整数の終了コードを返します
