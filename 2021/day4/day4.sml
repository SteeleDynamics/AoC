(* Advent of Code 2021, Puzzle 7 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

fun isDelim #"\n" = true
  | isDelim #"," = true
  | isDelim _ = false 

fun mkDraw tok =
  case Int.fromString tok of
    SOME n => n
  | NONE => raise Fail "mkDraw Error"

fun mkNum tok =
  case Int.fromString tok of
    SOME n => (n, false)
  | NONE => raise Fail "mkNum Error"

fun mkNumRow rstr = List.map mkNum (String.tokens Char.isSpace rstr)

fun mkBoards boardsStr =
  List.foldr 
    (fn (r, acc1) => 
      List.foldr 
        (fn (c, acc2) => c :: acc2)
        acc1
        (mkNumRow r)) 
    []
    boardsStr

fun markNum ([], draw) = []
  | markNum ((n, m) :: ns, draw) =
  case Int.compare (n, draw) of
    EQUAL => (n, true) :: markNum (ns, draw)
  | (LESS | GREATER) => (n, m) :: markNum (ns, draw)

fun nthBoard (boards, i) = List.take (List.drop (boards, 25 * i), 25)

fun trBoard board =
  List.tabulate (25, fn i => List.nth (board, 5 * (i mod 5) + (i div 5)))

fun chkBoardRows ((n0,m0)::(n1,m1)::(n2,m2)::(n3,m3)::(n4,m4)::ns) =
  (m0 andalso m1 andalso m2 andalso m3 andalso m4) orelse (chkBoardRows ns)
  | chkBoardRows _ = false

fun chkBoardCols board = chkBoardRows (trBoard board)

fun chkBoard boards i =
  let
    val board = nthBoard (boards, i)
  in
    if chkBoardRows board orelse chkBoardCols board
    then SOME (i, board)
    else NONE
  end

fun playBingo ([], boards, acc) = acc
  | playBingo (draw :: draws, boards, acc) =
  let
    val boards' = markNum (boards, draw)
    val results = List.tabulate (List.length boards' div 25, chkBoard boards')
    val results' = 
      List.filter
        (fn (SOME (i, board)) => not (List.exists (fn (j, score) => i = j) acc)
          | NONE => false)
        results
    val g = fn ((n, true), acc') => acc'
            | ((n, false), acc') => n + acc'
    val f = fn (SOME (i, board)) => (i, (List.foldl g 0 board) * draw)
            | NONE => raise Fail "f Error"
  in
    playBingo (draws, boards', acc @ (List.map f results'))
  end

val drawStr::boardsStr = readInput (TextIO.openIn "day4Input.txt")
val draws = List.map mkDraw (String.tokens isDelim drawStr)
val boards = mkBoards boardsStr
val rank = playBingo (draws, boards, [])
val soln7 = List.hd rank

(* Advent of Code 2021, Puzzle 8 *)
val soln8 = List.nth (rank, List.length rank - 1)
