(* Advent of Code 2021, Puzzle 9 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

fun isDelim #"," = true
  | isDelim c = Char.isSpace c

fun mkPoint (xstr, ystr) =
  case (Int.fromString xstr, Int.fromString ystr) of
    (SOME x, SOME y) => SOME (x, y)
  | (_     , _     ) => NONE

fun mkLine [x1str, y1str, "->", x2str, y2str] = (
    case (mkPoint (x1str, y1str), mkPoint (x2str, y2str)) of
      (SOME (x1, y1), SOME (x2, y2)) => ((x1, y1), (x2, y2))
    | (_            , _            ) => raise Fail "mkLine - PointFmtErr")
  | mkLine _ = raise Fail "mkLine - LineFmtErr"

fun isHorz ((x1, y1), (x2, y2)) =
  case Int.compare (y1, y2) of
    EQUAL => true
  | (LESS | GREATER) => false

fun isVert ((x1, y1), (x2, y2)) =
  case Int.compare (x1, x2) of
    EQUAL => true
  | (LESS | GREATER) => false

fun genPoints ((x1, y1), (x2, y2)) =
  case (Int.compare (x1, x2), Int.compare (y1, y2)) of
    (EQUAL  , EQUAL  ) => [(x1, y1)]
  | (LESS   , EQUAL  ) => (x1, y1) :: genPoints ((x1 + 1, y1), (x2, y2))
  | (GREATER, EQUAL  ) => (x1, y1) :: genPoints ((x1 - 1, y1), (x2, y2))
  | (EQUAL  , LESS   ) => (x1, y1) :: genPoints ((x1, y1 + 1), (x2, y2))
  | (EQUAL  , GREATER) => (x1, y1) :: genPoints ((x1, y1 - 1), (x2, y2))
  | (LESS   , LESS   ) => (x1, y1) :: genPoints ((x1 + 1, y1 + 1), (x2,y2))
  | (LESS   , GREATER) => (x1, y1) :: genPoints ((x1 + 1, y1 - 1), (x2,y2))
  | (GREATER, LESS   ) => (x1, y1) :: genPoints ((x1 - 1, y1 + 1), (x2,y2))
  | (GREATER, GREATER) => (x1, y1) :: genPoints ((x1 - 1, y1 - 1), (x2,y2))

fun gatherPoints [] = []
  | gatherPoints (l::ls) = genPoints l @ gatherPoints ls

local
  val tbl = Array2.array (1000, 1000, 0)
in
  fun markTbl [] = ()
    | markTbl ((i, j) :: pts) =
      let val v' = Array2.sub (tbl, i, j) + 1
      in (Array2.update (tbl, i, j, v'); markTbl pts)
      end

  fun foldTbl f acc = Array2.fold Array2.RowMajor f acc tbl

  fun resetTbl () = Array2.modify Array2.RowMajor (fn x => 0) tbl
end

val tokens =
  List.map
    (String.tokens isDelim)
    (readInput (TextIO.openIn "day5Input.txt"))
val lines = List.map mkLine tokens
val points = (gatherPoints (List.filter isHorz lines)) @
             (gatherPoints (List.filter isVert lines))
val () = markTbl points
val f = fn (x, acc) => if x > 1 then acc + 1 else acc
val soln9 = foldTbl f 0

(* Advent of Code 2021, Puzzle 10 *)
val () = resetTbl ()
val points' = gatherPoints lines
val () = markTbl points'
val soln10 = foldTbl f 0
