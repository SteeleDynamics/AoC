(* Advent of Code 2021, Puzzle 3 *)
datatype cmd = Nil | Down of int | Forward of int | Up of int

fun readInput (inStream, acc) =
  case TextIO.inputLine inStream of
    SOME str => str :: (readInput (inStream, acc))
  | NONE => acc

fun mkCmd (dstr::nstr::[]) = (
    case (dstr, Int.fromString nstr) of
      ("down", SOME n) => Down n
    | ("forward", SOME n) => Forward n
    | ("up", SOME n) => Up n
    | (_, _) => Nil)
  | mkCmd _ = Nil

fun g (Down n, (r, d)) = (r, d + n)
  | g (Forward n, (r, d)) = (r + n, d)
  | g (Up n, (r, d)) = (r, d - n)
  | g (Nil, (r, d)) = (r, d)

val tokens =
  List.map
    (String.tokens Char.isSpace)
    (readInput (TextIO.openIn "day2Input.txt", []))
val cmds = List.map mkCmd tokens
val (r3, d3) = List.foldr g (0,0) cmds
val soln3 = r3 * d3

(* Advent of Code 2021, Puzzle 4 *)
fun g' (Down n, (r, d, a)) = (r, d, a + n)
  | g' (Forward n, (r, d, a)) = (r + n, d + (a * n), a)
  | g' (Up n, (r, d, a)) = (r, d, a - n)
  | g' (Nil, (r, d, a)) = (r, d, a)

val (r4, d4, a4) = List.foldl g' (0, 0, 0) cmds
val soln4 = r4 * d4
