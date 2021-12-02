(* Advent of Code 2021, Puzzle 1 *)
fun readInput (inStream, acc) =
  case TextIO.inputLine inStream of
    SOME str => str :: (readInput (inStream, acc))
  | NONE => acc

val f = fn SOME n => n | NONE => 0

fun prevCmp (x1::x2::xs) = Int.compare (x2, x1) :: prevCmp (x2::xs)
  | prevCmp _ = []

val g = fn (x,z) =>
  case x of
    GREATER => z + 1
  | (LESS | EQUAL) => z

val input' = List.map
  Int.fromString
  (readInput (TextIO.openIn "day1Input.txt", []))
val input = List.map f input'
val cmps1 = prevCmp input
val soln1 = List.foldl g 0 cmps1

(* Advent of Code 2021, Puzzle 2 *)
fun windowSum (x1::x2::x3::xs) = (x1+x2+x3) :: windowSum (x2::x3::xs)
  | windowSum _ = []

val sums = windowSum input
val cmps2 = prevCmp sums
val soln2 = List.foldl g 0 cmps2
