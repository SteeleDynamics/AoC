(* Advent of Code 2021, Puzzle 11 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

fun isDelim #"," = true
  | isDelim _ = false

fun rmOpt (SOME n) = n
  | rmOpt (NONE) = raise Fail "rmOpt Error"

fun shl arr = (List.tl arr) @ [0]

fun sub (arr, i) = List.nth (arr, i)

fun update (arr, i, x) = List.take (arr, i) @ (x :: (List.drop (arr, i + 1)))

fun addi (arr, i, n) = update (arr, i, sub (arr, i) + n)

fun hist ([], pop) = pop
  | hist (x::xs, pop) = hist (xs, addi (pop, x, 1))

fun iterate pop =
  let val z = List.hd pop
  in addi (addi (shl pop, 6, z), 8, z)
  end

fun simulate (pop, 0) = pop
  | simulate (pop, n) = simulate (iterate pop, n - 1)

val inputStr = List.hd (readInput (TextIO.openIn "day6Input.txt"))
val xs = List.map (rmOpt o Int.fromString) (String.tokens isDelim inputStr)
val pop = hist (xs, List.tabulate (9, fn i => 0))
val pop80 = simulate (pop, 80)
val soln11 = List.foldl (op +) 0 pop80;

(* Advent of Code 2021, Puzzle 12 *)
val pop256 = simulate (pop, 256)
val soln12 = List.foldl (op +) 0 pop256
