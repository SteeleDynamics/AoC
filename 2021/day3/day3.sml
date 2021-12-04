(* Advent of Code 2021, Puzzle 5 *)
fun readInput (inStream) =
  case TextIO.inputLine inStream of
    SOME str => str :: (readInput (inStream))
  | NONE => []

fun strToBin str =
  List.map
    (fn c => Char.ord c - Char.ord #"0")
    (List.filter Char.isDigit (String.explode str))

fun transpose M =
  let
    val m = List.length M
    val n = List.length (List.hd M)
    val index = fn (i, j) => List.nth (List.nth (M, i), j)
  in
    List.tabulate (n, fn j => List.tabulate (m, fn i => index (i, j)))
  end

fun maj k a =
  case Int.compare (a, k div 2 + k mod 2) of
    (GREATER | EQUAL) => 1
  | LESS => 0

fun flip 0 = 1
  | flip 1 = 0
  | flip x = x

fun popCount L = List.foldl (op +) 0 L

fun toInt bs = List.foldl (fn (b, acc) => b + 2 * acc) 0 bs

val ws = List.map strToBin (readInput (TextIO.openIn "day3Input.txt"))
val wsT = transpose ws
val k = List.length ws
val gammaBits = List.map (maj k) (List.map popCount wsT)
val epsilonBits = List.map flip gammaBits
val gamma = toInt gammaBits
val epsilon = toInt epsilonBits
val soln5 = gamma * epsilon

(* Advent of Code 2021, Puzzle 6 *)
fun recurMajMin (ws, j, isMaj) =
  let
    val rowj = List.nth (transpose ws, j)
    val popj = popCount rowj
    val k = List.length rowj
    val major = maj k popj
    val majMin = if isMaj then major else flip major
  in
    case List.filter (fn w => List.nth (w,j) = majMin) ws of
      [] => []
    | [x] => x
    | xs => recurMajMin (xs, j + 1, isMaj)
  end

val oxyBits = recurMajMin (ws, 0, true)
val co2Bits = recurMajMin (ws, 0, false)
val oxy = toInt oxyBits
val co2 = toInt co2Bits
val soln6 = oxy * co2
