(* Advent of Code 2021, Puzzle 21 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

(* charToInt : char -> int *)
fun charToInt c = Char.ord c - Char.ord #"0"

(* sub : 'a list * int * int -> 'a *)
fun sub (arr, i, j) = List.nth (arr, 10 * i + j)

(* update : 'a list * int * int * 'a -> 'a list *)
fun update (arr, i, j, a) =
  let val idx = 10 * i + j
  in List.take (arr, idx) @ (a :: List.drop (arr, idx + 1))
  end

(* enum : 'a list -> (int * 'a) list *)
fun enum L = ListPair.zip (List.tabulate (List.length L, fn x => x), L)

(* toIndex : int -> int * int *)
fun toIndex k = (k div 10, k mod 10)

(* disp : int list -> unit *)
fun disp arr =
  let
    val arr' = enum arr
    fun impl [] = ""
      | impl ((k, a) :: xs) =
        case toIndex k of
          (_, 9) => Int.toString a ^ "\n" ^ impl xs
        | _ => Int.toString a ^ impl xs
  in
    print (impl arr' ^ "\n")
  end

(* incr : int list -> int list *)
fun incr arr = List.map (fn x => x + 1) arr

(* ? : int * int -> order *)
infix ?
fun x ? y = Int.compare (x, y)

(* neighbors : int * int -> (int * int) list *)
fun neighbors (i, j) =
  let
    val ns = [(i-1,j-1), (i-1,j), (i-1,j+1),
              (i  ,j-1),          (i  ,j+1),
              (i+1,j-1), (i+1,j), (i+1,j+1)]
    fun pred (x, y) =
      case (x ? 0, x ? 9, y ? 0, y ? 9) of
        (LESS, _, _, _) => false
      | (_, GREATER, _, _) => false
      | (_, _, LESS, _) => false
      | (_, _, _, GREATER) => false
      | _ => true
  in
    List.filter pred ns
  end

(* fst : 'a * 'b -> 'a *)
fun fst (x, y) = x

(* flash : int list * int list -> int list * int list *)
fun flash (arr, fd) =
  let
    val f = fn (k, aij) => aij > 9 andalso not (List.exists (fn x => x = k) fd)
    val ks = List.filter f (enum arr)
    val g = fn ((i, j), acc) => update (acc, i, j, sub (acc, i, j) + 1)
    val h = fn ((k, aij), acc) => List.foldl g acc (neighbors (toIndex k))
  in
    if List.length ks > 0
    then flash (List.foldl h arr ks, fd @ (List.map fst ks))
    else (arr, fd)
  end

(* step : int list -> int list * int *)
fun step arr =
  let val (arr', fd') =  flash (incr arr, [])
  in (List.map (fn x => if x > 9 then 0 else x) arr', List.length fd')
  end

(* iterate : int list * int * int -> int *)
fun iterate (arr, 0, acc) = acc
  | iterate (arr, n, acc) =
  let val (arr', f) = step arr
  in iterate (arr', n - 1, acc + f)
  end

val arr = List.foldr (op @) []
  (List.map
    ((List.map charToInt) o (List.filter Char.isDigit) o String.explode)
    (readInput (TextIO.openIn "day11Input.txt")))

val soln21 = iterate (arr, 100, 0)

(* Advent of Code 2021, Puzzle 22 *)
fun iterate' (arr, acc) =
  let val (arr', f) = step arr
  in if f = 100 then acc else iterate' (arr', acc + 1)
  end

val soln22 = iterate' (arr, 1)
