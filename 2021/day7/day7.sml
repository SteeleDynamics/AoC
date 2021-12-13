(* Read input file *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

fun isDelim #"," = true
  | isDelim _ = false

fun rmOpt (SOME n) = n
  | rmOpt (NONE) = raise Fail "rmOpt Error"

val inputStr = List.hd (readInput (TextIO.openIn "day7Input.txt"))
val xs = List.map (rmOpt o Int.fromString) (String.tokens isDelim inputStr)

(* Advent of Code 2021, Puzzle 13 ==> find median *)

(* split : int list -> int list * int list
 * REQUIRES: true
 * ENSURES: split L evaluates to (A,B) where A and B differ in length by at most
 *          one, and A @ B is a permutation of L
 *)
fun split ([] : int list) : int list * int list = ([], [])
  | split [x] = ([x], [])
  | split (x::x'::xs) =
    let val (A, B) = split xs
    in (x::A, x'::B)
    end

(* merge : int list * int list -> int list
 * REQUIRES: A and B are sorted
 * ENSURES: merge (A,B) evaluates to a sorted permutation of A @ B
 *)
fun merge (L1 : int list, [] : int list) : int list = L1
  | merge ([], L2) = L2
  | merge (x::xs, y::ys) =
     (case Int.compare(x,y) of
        GREATER => y :: merge (x::xs, ys)
      | _ => x :: merge (xs, y::ys))

(* msort : int list -> int list
 * REQUIRES: true
 * ENSURES: msort ( L ) evaluates to a sorted permutation of L
 *)
fun msort([] : int list) : int list = []
  | msort [x] = [x]
  | msort L =
    let val (A,B) = split L
    in merge(msort A, msort B)
    end

fun median ([] : int list) : int = raise Fail "median EmptyListErr"
  | median L =
    let
      val n = List.length L
      val S = msort L
      val x1 = List.nth (S, n div 2 - 1)    (* zero-based index *)
      val x2 = List.nth (S, n div 2)
    in
      if (n mod 2) = 0
      then (x1 + x2) div 2
      else x2
    end

val xTilde = median xs
val soln13 = List.foldl (op +) 0 (List.map (fn x => Int.abs (x - xTilde)) xs)

(* Advent of Code 2021, Puzzle 14 ==> find mean *)
fun mean ([] : int list) : int = raise Fail "mean EmptyListErr"
  | mean L = (List.foldl (op +) 0 L) div (List.length L)

fun gauss n = n * (n + 1) div 2
fun fuel xBar x = gauss (Int.abs (x - xBar))

val xBar = mean xs
val soln14 = List.foldl (op +) 0 (List.map (fuel xBar) xs)
