(* Advent of Code 2021, Puzzle 19 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

datatype token = LeftRound  | RightRound
               | LeftSquare | RightSquare
               | LeftCurly  | RightCurly
               | LeftAngle  | RightAngle

datatype SyntaxError = Found of token | Incomplete of token list

fun lex #"(" = LeftRound
  | lex #")" = RightRound
  | lex #"[" = LeftSquare
  | lex #"]" = RightSquare
  | lex #"{" = LeftCurly
  | lex #"}" = RightCurly
  | lex #"<" = LeftAngle
  | lex #">" = RightAngle
  | lex _ = raise Fail "lex Error"

fun parse' ([], stack) = Incomplete stack
  | parse' (LeftRound :: input, stack) = parse' (input, LeftRound :: stack)
  | parse' (LeftSquare :: input, stack) = parse' (input, LeftSquare :: stack)
  | parse' (LeftCurly :: input, stack) = parse' (input, LeftCurly :: stack)
  | parse' (LeftAngle :: input, stack) = parse' (input, LeftAngle :: stack)
  | parse' (RightRound :: input, LeftRound :: stack) = parse' (input, stack)
  | parse' (RightSquare :: input, LeftSquare :: stack) = parse' (input, stack)
  | parse' (RightCurly :: input, LeftCurly :: stack) = parse' (input, stack)
  | parse' (RightAngle :: input, LeftAngle :: stack) = parse' (input, stack)
  | parse' (RightRound :: input, _) = Found RightRound
  | parse' (RightSquare :: input, _) = Found RightSquare
  | parse' (RightCurly :: input, _) = Found RightCurly
  | parse' (RightAngle :: input, _) = Found RightAngle

fun parse input = parse' (input, [])

fun syntaxScore (Incomplete stack) = 0
  | syntaxScore (Found RightRound) = 3
  | syntaxScore (Found RightSquare) = 57
  | syntaxScore (Found RightCurly) = 1197
  | syntaxScore (Found RightAngle) = 25137
  | syntaxScore (Found _) = 0

val lines = List.map
              ((List.filter Char.isGraph) o String.explode)
              (readInput (TextIO.openIn "day10Input.txt"))

val scores =  List.map
                (syntaxScore o parse o (List.map lex))
                lines

val soln19 = List.foldl (op +) 0 scores

(* Advent of Code 2021, Puzzle 20 *)
fun autoTok LeftRound = RightRound
  | autoTok LeftSquare = RightSquare
  | autoTok LeftCurly = RightCurly
  | autoTok LeftAngle = RightAngle
  | autoTok _ = raise Fail "autoTok Error"

fun mkAuto stack = List.foldr (fn (x, acc) => autoTok x :: acc) [] stack

fun autoScore (RightRound, acc) = 5 * acc + 1
  | autoScore (RightSquare, acc) = 5 * acc + 2
  | autoScore (RightCurly, acc) = 5 * acc + 3
  | autoScore (RightAngle, acc) = 5 * acc + 4
  | autoScore _ = raise Fail "autoScore Error"

fun autoCompScore (Incomplete stack) = List.foldl autoScore 0 (mkAuto stack)
  | autoCompScore _ = 0

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

val scores' = List.filter
                (fn x => x <> 0)
                  (List.map
                    (autoCompScore o parse o (List.map lex))
                    lines)

val soln20 = List.nth (msort scores', List.length scores' div 2)
