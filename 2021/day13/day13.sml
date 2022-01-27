(* Advent of Code 2021, Puzzle 25 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

val lines = readInput (TextIO.openIn "day13Input.txt")
val s1 = "fold along"
val (pos, neg) = List.partition (fn s2 => not (String.isPrefix s1 s2)) lines

(* delim1 : char -> bool *)
val delim1 = fn #"," => true | #"\n" => true | _ => false

(* mkPair1 : 'a list -> 'a * 'a *)
fun mkPair1 (x1 :: x2 :: xs) = (x1, x2)
  | mkPair1 _ = raise Fail "mkPair Error"

(* mkPoint : string * string -> int * int *)
fun mkPoint (xstr, ystr) =
  case (Int.fromString xstr, Int.fromString ystr) of
    (SOME x, SOME y) => (x, y)
  | _ => raise Fail "mkPoint Error"

(* points : (int * int) list *)
val points = List.map (mkPoint o mkPair1 o (String.tokens delim1)) pos

(* datatype fold constructors *)
datatype fold = AlongX of int | AlongY of int

(* delim2 : char -> bool *)
val delim2 = fn #" " => true | #"=" => true | #"\n" => true | _ => false

(* mkPair2 : 'a list -> 'a * 'a *)
fun mkPair2 L = (List.nth (L, 2), List.nth (L, 3))

(* mkFold : string * string -> fold *)
fun mkFold (axisStr, valueStr) =
  case (axisStr, Int.fromString (valueStr)) of
    ("x", SOME v) => AlongX v
  | ("y", SOME v) => AlongY v
  | _ => raise Fail "mkFold Error"

(* folds : fold list *)
val folds = List.map (mkFold o mkPair2 o (String.tokens delim2)) neg

(* applyFold : fold -> int * int -> int * int *)
fun applyFold (AlongX v) (x, y) = if x > v then (v - (x - v), y) else (x, y)
  | applyFold (AlongY v) (x, y) = if y > v then (x, v - (y - v)) else (x, y)

(* comparison function type 'a ord *)
type 'a ord = 'a * 'a -> order

(* split : 'a list -> 'a list * 'a list
 * REQUIRES: true
 * ENSURES: split L ==> (l,r) where l @ r is a permutation of L
 *)
fun split ([] : 'a list) : 'a list * 'a list = ([],[])
  | split [x] = ([x],[])
  | split (x::y::L) =
      let
        val (A,B) = split L
      in
        (x::A, y::B)
      end

(* merge : 'a ord * 'a list * 'a list -> 'a list
 * REQUIRES: cmp is an order function, l and r are sorted
 * ENSURES: merge (cmp,l,r) ==> L, where L is a cmp-sorted permutation of l @ r
 *)
fun merge (_ : 'a ord, [] : 'a list, L : 'a list) : 'a list = L
  | merge (_, L, []) = L
  | merge (cmp, x::xs, y::ys) =
      case cmp (x, y) of
        GREATER => y :: merge (cmp, x::xs, ys)
      | _ => x :: merge (cmp, xs, y::ys)

(* msort : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: msort (cmp, L) ==> L', where L' is a cmp-sorted permutation of L
 *)
fun msort (_ : 'a ord, [] : 'a list) : 'a list = []
  | msort (_, [x]) = [x]
  | msort (cmp, L)  =
      let
        val (A,B) = split L
      in
        merge (cmp, msort (cmp, A), msort (cmp, B))
      end

(* doop : 'a ord -> 'a * 'a list -> 'a list
 * REQUIRES: x be the next element in the reverse traversal of a sorted list S
 * ENSURES: foldr (doop cmp) [] S ==> U, where U has the same elements as sorted
 * list S, with duplicates removed, and U remains sorted
 *)
fun doop (cmp : 'a ord) (x : 'a, [] : 'a list) = x :: []
  | doop cmp (x, y :: ys) =
    case cmp (x,y) of
        EQUAL => y :: ys
      | _ => x :: y :: ys

(* fastDoop : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: fastDoop (cmp,L) ==> L', where L' is L with cmp-duplicates removed
 *)
fun fastDoop (cmp : 'a ord, L : 'a list) : 'a list =
  foldr (doop cmp) [] (msort (cmp, L))

(* cmp (int * int) * (int * int) -> order *)
fun cmp ((x1, y1), (x2, y2)) =
  case Int.compare (x1, x2) of
    EQUAL => Int.compare (y1, y2)
  | v => v

val fold1 = List.hd folds
val points1 = List.map (applyFold fold1) points
val soln25 = List.length (fastDoop (cmp, points1))

(* Advent of Code 2021, Puzzle 26 *)
val folded = List.foldl (fn (f,acc) => List.map (applyFold f) acc) points folds
val origami = List.map (fn (x, y) => (y, x)) (fastDoop (cmp, folded))

fun f ((i,j), (imax, jmax)) =
  case (i > imax, j > jmax) of
    (false, false) => (imax, jmax)
  | (false, true ) => (imax, j   )
  | (true , false) => (i   , jmax)
  | (true , true ) => (i   , j   )

val (imax, jmax) = List.foldl f (0,0) origami
val (m, n) = (imax + 1, jmax + 1)

(* update : 'a list * int * int * 'a -> 'a list *)
fun update (arr, i, j, a) =
  let val idx = n * i + j
  in List.take (arr, idx) @ (a :: List.drop (arr, idx + 1))
  end

(* plot : (int * int) * string list -> string list *)
fun plot ((i, j), arr) = update (arr, i, j, "#")

(* enum : 'a list -> (int * 'a) list *)
fun enum L = ListPair.zip (List.tabulate (List.length L, fn x => x), L)

(* toIndex : int -> int * int *)
fun toIndex k = (k div n, k mod n)

(* disp : (int * int) list -> unit *)
fun disp ps =
  let
    val row = List.tabulate (n, fn i => " ")
    val arr = List.concat (List.tabulate (m, fn i => row))
    val arr' = enum (List.foldl plot arr ps)

    fun impl [] = ""
      | impl ((k, a) :: xs) =
        let val (i, j) = toIndex k
        in  case Int.compare (j, jmax) of
              EQUAL => a ^ "\n" ^ impl xs
            | _ => a ^ impl xs
        end
  in
    print (impl arr' ^ "\n")
  end
