(* Advent of Code 2021, Puzzle 17 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

(* charToInt : char -> int *)
fun charToInt c = Char.ord c - Char.ord #"0"

(* getTop : 'a Array2.array * int * int -> 'a option *)
fun getTop (G, i, j) =
  case Int.compare (i, 0) of
    (LESS | EQUAL) => NONE
  | GREATER => SOME (Array2.sub (G, i - 1, j))

(* getBottom : 'a Array2.array * int * int -> 'a option *)
fun getBottom (G, i, j) =
  case Int.compare (i, (Array2.nRows G) - 1) of
    LESS => SOME (Array2.sub (G, i + 1, j))
  | (EQUAL | GREATER) => NONE

(* getLeft : 'a Array2.array * int * int -> 'a option *)
fun getLeft (G, i, j) =
  case Int.compare (j, 0) of
    (LESS | EQUAL) => NONE
  | GREATER => SOME (Array2.sub (G, i, j - 1))

(* getRight : 'a Array2.array * int * int -> 'a option *)
fun getRight (G, i, j) =
  case Int.compare (j, (Array2.nCols G) - 1) of
    LESS => SOME (Array2.sub (G, i, j + 1))
  | (EQUAL | GREATER) => NONE

(* isSome : 'a option -> bool *)
fun isSome (SOME x) = true
  | isSome (NONE) = false

(* rmSome : 'a option -> 'a *)
fun rmSome (SOME x) = x
  | rmSome (NONE) = raise Fail "rmSome Error"

(* getNeighbors : 'a Array2.array * int * int -> 'a list *)
fun getNeighbors (G, i, j) =
  ((List.map rmSome) o (List.filter isSome))
  [getTop(G,i,j), getBottom(G,i,j), getLeft(G,i,j), getRight(G,i,j)]

datatype bfs_color = White | Gray | Black
type vertex = int * int * int * bfs_color * int option

(* relMinAcc : vertex Array2.array -> vertex * vertex list -> vertex list *)
fun relMinAcc G (u as (i,j,k,l,m), acc) =
  if foldl (fn ((v,w,x,y,z),a) => k < x andalso a) true (getNeighbors (G,i,j))
  then acc @ [u]
  else acc

(* make int Array2.array *)
val arr =
  Array2.fromList
    (List.map
      ((List.map charToInt) o (List.filter Char.isDigit) o String.explode)
      (readInput (TextIO.openIn "day9Input.txt")))

val mkVertex = fn (i, j) => (i,j,Array2.sub(arr,i,j),White,NONE : int option)
val G = Array2.tabulate Array2.RowMajor (100, 100, mkVertex)

val rmins = Array2.fold Array2.RowMajor (relMinAcc G) [] G

val soln17 = foldl (op +) 0 (List.map (fn (_,_,x,_,_) => x + 1) rmins)

(* Advent of Code 2021, Puzzle 18 *)

(* QUEUE signature *)
signature QUEUE =
sig
  type 'a queue
  val emp : 'a queue
  val ins : 'a * 'a queue -> 'a queue
  val rem : 'a queue -> ('a * 'a queue) option
end

(* Queue : QUEUE structure *)
structure Queue : QUEUE =
struct
  type 'a queue = ('a list) * ('a list)

  val emp = ([], [])

  fun ins (n, (front, back)) = (front, n::back)

  fun rem (  [] ,  [] ) = NONE
    | rem (y::ys, back) = SOME (y,(ys,back))
    | rem (  [] , back) = rem (List.rev back,[])
end

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

(* escaped byte strings of utf-8 chars for graph display *)
val fullBlock = "\226\150\136"        (* u2588 ==> e2 96 88 bytes *)
val middleDot = "\194\183"            (* u00b7 ==> c2 b7 bytes *)

(* vertexToEscStr : vertex -> string *)
fun vertexToEscStr (i, j, aij, bij, NONE) = fullBlock
  | vertexToEscStr (i, j, aij, bij, SOME b) = middleDot

(* graphToEscStr : (vertex -> string) -> vertex Array2.array -> string *)
fun graphToEscStr f G =
  Array2.fold
    Array2.RowMajor
    (fn (x as (i,j,aij,bij,cij), acc) =>
      case Int.compare (j, Array2.nCols G - 1) of
        EQUAL => acc ^ f x ^ "\n"
      | (LESS | GREATER) => acc ^ f x)
    ""
    G

(* graphDisp : vertex Array2.array -> unit *)
fun graphDisp G = print (graphToEscStr vertexToEscStr G)

(* BFS : vertex Array2.array * 'a * int * int -> unit *)
fun BFS (G, n, i, j) =
  let
    val (_, _, aij, _, _) = Array2.sub (G, i, j)
    val Q = (Array2.update (G, i, j, (i, j, aij, Gray, SOME n));
            Queue.ins (Array2.sub (G, i, j), Queue.emp))
    fun impl (NONE) = ()
      | impl (SOME (u as (x, y, axy, bxy, cxy), Q')) =
        let
          (* filter out 9's and Gray/Black vertices *)
          val pred = fn (_, _, 9, _, _) => false
                      | (_, _, _, White, _) => true
                      | _ => false

          (* mark vertices as Gray and add basin number n *)
          val mark = fn ((x, y, axy, bxy, cxy), acc) =>
                        Array2.update (G, x, y, (x, y, axy, Gray, SOME n))

          (* insert vertices into accumulator Queue  *)
          val insv = fn ((x, y, axy, bxy, cxy), acc) =>
                        Queue.ins (Array2.sub (G, x, y), acc)

          (* filter, mark, then insert vertices *)
          val vs = List.filter pred (getNeighbors (G, x, y))
          val Q'' = (List.foldl mark () vs;
                    List.foldl insv Q' vs)
        in
          (* update color of current vertex u to Black, then loop *)
          (Array2.update (G, x, y, (x, y, axy, Black, cxy));
          impl (Queue.rem Q''))
        end
  in
    (* start BFS with singleton queue *)
    impl (Queue.rem Q)
  end

(* markBasin : vertex Array2.array -> ('a * vertex) * 'b -> 'b *)
fun markBasin G ((n, (i, j, aij, bij, cij)), acc) = BFS (G, n, i, j)

(* sizeBasin : vertex Array2.array -> ('a * vertex) * 'b -> 'b *)
fun sizeBasin G ((n, v), acc) =
    acc @
      [Array2.fold
        Array2.RowMajor
        (fn ((_, _, _, _, SOME k), acc') => if k = n then acc' + 1 else acc'
          | ((_, _, _, _, NONE), acc') => acc')
        0
        G]

(* enumerate list of relative minimums *)
val basins = ListPair.zip (List.tabulate (List.length rmins, fn x => x), rmins)

(* mark all basins then get sizes in reversed sorted order *)
val sizes = (List.foldl (markBasin G) () basins;
            rev (msort (List.foldl (sizeBasin G) [] basins)))

(* get three largest basin sizes *)
val b1 :: b2 :: b3 :: _ = sizes

(* calulate soln *)
val soln18 = b1 * b2 * b3
