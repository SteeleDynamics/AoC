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

val relMins = Array2.fold Array2.RowMajor (relMinAcc G) [] G

val soln17 = foldl (op +) 0 (List.map (fn (_,_,x,_,_) => x + 1) relMins)

(* Advent of Code 2021, Puzzle 18 *)
signature QUEUE =
sig
  type 'a queue
  val emp : 'a queue
  val ins : 'a * 'a queue -> 'a queue
  val rem : 'a queue -> ('a * 'a queue) option
end

structure Queue : QUEUE =
struct
  type 'a queue = ('a list) * ('a list)

  val emp = ([], [])

  fun ins (n, (front, back)) = (front, n::back)

  fun rem (  [] ,  [] ) = NONE
    | rem (y::ys, back) = SOME (y,(ys,back))
    | rem (  [] , back) = rem (List.rev back,[])
end

val whiteSq = "\^[[0;47m \^[[0;49m"   (* ASCII CSI with ANSI colors *)
val middleDot = "\194\183"            (* UTF-16 u00b7 ==> UTF-8 c2 b7 bytes *)

fun vertexToEscStr (i, j, aij, bij, NONE) = middleDot
  | vertexToEscStr (i, j, aij, bij, SOME b) = Int.toString b

fun graphToEscStr f G =
  Array2.fold
    Array2.RowMajor
    (fn (x as (i,j,aij,bij,cij), acc) =>
      case Int.compare (j, Array2.nCols G - 1) of
        EQUAL => acc ^ f x ^ "\n"
      | (LESS | GREATER) => acc ^ f x)
    ""
    G

fun graphDisp G = print (graphToEscStr vertexToEscStr G)

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
