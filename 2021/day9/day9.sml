(* Advent of Code 2021, Puzzle 17 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

(* charToInt : char -> int *)
fun charToInt c = Char.ord c - Char.ord #"0"

(* getTop : 'a Array2.array * int * int -> (int * int * 'a) option *)
fun getTop (arr, i, j) =
  case Int.compare (i, 0) of
    (LESS | EQUAL) => NONE
  | GREATER => SOME (i, j, Array2.sub (arr, i - 1, j))

(* getBottom : 'a Array2.array * int * int -> (int * int * 'a) option *)
fun getBottom (arr, i, j) =
  case Int.compare (i, (Array2.nRows arr) - 1) of
    LESS => SOME (i, j, Array2.sub (arr, i + 1, j))
  | (EQUAL | GREATER) => NONE

(* getLeft : 'a Array2.array * int * int -> (int * int * 'a) option *)
fun getLeft (arr, i, j) =
  case Int.compare (j, 0) of
    (LESS | EQUAL) => NONE
  | GREATER => SOME (i, j, Array2.sub (arr, i, j - 1))

(* getRight : 'a Array2.array * int * int -> (int * int * 'a) option *)
fun getRight (arr, i, j) =
  case Int.compare (j, (Array2.nCols arr) - 1) of
    LESS => SOME (i, j, Array2.sub (arr, i, j + 1))
  | (EQUAL | GREATER) => NONE

(* isSome : 'a option -> bool *)
fun isSome (SOME x) = true
  | isSome (NONE) = false

(* rmSome : 'a option -> 'a *)
fun rmSome (SOME x) = x
  | rmSome (NONE) = raise Fail "rmSome Error"

(* getNeighbors : 'a Array2.array * int * int -> 'a list *)
fun getNeighbors (arr, i, j) =
  ((List.map rmSome) o (List.filter isSome))
  [getTop(arr,i,j), getBottom(arr,i,j), getLeft(arr,i,j), getRight(arr,i,j)]

(* relMinAcc : int Array2.array -> int * int * int * int list -> int list *)
fun relMinAcc arr (i, j, aij, acc) =
  if foldl (fn ((x,y,v),a) => aij < v andalso a) true (getNeighbors (arr,i,j))
  then acc @ [(i,j,aij)]
  else acc

val arr =
  Array2.fromList
    (List.map
      ((List.map charToInt) o (List.filter Char.isDigit) o String.explode)
      (readInput (TextIO.openIn "day9Input.txt")))

val relMins = Array2.foldi Array2.RowMajor (relMinAcc arr) []
              {base=arr, row=0, col=0, nrows=NONE, ncols=NONE}

val soln17 = foldl (op +) 0 (List.map (fn (_, _, x) => x + 1) relMins)

(* Advent of Code 2021, Puzzle 18 *)
fun fillBasin (arr, n, i, j) =
  case (Array2.sub (arr, i, j)) of
    (_, SOME k) => ()
  | (9, NONE) => ()
  | (aij, NONE) =>
    let
     val f = fn ((x, y, _), acc) => fillBasin (arr, n, x, y)
    in
      (Array2.update (arr, i, j, (aij, SOME n));
      foldl f () (getNeighbors (arr, i, j)))
    end

val tabf = fn (i, j) => (Array2.sub (arr, i, j), NONE : int option)
val arr' = Array2.tabulate Array2.RowMajor (100, 100, tabf)
val (i0, j0, _) = List.hd relMins
val () = fillBasin (arr', 0, i0, j0)
