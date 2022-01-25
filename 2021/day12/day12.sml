(* Advent of Code 2021, Puzzle 23 *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

(* delim : char -> bool *)
val delim = fn #"-" => true | #"\n" => true | _ => false

(* mkEdge : string list -> string * string *)
fun mkEdge [u, v] = (u, v)
  | mkEdge _ = raise Fail "mkEdge Error"

(* structure K : ORD_KEY for Red-Black Tree ORD_MAP *)
structure K : ORD_KEY =
struct
  type ord_key = string
  val compare = String.compare
end

(* Dict :> ORD_MAP where type Key.ord_key = K.ord_key *)
structure Dict = RedBlackMapFn (K)

(* ins : (Dict.Key.ord_key * Dict.Key.ord_key) * Dict.Key.ord_key list Dict.map
 *       -> Dict.Key.ord_key list Dict.map
 *)
fun ins ((u,v), D) =
  case (Dict.inDomain (D, u), Dict.inDomain (D, v)) of
    (false, false) => Dict.insert (Dict.insert (D, u, [v]), v, [u])
  | (false, true ) =>
    let val us = Dict.lookup (D, v)
    in Dict.insert (Dict.insert (D, u, [v]), v, u :: us)
    end
  | (true , false) =>
    let val vs = Dict.lookup (D, u)
    in Dict.insert (Dict.insert (D, u, v :: vs), v, [u])
    end
  | (true , true ) =>
    let val us = Dict.lookup (D, v)
        val vs = Dict.lookup (D, u)
    in Dict.insert (Dict.insert (D, u, v :: vs), v, u :: us)
    end

(* input : (string * string) list *)
val input =
  List.map
    (mkEdge o (String.tokens delim))
    (readInput (TextIO.openIn "day12test1.txt"))

(* Adj : Dict.Key.ord_key list Dict.map *)
val Adj = foldl ins Dict.empty input

(* isSmall : string -> bool *)
fun isSmall s = Char.isLower (String.sub (s, 0))

(* pathToString : string list -> string *)
fun pathToString path = List.foldl (fn (x, acc) => acc ^ x) "" path

(* eq : string * string -> bool *)
fun eq (a, b) =  case String.compare (a, b) of EQUAL => true | _ => false

(* Advent of Code 2021, Puzzle 22 *)
