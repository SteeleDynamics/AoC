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

(* isSmall : string -> bool *)
fun isSmall s = Char.isLower (String.sub (s, 0))

(* pathToString : string list -> string *)
fun pathToString p = List.foldl (fn (x, acc) => acc ^ x) "" p

(* eq : string * string -> bool *)
fun eq (a, b) =  case String.compare (a, b) of EQUAL => true | _ => false

(* memberOf : string list * string -> bool *)
fun memberOf (L, e) = List.exists (fn x => eq (x, e)) L

fun dfs (Adj, s, f) =
  let val us = Dict.lookup (Adj, s)
  in List.foldl (dfsVisit Adj s f []) [] us
  end
and dfsVisit Adj u f path = fn (v, acc) =>
  let
    val ws = Dict.lookup (Adj, v)
    val p = path @ [u]
  in
    case (eq (u, f), isSmall u, memberOf (path, u)) of
      (false, false, _    ) => List.foldl (dfsVisit Adj v f p) acc ws
    | (false, true , false) => List.foldl (dfsVisit Adj v f p) acc ws
    | (false, true , true ) => acc
    | (true , _    , _    ) =>
      let val e = pathToString p
          val L = List.map (fn x => pathToString x) acc
      in  if memberOf (L, e)
          then acc
          else p :: acc
      end
  end

(* input : (string * string) list *)
val input1 = List.map (mkEdge o (String.tokens delim))
    (readInput (TextIO.openIn "day12test1.txt"))
val input2 = List.map (mkEdge o (String.tokens delim))
    (readInput (TextIO.openIn "day12test2.txt"))
val input3 = List.map (mkEdge o (String.tokens delim))
    (readInput (TextIO.openIn "day12test3.txt"))
val input = List.map (mkEdge o (String.tokens delim))
    (readInput (TextIO.openIn "day12Input.txt"))

(* Adj : Dict.Key.ord_key list Dict.map *)
val Adj1 = foldl ins Dict.empty input1
val Adj2 = foldl ins Dict.empty input2
val Adj3 = foldl ins Dict.empty input3
val Adj  = foldl ins Dict.empty input

(* unit tests and solution *)
val test1 = List.length (dfs (Adj1, "start", "end"))
val test2 = List.length (dfs (Adj2, "start", "end"))
val test3 = List.length (dfs (Adj3, "start", "end"))
val soln23  = List.length (dfs (Adj, "start", "end"))

(* Advent of Code 2021, Puzzle 24 *)
