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

(* eq : string * string -> bool *)
fun eq (a, b) =  case String.compare (a, b) of EQUAL => true | _ => false

fun dfs1 Adj =
  let
    val keys = Dict.listKeys Adj
    val init = fn (x, acc) => Dict.insert (acc, x, 0)
    val Visited = List.foldl init Dict.empty keys
    val u = "start"
    val vs = Dict.lookup (Adj, u)
    val p = ""
  in
    List.foldl (dfsVisit1 Adj Visited u p) Dict.empty vs
  end
and dfsVisit1 Adj Visited u p = fn (v, acc) =>
  let
    val small_u = isSmall u
    val visited_count_u = Dict.lookup (Visited, u)
    val visited_u = visited_count_u > 0
    val found_f = eq (u, "end")
    val q = p ^ u
    val Visited' = Dict.insert (Visited, u, visited_count_u + 1)
    val ws = Dict.lookup (Adj, v)
    val dfsVisitFn1 = dfsVisit1 Adj Visited' v q
  in
    case (found_f, small_u, visited_u) of
      (false,false,_    ) => List.foldl dfsVisitFn1 acc ws
    | (false,true ,false) => List.foldl dfsVisitFn1 acc ws
    | (false,true ,true ) => acc
    | (true ,_    ,_    ) => Dict.insert (acc, q, 0)
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
val test1_1 = Dict.numItems (dfs1 Adj1)
val test2_1 = Dict.numItems (dfs1 Adj2)
val test3_1 = Dict.numItems (dfs1 Adj3)
val soln23 = Dict.numItems (dfs1 Adj)

(* Advent of Code 2021, Puzzle 24 *)

fun dfs2 Adj =
  let
    val keys = Dict.listKeys Adj
    val init = fn (x, acc) => Dict.insert (acc, x, 0)
    val Visited = List.foldl init Dict.empty keys
    val u = "start"
    val vs = Dict.lookup (Adj, u)
    val p = ""
    val b = false
  in
    List.foldl (dfsVisit2 Adj Visited u p b) Dict.empty vs
  end
and dfsVisit2 Adj Visited u p b = fn (v, acc) =>
  let
    val small_u = isSmall u
    val visited_count_u = Dict.lookup (Visited, u)
    val visited_u = visited_count_u > 0
    val found_s = eq (u, "start")
    val found_f = eq (u, "end")
    val q = p ^ u
    val Visited' = Dict.insert (Visited, u, visited_count_u + 1)
    val ws = Dict.lookup (Adj, v)
    val dfsVisitFn2 = fn x => dfsVisit2 Adj Visited' v q x
  in
    case (found_s, found_f, small_u, visited_u, b) of
      (true ,_    ,_    ,false,_    ) => List.foldl (dfsVisitFn2 b) acc ws
    | (true ,_    ,_    ,true ,_    ) => acc
    | (false,false,false,_    ,_    ) => List.foldl (dfsVisitFn2 b) acc ws
    | (false,false,true ,false,_    ) => List.foldl (dfsVisitFn2 b) acc ws
    | (false,false,true ,true ,false) => List.foldl (dfsVisitFn2 true) acc ws
    | (false,false,true ,true ,true ) => acc
    | (false,true ,_    ,_    ,_    ) => Dict.insert (acc, q, 0)
  end

(* unit tests and solution *)
val test1_2 = Dict.numItems (dfs2 Adj1)
val test2_2 = Dict.numItems (dfs2 Adj2)
val test3_2 = Dict.numItems (dfs2 Adj3)
val soln24 = Dict.numItems (dfs2 Adj)
