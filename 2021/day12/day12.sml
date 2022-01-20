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
    (readInput (TextIO.openIn "day12Input.txt"))

(* Adj : Dict.Key.ord_key list Dict.map *)
val Adj = foldl ins Dict.empty input

(* ids : Dict.Key.ord_key list *)
val ids = Dict.listKeys Adj

(* isSmall : string -> bool *)
fun isSmall k =
  let fun impl [] = raise Fail "isSmall Error"
        | impl (c :: cs) = if Char.isLower c then true else false
  in impl (String.explode k)
  end

(* signature VERTEX for DFS *)
signature VERTEX =
sig
  type vertex_ids
  type vertex_color
  type t

  (* constructors *)
  val init : t
  val setColor : t * vertex_color -> t
  val setPreds : t * vertex_ids -> t

  (* destructors *)
  val getColor : t -> vertex_color
  val getPreds : t -> vertex_ids
end

(* structure Vertex :> VERTEX *)
structure Vertex : VERTEX =
struct
  type vertex_ids = Dict.Key.ord_key list          (* multiple predecessors *)
  datatype vertex_color = White | Gray | Black
  type t = vertex_color * vertex_ids               (* (color, pis) *)

  val init = (White, [])
  fun setColor ((color, pis), color') = (color', pis)
  fun setPreds ((color, pis), pis') = (color, pis')

  fun getColor (color, pis) = color
  fun getPreds (color, pis) = pis
end

(* V : Vertex.t Dict.map *)
val V = foldl (fn (k,D) => Dict.insert (D, k, Vertex.init)) Dict.empty ids

(* mkPath : Vertex.t Dict.map * Dict.Key.ord_key * Dict.Key.ord_key
 *          Dict.Key.ord_key list -> Dict.Key.ord_key list
 *)
fun mkPath (V, s, v, acc) =
  case (v = s, Vertex.getPreds (Dict.lookup (V, v))) of
    (true , _        ) => s :: acc
  | (false, []       ) => raise Fail "mkPath Error"
  | (false, pi :: pis) =>
    let val vertex_v = Dict.lookup (V, v)
        val vertex_v' = Vertex.setPreds (vertex_v, pis)
        val V' = Dict.insert (V, v, vertex_v')
    in mkPath (V', s, pi, v :: acc)
    end

(* DFS : Vertex.t Dict.map * Dict.Key.ord_key list Dict.map *
 *       Vertex.vertex_id * Vertex.vertex_id -> Vertex.vertex_id list list
 * REQUIRES: let K be the set of all vertex_ids (keys)
 *           V and Adj have domain K
 *           s and f are elements of K
 * ENSURES:  DFS (V, Adj, s, f) ==> list of all paths such that s ~~> f, small
 *             caves are only visited once, and large caves can be visited
 *             multiple times
 *)
fun DFS (V, Adj, s, f) =
  let
    fun DFS_Visit (V, Adj, u, acc) = raise Fail "Unimplemented"
  in
    raise Fail "Unimplemented"
  end

(* Advent of Code 2021, Puzzle 22 *)
