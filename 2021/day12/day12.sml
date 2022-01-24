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
fun isSmall k =
  let fun impl [] = raise Fail "isSmall Error"
        | impl (c :: cs) = if Char.isLower c then true else false
  in impl (String.explode k)
  end

fun memberOf (L, e) =
  let val p = fn x => case String.compare (x, e) of EQUAL => true | _ => false
  in List.exists p L
  end

(* findOnePath : Dict.Key.ord_key -> string -> Dict.Key.ord_key list Dict.map
 *               -> Dict.Key.ord_key list -> string list -> (string list -> 'a)
 *               -> (unit -> 'a) -> 'a
 * REQUIRES: G is a graph such that
 *           - s and f are vertices in G
 *           - Adj is the adjacency list representation of G
 *           - us are current vertices adjacent to s (obtained from Adj)
 *           - visited are current small vertices/caves visited
 * ENSURES:  findOnePath s f Adj us visited sc fc  ==> sc p such that
 *           - p : string list that represents a path from s to f
 *           - along p, big vertices/caves can be visited multiple times
 *           - along p, small vertices/caves can be visited at most one time
 *)
fun findOnePath s f Adj us visited sc fc =
  case (s = f, isSmall s, memberOf (visited, s), us) of
    (true , _    , _    , _      ) => sc (f :: [])
  | (false, _    , _    , []     ) => fc ()
  | (false, false, _    , v :: vs) =>
    let val ws = Dict.lookup (Adj, v)
    in findOnePath v f Adj ws visited (fn r1 => sc (s :: r1)) (fn () =>
         findOnePath s f Adj vs visited (fn r2 => sc (s :: r2)) fc)
    end
  | (false, true , true , v :: vs) => fc ()
  | (false, true , false, v :: vs) =>
    let val ws = Dict.lookup (Adj, v)
    in findOnePath v f Adj ws (s :: visited) (fn r1 => sc (s :: r1)) (fn () =>
         findOnePath s f Adj vs (s :: visited) (fn r2 => sc (s :: r2)) fc)
    end

val s = "start"
val f = "end"
val us = Dict.lookup (Adj, s)
val sc = SOME
val fc = fn () => NONE
val path = findOnePath s f Adj us [] sc fc

(* Advent of Code 2021, Puzzle 22 *)
