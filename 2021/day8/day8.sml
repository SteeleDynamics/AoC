(* Read input file *)
fun readInput inStream =
  case TextIO.inputLine inStream of
    SOME "\n" => readInput inStream           (* skip empty lines *)
  | SOME str => str :: (readInput inStream)
  | NONE => []

fun parse [] = raise Fail "parse EntryEmptyErr"
  | parse [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,"|",d0,d1,d2,d3] =
      ([p0,p1,p2,p3,p4,p5,p6,p7,p8,p9], [d0,d1,d2,d3])
  | parse _ = raise Fail "parse EntryFmtErr"

val entries =
  List.map
    (parse o (String.tokens Char.isSpace))
    (readInput (TextIO.openIn "day8Input.txt"))

(* Advent of Code 2021, Puzzle 15 *)
val g = fn (d, acc2) => case String.size d of (2|3|4|7) => acc2 + 1 | _ => acc2
val f = fn ((ps, ds), acc1) => List.foldl g acc1 ds
val soln15 = List.foldl f 0 entries

(* Advent of Code 2021, Puzzle 16 *)
signature EQ =
sig
  type t
  val equal : t * t -> bool
  val toString : t -> string
end

signature SET =
sig
  structure Elt : EQ              (* The EQ sig to use for elt comparison *)
  type t                          (* The type of the set *)

  val empty : t                   (* (ctor) create empty set  *)
  val singleton : Elt.t -> t      (* (ctor) create singleton set *)

  val size : t -> int             (* (dtor) get size of set *)
  val toString : t -> string      (* (dtor) string repr SET of Elt.t : EQ *)
  val toList : t -> Elt.t list    (* (dtor) list repr SET of Elt : EQ *)

  val insert : t -> Elt.t -> t    (* Elt related fns ... *)
  val remove : t -> Elt.t -> t
  val member : t -> Elt.t -> bool

  val union : t * t -> t          (* Bulk Ops ... *)
  val intersection : t * t -> t
  val difference : t * t -> t
end

functor MkSet (Elt : EQ) :> SET where type Elt.t = Elt.t =
struct
  structure Elt = Elt
  type t = Elt.t list

  val empty = []
  val singleton = fn x => x::[]

  val size = List.length
  fun toString' [] = "|]"
    | toString' [e] = Elt.toString e ^ toString' []
    | toString' (e::es) = Elt.toString e ^ ", " ^ toString' es
  fun toString S = "[|" ^ toString' S
  fun toList S = S

  val remove' = fn (x,l) => List.filter (not o Fn.curry Elt.equal x) l
  val insert' = fn (x,l) => x :: remove' (x,l)
  val member = fn l => fn x => List.exists (Fn.curry Elt.equal x) l

  val insert = Fn.curry (Fn.flip insert')
  val remove = Fn.curry (Fn.flip remove')

  val union = Fn.uncurry (foldr insert')
  val intersection = fn (s,t) => List.filter (member t) s
  val difference = fn (s,t) => foldr remove' s t
end

(* Implementation of CharElt : EQ *)
structure CharElt : EQ =
struct
  type t = char

  fun equal (x, y) =
    case Char.compare (x, y) of
      EQUAL => true
    | (LESS | GREATER) => false

  fun toString c = Char.toString c
end

(* Implementation of CharSet : SET *)
structure CharSet = MkSet (CharElt)

(* charsToSet : char list * CharSet -> CharSet *)
fun charsToSet ([], acc) = acc
  | charsToSet (c::cs, acc) = charsToSet (cs, CharSet.insert acc c)

(* strToSet : string -> CharSet *)
fun strToSet str = charsToSet (String.explode str, CharSet.empty)

(* listToString : ('a -> string) -> 'a list -> string *)
fun listToString' f [] = "]"
  | listToString' f [e] = (f e) ^ listToString' f []
  | listToString' f (e::es) = (f e) ^ ", " ^ listToString' f es
fun listToString f L = "[" ^ listToString' f L

(* mapPair : ('a -> 'b) -> 'a * 'a -> 'b * 'b *)
fun mapPair f (a, b) = (f a, f b)

(* charSetEqual : CharSet * CharSet -> bool *)
fun charSetEqual (A, B) =
  CharSet.size (CharSet.difference (A, B)) = 0 andalso
  CharSet.size (CharSet.difference (B, A)) = 0

(* getElt : CharSet.t -> CharSet.Elt.t *)
fun getElt S = List.hd (CharSet.toList S)

fun getOutputVal entry =
  let
    val (patterns, digits) = mapPair (List.map strToSet) entry
    val SOME oneSet = List.find (fn S => CharSet.size S = 2) patterns
    val SOME sevenSet = List.find (fn S => CharSet.size S = 3) patterns
    val SOME fourSet = List.find (fn S => CharSet.size S = 4) patterns
    val SOME eightSet = List.find (fn S => CharSet.size S = 7) patterns
    val twoThreeFive = List.filter (fn S => CharSet.size S = 5) patterns
    val zeroSixNine = List.filter (fn S => CharSet.size S = 6) patterns
    val SOME threeSet =
      List.find
        (fn S => CharSet.size (CharSet.difference (oneSet, S)) = 0)
        twoThreeFive
    val tlSeg = getElt (CharSet.difference (fourSet, threeSet))
    val twoFive =
      List.filter
        (fn S => CharSet.size (CharSet.difference (oneSet, S)) <> 0)
        twoThreeFive
    val SOME twoSet = List.find (fn S => not (CharSet.member S tlSeg)) twoFive
    val SOME fiveSet = List.find (fn S => CharSet.member S tlSeg) twoFive
    val trSeg = getElt (CharSet.intersection (oneSet, twoSet))
    val blSeg = getElt (CharSet.difference (twoSet, threeSet))
    val cSeg =
      getElt
        (CharSet.intersection (
          CharSet.difference (threeSet, sevenSet),
          fourSet))
    val SOME sixSet =
      List.find (fn S => not (CharSet.member S trSeg)) zeroSixNine
    val SOME nineSet =
      List.find (fn S => not (CharSet.member S blSeg)) zeroSixNine
    val SOME zeroSet =
      List.find (fn S => not (CharSet.member S cSeg)) zeroSixNine

    fun matchDigit (digit) =
      case CharSet.size digit of
        2 => 1
      | 3 => 7
      | 4 => 4
      | 7 => 8
      | 5 => if charSetEqual (digit, twoSet) then 2
             else if charSetEqual (digit, threeSet) then 3
             else 5
      | 6 => if charSetEqual (digit, zeroSet) then 0
             else if charSetEqual (digit, sixSet) then 6
             else 9
      | _ => raise Fail "matchDigit Error"
  in
    foldl (fn (x, acc) => 10 * acc + x) 0 (List.map matchDigit digits)
  end

val soln16 = foldl (op +) 0 (List.map getOutputVal entries)
