datatype 'a shrub = Leaf of 'a
                  | Branch of 'a shrub * 'a shrub

val p1 = fn x => x > 0
val p2 = fn x => x = 1
val eq = op=
val sc = SOME
val fc = fn () => NONE

val T0 = Branch (Branch (Leaf 0, Leaf 0), Branch (Leaf 1, Leaf 1))
val T1 = Branch (Branch (Leaf 0, Leaf 0), Branch (Leaf 1, Leaf 2))
val T2 = Branch (Branch (Leaf 0, Leaf 1), Branch (Leaf 2, Leaf 3))
val T3 = Branch (Branch (Leaf 3, Leaf 2), Branch (Leaf 1, Leaf 0))

(* findOne : ('a -> bool) -> 'a shrub -> ('a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total
 * ENSURES:  findOne p T sc fc ==> s x if x is in T and p(x) ==> true.
 *           If no such x exists, this evalutes to fc () instead.
 *           If more than one such x exists,
 *           findOne p T sc fc evaluates the leftmost such x.
 *)
fun findOne p (Leaf x) sc fc = if p x then sc x else fc ()
  | findOne p (Branch (L,R)) sc fc =
      findOne p L sc (fn () =>
      findOne p R sc fc)

(* findOne test cases *)
val NONE = findOne p1 (Leaf 0) sc fc
val SOME 1 = findOne p1 (Branch (Leaf 1, Leaf 2)) sc fc
val SOME 2 = findOne p1 (Branch (Leaf 0, Leaf 2)) sc fc

(* findTwo : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub ->
 *           ('a * 'a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total, eq is total, eq represents an equivalence relation
 * ENSURES:  findTwo p eq T sc fc ==> sc(x,y) if x and y are values in T s.t.
 *           p(x) ==> true and p(y) ==> true and eq(x,y) ==> false.
 *           If no such pair (x,y) exists, findTwo p T sc fc ==> fc ()
 *)
fun findTwo p eq (Leaf x) sc fc = fc ()
  | findTwo p eq (Branch (L,R)) sc fc =
    let
      fun q v = fn x => (p x) andalso not (eq (x,v))
    in
      findOne p L (fn r1 =>
        findOne (q r1) L (fn r2 => sc (r1, r2)) (fn () =>
        findOne (q r1) R (fn r2 => sc (r1, r2)) fc)) (fn () =>
      findOne p R (fn r1 =>
        findOne (q r1) R (fn r2 => sc (r1,r2)) fc) fc)
    end

(* findTwo test cases *)
val NONE = findTwo p1 eq T0 sc fc
val SOME (1,2) = findTwo p1 eq T1 sc fc
val SOME (1,2) = findTwo p1 eq T2 sc fc
val SOME (3,2) = findTwo p1 eq T3 sc fc

(* findN : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub -> int ->
 *         ('a list -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: n >= 0, p total, eq total, eq represents an equivalence relation
 * ENSURES: findN p eq T n sc fc ==> sc L if there exists a list L of length n
 *          s.t. the elements in L are pairwise distinct by eq, and for each
 *          element x in L, p x ==> true. Otherwise evaluates to fc ().
 *)
fun findN (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
          (n : int) (sc : 'a list -> 'b) (fc : unit -> 'b) =
  case n of
    0 => sc []
  | _ =>
      let
        fun q v = fn x => (p x) andalso not (eq (x,v))
        fun success x = findN (q x) eq T (n-1) (fn r1 => sc (x::r1)) fc
      in
        findOne p T success fc
      end

(* findN test cases *)
val SOME [] = findN p1 eq T2 0 sc fc
val SOME [1] = findN p1 eq T2 1 sc fc
val SOME [1,2] = findN p1 eq T2 2 sc fc
val SOME [1,2,3] = findN p1 eq T2 3 sc fc
val SOME [3,2,1] = findN p1 eq T3 3 sc fc
val NONE = findN p1 eq T3 4 sc fc
