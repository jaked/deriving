(* sums (nullary, unary, and n-ary) *)
type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
  deriving (Dump, Eq, Show, Typeable, Pickle)

type nullsum = N0 | N1 | N2 | N3
    deriving (Enum, Bounded, Eq, Typeable, Pickle)

(* records with mutable and immutable fields (and various combinations) *)
type r1 = {
  r1_l1 : int;
  r1_l2 : int;
} deriving (Dump, Eq, Show, Typeable, Pickle, Functor)

type r2 = {
  mutable r2_l1 : int;
  mutable r2_l2 : int;
} deriving (Eq, Show, Typeable, Pickle)

type r3 = {
  r3_l1 : int;
  mutable r3_l2 : int;
} deriving (Eq, Show, Typeable, Pickle)

(* polymorphic records *)
type r4 = {
  r4_l1 : 'a . 'a list
} deriving (Dump, Eq, Show)

(* label types *)
type label = x:int -> int
  (*  deriving (Dump, Eq, Show) *)

(* function types  *)
type funct = int -> int
  (* deriving (Dump, Eq, Show) *)

(* recursive types *)
type intseq = INil | ICons of int * intseq
  deriving (Dump, Eq, Show, Typeable, Pickle, Functor)

type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

(* applied type constructors (nullary, n-ary) *)
type uses_seqs = (intseq * float seq)
    deriving (Dump, Eq, Show, Typeable, Pickle)

(* object and class types *)
type obj = < x : int >

(* class types *)
class c = object end

(* polymorphic variants (nullary, unary tags, extending complex type expressions, defined inline) *)
type poly0 = [`T0 | `T1 | `T2 | `T3]
    deriving (Enum, Bounded, Show, Eq, Typeable, Pickle)

type poly1 = [`T0 | `T1 of int]
    deriving (Dump, Eq, Show)

type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Dump, Eq, Show)

(* `as'-recursion *)
type poly3 = [`Nil | `Cons of int * 'c] as 'c
    deriving (Dump, Eq, Show, Typeable, Pickle)

type poly3b = int * ([`Nil | `Cons of int * 'c] as 'c) * [`F]
    deriving (Dump, Eq, Show, Typeable, Pickle)

(* <, >, =, > < polymorphic variants *)
type 'a poly7 = Foo of [`F of 'a]
and 'a poly8 = { x : [`G of [`H of [`I of 'a poly7]]] }
    deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

(*
type poly9 = [`F | [`G]]
    deriving (Dump, Eq, Show, Typeable, Pickle)
  currently broken.
*)
type poly10 = [`F | poly3]
    deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

(* mutually recursive types (monomorphic, polymorphic) *)
type mutrec_a = mutrec_c
and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
and mutrec_c = S of int * mutrec_a | N
and mutrec_d = [`T of mutrec_b]
    deriving (Dump, Eq, Show, Typeable, Pickle)

type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
    deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

type 'a pmutrec_a' = ('a,'a) pmutrec_c'
and ('a,'b) pmutrec_b' = { pl1' : ('b,'a) pmutrec_c' ; pl2' : 'a pmutrec_a' }
and ('a,'b) pmutrec_c' = SS' of 'a * 'b pmutrec_a' * 'b | TT' of ('a * ('a,'b,'a) pmutrec_d' * 'b)
and ('a,'b,'c) pmutrec_d' = [ `S of ('a,'b) pmutrec_b' | `T of ('b,'c) pmutrec_b' ]
    deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

(* polymorphic types *)
type 'a ff1 = F of 'a * 'a | G of int deriving (Show, Eq, Dump, Functor, Typeable, Pickle)
type ('a,'b) ff2 = F1 of ('a,'b) ff2 | F2 of 'a seq * int * 'b option
  deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

(* tuples *)
type tup0 = unit
    deriving (Dump, Eq, Show, Typeable, Pickle)
type tup2 = int * float
    deriving (Dump, Eq, Show, Typeable, Pickle)
type tup3 = int * float * bool
    deriving (Dump, Eq, Show, Typeable, Pickle)
type tup4 = int * int * bool * unit
    deriving (Dump, Eq, Show, Typeable, Pickle, Bounded)

(* type equations (replication) *)
(* TODO *)

(* references *)
type withref = WR of int * (int ref)
  deriving (Eq, Show, Typeable, Pickle)

(* through module boundaries *)
module rec M : sig
  type t deriving (Show, Eq, Dump)
end =
struct
  type t = [`N|`C of M.t] deriving (Show, Eq, Dump)
end

(* parameterized types through module boundaries *)
module rec P : sig
  type 'a t (* deriving (Show) *)
end =
struct
  type 'a t = [`N|`C of 'a P.t]
(*Doesn't work: results in an unsafe module definition
*)(*      deriving (Show)*)
end

(* with constraints *)
type 'a constrained = [`F of 'a] constraint 'a = int
    deriving (Functor) (* Show, etc. don't work here *)

(* private datatypes *)
type p1 = private P1
    deriving (Show, Eq)

(* check that `private' in the interface is allowed for classes that
   disallow `private' (e.g. Dump) as long as we don't have `private'
   in the implementation *)
module Private : sig
  type p2 = private Q deriving (Show, Eq, Dump)
end =
struct
  type p2 = Q deriving (Show, Eq, Dump)
end

(* Reusing existing instances *)
type t = int
    deriving (Eq, Enum, Bounded, Dump, Show, Typeable, Pickle, Functor)

(* Int32, etc. *)

type ii = {
    int32: int32;
    int64: int64;
    nativeint: nativeint;
  } deriving (Eq, Dump, Typeable, Pickle, Show)

type ii' = {
    int32': Int32.t;
    int64': Int64.t;
  } deriving (Eq, Dump, Typeable, Pickle, Show)

#if ocaml_version >= (4, 00)

(* GADTs *)

type _ g1 =
  | I : int -> int g1
  | C : 'a -> 'a g1
  | L : 'a list -> 'a list g1
  | R : 'a g1 * 'a -> 'a g1
  | B : 'a * 'a * int -> 'a g1
      deriving (Show)

type (_, _) g2 =
  | A : 'a -> ('a, 'b) g2
  | B : 'b -> ('a, 'b) g2
  | R : ('b, 'a) g2 -> ('a, 'b) g2
      deriving (Show)

type _ g3 =
  | A : 'a g4 * 'a -> 'a g3
  | B : int g3
and _ g4 =
  | C : 'a g3 * 'a -> 'a g4
  | D : float g4
      deriving (Show)

#endif
