module type A = sig
  type t = private [> `A ]
      deriving (Show)
end

module Make(M : A) = struct
  type truc = Plop of M.t
        deriving (Show)

  let chose x = Plop x
end

module MA = struct
  type t = [ `A | `B ]
      deriving (Show)
end

module M = Make(MA)

let _ = print_endline (Show.show<M.truc>(M.chose `B))
