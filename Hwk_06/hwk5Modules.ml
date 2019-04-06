open StreamModules


module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) : Hwk5Sig = struct
   type 'a stream= 'a S.t

   let take=S.take
   let head=S.head
   let zip=S.zip
   let from=S.from
   let nats=from 1
   let cubes_from=S.cubes_from
   let cubes_from_zip n=
   zip ( * ) (from n) (zip ( * ) (from n) (from n))
   let cubes_from_map n=
   S.map (fun x->x*x*x) (from n)
   let drop=S.drop
   let drop_until=S.drop_until
   let sum_positive_prefix input=S.foldr  (fun a b->if a>0 then (a+(S.demand b)) else 0) input
   let primes=S.sieve (from 2)

end