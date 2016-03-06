module Location :
  sig
    type t = int * int
    val compare : 'a -> 'a -> int
    val leq : 'a -> 'a -> bool
  end

module type S =
  sig
		module Location = Location
    type 'a t
		val max_neighbours : int
		val is_boundary : 'a t -> Location.t -> bool
		val is_valid : 'a t -> Location.t -> bool
    val dim : 'a t -> int * int
    val dimx : 'a t -> int
    val dimy : 'a t -> int
    val init : (int * int) -> (Location.t -> 'a) -> 'a t
    val map : 'a t -> (Location.t -> 'a -> 'b) -> 'b t
    val update : 'a t -> (Location.t -> 'a -> 'a) -> unit
    val get : 'a t -> Location.t -> 'a
    val set : 'a t -> Location.t -> 'a -> unit
    val neighbours : 'a t -> Location.t -> (Location.t -> unit) -> unit
		val iter : 'a t -> (Location.t -> 'a -> unit) -> unit
  end

module Square : S
module Hex_horizontal : S
module Hex_vertical : S
