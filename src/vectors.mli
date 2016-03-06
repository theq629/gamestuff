module type S =
  sig
    type 'a t
    val int_of_float : float t -> int t
    val float_of_int : int t -> float t
    val length : float t -> float
    val norm : float t -> float t
    val dist : float t -> float t -> float
    val ( + ) : int t -> int t -> int t
    val ( - ) : int t -> int t -> int t
    val ( * ) : int -> int t -> int t
    val ( / ) : int t -> int -> int t
    val ( ** ) : int t -> int t -> int
    val ( *** ) : int t -> int t -> int t
    val ( /// ) : int t -> int t -> int t
    val ( +. ) : float t -> float t -> float t
    val ( -. ) : float t -> float t -> float t
    val ( *. ) : float -> float t -> float t
    val ( /. ) : float t -> float -> float t
    val ( **. ) : float t -> float t -> float
    val ( ***. ) : float t -> float t -> float t
    val ( ///. ) : float t -> float t -> float t
		val all : 'a -> 'a t
		val map : ('a -> 'b) -> 'a t -> 'b t
		val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
		val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
		val sumi : int t -> int
		val sumf : float t -> float
		val prodi : int t -> int
		val sumf : float t -> float
  end

module TwoD :
  sig
		include S with type 'a t = ('a * 'a)
    val perpi : int t -> int t
    val perpf : float t -> float t
  end

module ThreeD :
  sig
		include S with type 'a t = ('a * 'a * 'a)
  end

module FourD :
  sig
		include S with type 'a t = ('a * 'a * 'a * 'a)
  end
