(*
Basic vectors of various sizes.
*)

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

module OneD =
struct
	type 'a t = 'a

	module Operators =
		struct
			let ( + ) x1 x2 = Pervasives.(x1 + x2)
			let ( - ) x1 x2 = Pervasives.(x1 - x2)
			let ( * ) c x = Pervasives.(c * x)
			let ( / ) x c = Pervasives.(x / c)
			let ( ** ) x1 x2 = Pervasives.(x1 * x2)
			let ( *** ) x1 x2 = Pervasives.(x1 * x2)
			let ( /// ) x1 x2 = Pervasives.(x1 / x2)
			let ( +. ) x1 x2 = Pervasives.(x1 +. x2)
			let ( -. ) x1 x2 = Pervasives.(x1 -. x2)
			let ( *. ) c x = Pervasives.(c *. x)
			let ( /. ) x c = Pervasives.(x /. c)
			let ( **. ) x1 x2 = Pervasives.(x1 *. x2)
			let ( ***. ) x1 x2 = Pervasives.(x1 *. x2)
			let ( ///. ) x1 x2 = Pervasives.(x1 /. x2)
		end

	let int_of_float x = (int_of_float x)
	let float_of_int x = (float_of_int x)

	let length x = x
	let norm _ = 1.
	let dist x1 x2 = abs_float (x2 -. x1)

	let all x = x
	let map f x = f x
	let fold_left f a x = f a x
	let fold_right f x b = f x b
	let sumi x = x
	let sumf x = x
	let prodi x = x 
	let prodf x = x

	include Operators
end

module TwoD =
struct
	type 'a t = 'a * 'a

	module Operators =
		struct
			let ( + ) (x1, y1) (x2, y2) = Pervasives.(x1 + x2, y1 + y2)
			let ( - ) (x1, y1) (x2, y2) = Pervasives.(x1 - x2, y1 - y2)
			let ( * ) c (x, y) = Pervasives.(c * x, c * y)
			let ( / ) (x, y) c = Pervasives.(x / c, y / c)
			let ( ** ) (x1, y1) (x2, y2) = Pervasives.(x1 * x2 + y1 * y2)
			let ( *** ) (x1, y1) (x2, y2) = Pervasives.(x1 * x2, y1 * y2)
			let ( /// ) (x1, y1) (x2, y2) = Pervasives.(x1 / x2, y1 / y2)
			let ( +. ) (x1, y1) (x2, y2) = Pervasives.(x1 +. x2, y1 +. y2)
			let ( -. ) (x1, y1) (x2, y2) = Pervasives.(x1 -. x2, y1 -. y2)
			let ( *. ) c (x, y) = Pervasives.(c *. x, c *. y)
			let ( /. ) (x, y) c = Pervasives.(x /. c, y /. c)
			let ( **. ) (x1, y1) (x2, y2) = Pervasives.(x1 *. x2 +. y1 *. y2)
			let ( ***. ) (x1, y1) (x2, y2) = Pervasives.(x1 *. x2, y1 *. y2)
			let ( ///. ) (x1, y1) (x2, y2) = Pervasives.(x1 /. x2, y1 /. y2)
		end

	let int_of_float (x, y) = (int_of_float x, int_of_float y)
	let float_of_int (x, y) = (float_of_int x, float_of_int y)

	let length (x, y) = sqrt (x**2. +. y**2.)
	let norm (x, y as v) = let l = length v in (x /. l, y /. l)
	let dist (x1, y1) (x2, y2) = sqrt ((x2 -. x1)**2. +. (y2 -. y1)**2.)

	let perpi (x, y) = (-y, x)
	let perpf (x, y) = (-.y, x)

	let all x = (x, x)
	let map f (x, y) = (f x, f y)
	let fold_left f a (x, y) = f (f a x) y
	let fold_right f (x, y) b = f x (f y b)
	let sumi (x, y) = x + y
	let sumf (x, y) = x +. y
	let prodi (x, y) = x * y
	let prodf (x, y) = x *. y

	include Operators
end

module ThreeD =
struct
	type 'a t = 'a * 'a * 'a

	module Operators =
		struct
			let ( + ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 + x2, y1 + y2, z1 + z2)
			let ( - ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 - x2, y1 - y2, z1 - z2)
			let ( * ) c (x, y, z) = Pervasives.(c * x, c * y, c * z)
			let ( / ) (x, y, z) c = Pervasives.(x / c, y / c, z / c)
			let ( ** ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 * x2 + y1 * y2 + z1 * z2)
			let ( *** ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 * x2, y1 * y2, z1 * z2)
			let ( /// ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 / x2, y1 / y2, z1 / z2)
			let ( +. ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 +. x2, y1 +. y2, z1 +. z2)
			let ( -. ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 -. x2, y1 -. y2, z1 -. z2)
			let ( *. ) c (x, y, z) = Pervasives.(c *. x, c *. y, c *. z)
			let ( /. ) (x, y, z) c = Pervasives.(x /. c, y /. c, z /. c)
			let ( **. ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 *. x2 +. y1 *. y2 +. z1 *. z2)
			let ( ***. ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 *. x2, y1 *. y2, z1 *. z2)
			let ( ///. ) (x1, y1, z1) (x2, y2, z2) = Pervasives.(x1 /. x2, y1 /. y2, z1 /. z2)
		end

	let int_of_float (x, y, z) = (int_of_float x, int_of_float y, int_of_float z)
	let float_of_int (x, y, z) = (float_of_int x, float_of_int y, float_of_int z)

	let length (x, y, z) = sqrt (x**2. +. y**2. +. z**2.)
	let norm (x, y, z as v) = let l = length v in (x /. l, y /. l, z /. l)
	let dist (x1, y1, z1) (x2, y2, z2) = sqrt ((x2 -. x1)**2. +. (y2 -. y1)**2. +. (z2 -. z1)**2.)

	let all x = (x, x, x)
	let map f (x, y, z) = (f x, f y, f z)
	let fold_left f a (x, y, z) = f (f (f a x) y) z
	let fold_right f (x, y, z) b = f x (f y (f z b))
	let sumi (x, y, z) = x + y + z
	let sumf (x, y, z) = x +. y +. z
	let prodi (x, y, z) = x * y * z
	let prodf (x, y, z) = x *. y *. z

	include Operators
end

module FourD =
struct
	type 'a t = 'a * 'a * 'a * 'a

	module Operators =
		struct
			let ( + ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 + w2, x1 + x2, y1 + y2, z1 + z2)
			let ( - ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 - w2, x1 - x2, y1 - y2, z1 - z2)
			let ( * ) c (w, x, y, z) = Pervasives.(c * w, c * x, c * y, c * z)
			let ( / ) (w, x, y, z) c = Pervasives.(c / w, x / c, y / c, z / c)
			let ( ** ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2)
			let ( *** ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 * w2, x1 * x2, y1 * y2, z1 * z2)
			let ( /// ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 / w2, x1 / x2, y1 / y2, z1 / z2)
			let ( +. ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 +. w2, x1 +. x2, y1 +. y2, z1 +. z2)
			let ( -. ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 -. w2, x1 -. x2, y1 -. y2, z1 -. z2)
			let ( *. ) c (w, x, y, z) = Pervasives.(c *. w, c *. x, c *. y, c *. z)
			let ( /. ) (w, x, y, z) c = Pervasives.(w /. c, x /. c, y /. c, z /. c)
			let ( **. ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 *. w2 +. x1 *. x2 +. y1 *. y2 +. z1 *. z2)
			let ( ***. ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 *. w2, x1 *. x2, y1 *. y2, z1 *. z2)
			let ( ///. ) (w1, x1, y1, z1) (w2, x2, y2, z2) = Pervasives.(w1 /. w2, x1 /. x2, y1 /. y2, z1 /. z2)
		end

	let int_of_float (w, x, y, z) = (int_of_float w, int_of_float x, int_of_float y, int_of_float z)
	let float_of_int (w, x, y, z) = (float_of_int w, float_of_int x, float_of_int y, float_of_int z)

	let length (w, x, y, z) = sqrt (w**2. +. x**2. +. y**2. +. z**2.)
	let norm (w, x, y, z as v) = let l = length v in (w /. l, x /. l, y /. l, z /. l)
	let dist (w1, x1, y1, z1) (w2, x2, y2, z2) = sqrt ((w2 -. w1)**2. +. (x2 -. x1)**2. +. (y2 -. y1)**2. +. (z2 -. z1)**2.)

	let all x = (x, x, x, x)
	let map f (w, x, y, z) = (f w, f x, f y, f z)
	let fold_left f a (w, x, y, z) = f (f (f (f a w) x) y) z
	let fold_right f (w, x, y, z) b = f w (f x (f y (f z b)))
	let sumi (w, x, y, z) = w + x + y + z
	let sumf (w, x, y, z) = w +. x +. y +. z
	let prodi (w, x, y, z) = w * x * y * z
	let prodf (w, x, y, z) = w *. x *. y *. z

	include Operators
end
