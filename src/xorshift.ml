(*
Xorshift random number source for fast but non-cryptographic purposes.
"Xorshift RNGs", George Marsaglia, 2003.
http://www.jstatsoft.org/v08/i14/paper
http://en.wikipedia.org/wiki/Xorshift
*)

module Period_64 =
	struct
		let shift_1 = 13
		let shift_2 = 17
		let shift_3 = 5

		type state = {
			mutable y : int32;
		}

		type 'a t = state -> 'a

		let gen s =
			let y1 = Int32.logxor s.y (Int32.shift_left s.y shift_1) in
			let y2 = Int32.logxor y1 (Int32.shift_right y1 shift_2) in
			s.y <- Int32.logxor y2 (Int32.shift_left y2 shift_3);
			s.y

		let make seed =
			assert (seed <> 0l);
			{
				y = seed;
			}

		let int max s =
			let x = (Int32.to_int (gen s)) in
			(x mod max + max) mod max

		let float s =
			let x = Int32.abs (gen s) in
			Int32.to_float x /. Int32.to_float Int32.max_int
	end

module Period_128 =
	struct
		let shift_x = 11
		let shift_w = 19
		let shift_t = 8

		type state = {
			mutable x : int32;
			mutable y : int32;
			mutable z : int32;
			mutable w : int32;
		}

		type 'a t = state -> 'a

		let gen s =
			let t = Int32.logxor s.x (Int32.shift_left s.x shift_x) in
			s.x <- s.y;
			s.y <- s.z;
			s.z <- s.w;
			s.w <- Int32.logxor
				(Int32.logxor s.w (Int32.shift_right s.w shift_w))
				(Int32.logxor t (Int32.shift_right t shift_t));
			s.w

		let make_complete seed_x seed_y seed_z seed_w =
			assert (seed_x <> 0l || seed_y <> 0l || seed_z <> 0l || seed_z <> 0l);
			let prime state =
				for i = 0 to 7 do
					ignore (gen state)
				done in
			let state =
				{
					x = seed_x;
					y = seed_y;
					z = seed_z;
					w = seed_w;
				} in
			prime state;
			state

		let make seed =
			let x = seed in
			let y = Int32.add x 1l in
			let z = Int32.add y 1l in
			let w = Int32.add z 1l in
			make_complete x y z w

		let int max s =
			let x = (Int32.to_int (gen s)) in
			(x mod max + max) mod max

		let float s =
			let x = Int32.abs (gen s) in
			Int32.to_float x /. Int32.to_float Int32.max_int
	end
