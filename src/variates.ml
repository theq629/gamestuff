(*
Random variates with various distributions.

The random variate generation is mostly based on chapter 29 of "The art of computer systems performance analysis" by Raj Jain, with some reference to "Non-uniform random variate generation" (web edition) by Luc Devroye.

TODO: This is mostly untested.
*)

module type SOURCE =
	sig
		type t
		val int : int -> t ->int
		val float : float -> t -> float
	end

module Stdlib_source =
	struct
		type t = Random.State.t
		let int bound src = Random.State.int src bound
		let float bound src = Random.State.float src bound
	end

let const v _ =
	v

module Make = functor (Source : SOURCE) ->
	struct
		module Source = Source

		module Uniform =
			struct
				let int a b =
					let d = b - a in
					if d <= 0 then invalid_arg "need a < b";
					let src_int = Source.int d in
					fun src ->
						a + src_int src

				let unit_float src =
					Source.float 1. src

				let float a b =
					let d = b -. a in
					if d <= 0. then invalid_arg "need a < b";
					let src_float = Source.float d in
					fun src ->
						a +. src_float src

				let bool =
					let src_float = Source.float 1. in
					fun src ->
						src_float src < 0.5

				let list_elt list =
					let len = List.length list in
					if len == 0 then invalid_arg "need non-empty list";
					let index = Source.int len in
					fun src ->
						List.nth list (index src)

				let array_elt array =
					let len = Array.length array in
					if len == 0 then invalid_arg "need non-empty array";
					let index = Source.int len in
					fun src ->
						array.(index src)
			end

			module Bernoulli =
				struct
					let values x y p =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						let u = Uniform.unit_float in
						fun src ->
							if u src < p then x else y

					let int = values 1 0

					let float = values 1. 0.

					let bool = values true false
				end

			module Empirical =
				struct
					let list_elt ~weight ~value list =
						let rec choose choice cum =
							function
							| [] -> raise Not_found
							| x::xs ->
								let w = weight x in
								let cum1 = cum +. w in
									if choice < cum1 then x
									else choose choice cum1 xs in
						let total = List.fold_left (fun s x -> s +. weight x) 0. list in
						let index = Source.float total in
						fun src ->
							value (choose (index src) 0. list)

					let array_elt ~weight ~value array =
						let len = Array.length array in
						let rec choose choice cum i =
							if i >= len then raise Not_found
							else
								let x = array.(i) in
								let w = weight x in
								let cum1 = cum +. w in
									if choice < cum1 then x
									else choose choice cum1 (i + 1) in
						let total = Array.fold_left (fun s x -> s +. weight x) 0. array in
						let index = Source.float total in
						fun src ->
							value (choose (index src) 0. 0)
				end

			module Normal =
				struct
					(* TODO: implement ziggurat algorithm? *)

					let float mean stddev =
						(* Polar Box-Muller *)
						if not (stddev > 0.) then invalid_arg "need stddev > 0";
						let u = Uniform.float (-1.) 1.
						and v = Uniform.float (-1.) 1. in
						let move z = z *. stddev +. mean in
						let rec choose_uniform src =
							let u = u src
							and v = v src in
							let s = u *. u +. v *. v in
							if s == 0. || s >= 1.
								then choose_uniform src
								else u, v, s in
						let cached_z = ref None in
						fun src ->
							match !cached_z with
							| Some z ->
								cached_z := None;
								move z
							| None ->
								let u, v, s = choose_uniform src in
								let d = sqrt (-2. *. log s /. s) in
								let z1 = u *. d
								and z2 = v *. d in
								cached_z := Some z1;
								move z2

					let bounded_float mean stddev a b =
						let base = float mean stddev in
						let rec choose src =
							let x = base src in
							if a <= x && x < b
								then x
								else choose src in
						choose
				end

			module Shifted_geometric =
				struct
					let float p =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						let u = Uniform.unit_float in
						let d = log (1. -. p) in
						fun src ->
							ceil (log (u src) /. d)

					let int p =
						let g = float p in
						fun src ->
							int_of_float (g src)
				end

			module Unshifted_geometric =
				struct
					let float p =
						let g = Shifted_geometric.float p in
						fun src ->
							g src -. 1.0

					let int p =
						let g = Shifted_geometric.int p in
						fun src ->
							g src - 1
				end

			module Pascal =
				struct
					let int p m =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						if not (m > 0) then invalid_arg "need m > 0";
						let g = Shifted_geometric.int p in
						let rec gen i t src =
							if i < m then gen (i + 1) (t + g src) src
							else t in
						gen 0 0

					let float p m =
						let int = int p m in
						fun src ->
							float_of_int (int src)
				end

			module Binomial =
				struct
					(* TODO: automatically choose which method to use? *)
					(* TODO: implement inverse transformation method like for Empirical? *)

					let int p n =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						if not (n > 0) then invalid_arg "need n > 0";
						let b = Bernoulli.int p in
						let rec gen i t src =
							if i < n then gen (i + 1) (t + b src) src
							else t in
						gen 0 0

					let float p n =
						let int = int p n in
						fun src ->
							float_of_int (int src)

					let float_small_p p n =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						if not (n > 0) then invalid_arg "need n > 0";
						let g = Shifted_geometric.float p in
						let rec gen i t src =
							if t <= float_of_int n then gen (i + 1) (t +. g src) src
							else float_of_int i -. 1. in
						gen 0 0.

					let int_small_p p n =
						let float = float_small_p p n in
						fun src ->
							let f = float src in
							assert (0. <= f && f <= 1.);
							assert ((float_of_int (int_of_float f)) = f);
							int_of_float f
				end

			module Poisson =
				struct
					(* TODO: implement inverse transformation method like for Empirical? *)

					let int lambda =
						if not (lambda > 0.) then invalid_arg "need lambda > 0";
						let u = Uniform.float 0. 1. in
						let rec gen i t src =
							if t < exp (-. lambda) then i
							else gen (i + 1) (t *. u src) src in
						gen 0 1.
				end

			module Negative_binomial =
				struct
					(* TODO: automatically choose which method to use? *)

					let int p m =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						if not (m > 0) then invalid_arg "need m > 0";
						let b = Bernoulli.int p in
						fun src ->
							let counts = [|0; 0|] in
							let rec run () =
								let i = b src in
								counts.(i) <- counts.(i) + 1 ;
								if counts.(1) < m then run () in
							run ();
							counts.(0)

					let float p n =
						let int = int p n in
						fun src -> float_of_int (int src)

					let float_small_p p m =
						if not (p >= 0. && p <= 1.) then invalid_arg "need 0 <= p <= 1";
						if not (m > 0) then invalid_arg "need m > 0";
						let g = Shifted_geometric.float p in
						let rec gen i t src =
							if i < m then gen (i + 1) (t +. g src) src
							else t in
						fun src ->
							gen 0 0. src -. float_of_int m

					let int_small_p p m =
						let float = float_small_p p m in
						fun src ->
							let f = float src in
							int_of_float f
				end

			module Exponential =
				struct
					let float a =
						if not (a > 0.) then invalid_arg "need a > 0";
						let ma = -1. *. a in
						let u = Source.float 1. in
						fun src ->
							ma *. log (u src)
				end

			module Erlang =
				struct
					let float a m =
						if not (a > 0.) then invalid_arg "need a > 0";
						if not (m > 0) then invalid_arg "need m > 0";
						let ma = -1. *. a in
						let u = Source.float 1. in
						fun src ->
							let rec run i prod =
								if i == 0 then prod
								else run (i - 1) (prod *. u src) in
							ma *. log (run m 1.)
				end

			let beta_small_args a b =
				let u = Uniform.unit_float in
				let rec gen src =
					let x = (u src)**(1. /. a) and y = (u src)**(1. /. b) in
						if x +. y > 1. then gen src
						else x /. (x +. y) in
					gen

			module Gamma =
				struct
					let rec float a b =
						if not (a > 0.) then invalid_arg "need a > 0";
						if not (b > 0.) then invalid_arg "need b > 0";
						let ib = float_of_int (int_of_float b) in
						if ib = b then begin
							Erlang.float a (int_of_float b)
						end else begin
							if b < 1. then
								let beta = beta_small_args b (1. -. b)
								and exp = Exponential.float 1. in
								fun src ->
									-. a *. beta src *. exp src
							else
								let int_gamma = float a ib
								and float_gamma = float a (b -. ib) in
								fun src ->
									int_gamma src +. float_gamma src
						end
				end

			module Beta =
				struct
					let float a b =
						if not (a > 0.) then invalid_arg "need a > 0";
						if not (b > 0.) then invalid_arg "need b > 0";
						if a < 1. && b < 1. then
							beta_small_args a b
						else
							let gamma_a = Gamma.float 1. a
							and gamma_b = Gamma.float 1. b in
							fun src ->
								gamma_a src /. (gamma_a src +. gamma_b src)
				end

			module Chi_squared =
				struct
					let rec float nu =
						if not (nu > 0) then invalid_arg "need nu > 0";
						if nu mod 2 == 0 then
							Gamma.float 0.5 (float_of_int (nu / 2))
						else
							let c = float (nu - 1)
							and n = Normal.float 0. 1. in
							fun src ->
								c src +. (n src)**2.
				end

			module F =
				struct
					let float n m =
						if not (n > 0) then invalid_arg "need n > 0";
						if not (m > 0) then invalid_arg "need m > 0";
						let chi_n = Chi_squared.float n
						and chi_m = Chi_squared.float m in
						fun src ->
							(chi_n src /. float_of_int n) /. (chi_m src /. float_of_int m)
				end

			module Lognormal =
				struct
					let float mean stddev =
						if not (mean > 0.) then invalid_arg "need mean > 0";
						if not (stddev > 0.) then invalid_arg "need stddev > 0";
						let n = Normal.float 0. 1. in
						fun src ->
							exp (mean +. stddev *. n src)
				end

			module Pareto =
				struct
					let float a =
						if not (a > 0.) then invalid_arg "need a > 0";
						let u = Uniform.float 0. 1. in
						fun src ->
							1. /. (u src ** (1. /. a))
				end

			module Students_t =
				struct
					(* TODO: can we approximate with normal for large nu (nu > 30) ? *)

					let float nu =
						if not (nu > 0) then invalid_arg "need nu > 0";
						let n = Normal.float 0. 1. in
						let cs = Chi_squared.float nu in
						fun src ->
							n src /. sqrt (cs src /. float_of_int nu)
				end

			module Weibull =
				struct
					let float a b =
						if not (a > 0.) then invalid_arg "need a > 0";
						if not (b > 0.) then invalid_arg "need b > 0";
						let u = Uniform.float 0. 1. in
						fun src ->
							a *. log (u src) ** (1. /. b)
				end

			module Zipf =
				struct
					let int a =
						(* "Non-Uniform Random Variate Generation", Luc Devroye, 1986. X.6.1 *)
						assert (a > 1.);
						let u = Source.float 1.
						and v = Source.float 1. in
						let a_m1 = a -. 1. in
						let a_m1_invm = -.1. /. a_m1 in
						let b = 2. ** a_m1 in
						let rec choose src =
							let u = u src in
							let v = v src in
							let xi = int_of_float (u ** a_m1_invm) in
							let x = float_of_int xi in
							let t = (1. +. 1. /. x) ** a_m1 in
							if v *. x *. ((t -. 1.) /. (b -. 1.)) <= (t /. b)
								then xi
								else choose src in
						choose
				end

			module Yule =
				struct
					let int a =
						(* "Non-Uniform Random Variate Generation", Luc Devroye, 1986. X.6.3 *)
						let e1 = Exponential.float 1. in
						let e2 = Exponential.float 1. in
						let a_nm1 = 1. -. a in
						fun src ->
							int_of_float (ceil (-1. *. e1 src /. log (1. -. exp (e2 src /. a_nm1))))
				end
	end
