(*
Demo of simplex noise.
*)

module Col = Vectors.ThreeD

type output_mode = Bw of float | Grays of int | Colour of int * float Col.t
type noise_dimensions = Two | Three | Four

let output_bw oc noise dimx dimy threshold =
	Pbm.write_bw oc dimx dimy begin fun p ->
		noise p > threshold
	end

let output_grays oc noise dimx dimy max_val =
	Pbm.write_grays oc dimx dimy max_val begin fun p ->
		int_of_float (float_of_int max_val *. (1. +. noise p) /. 2.)
	end

let output_colour oc noise dimx dimy max_val colour =
	Pbm.write_colour oc dimx dimy max_val begin fun p ->
		let v = (1. +. noise p) /. 2. in
		let c = Col.(v *. colour) in
		Col.(int_of_float (Pervasives.float_of_int max_val *. c))
	end

let make_noise rng dims scales =
	let sum f = List.fold_left (fun s x -> s +. f x) 0. in
	let n = float_of_int (List.length scales) in
	let rand_src _ = Random.State.float rng 1. in
	match dims with
	| Two ->
		let raw = Simplex_noise.noise2d rand_src in
		fun p ->
			sum (fun s -> raw Vectors.TwoD.(s *. float_of_int p)) scales /. n
	| Three ->
		let raw = Simplex_noise.noise3d rand_src in
		fun (x, y) ->
			sum (fun s -> raw Vectors.ThreeD.(s *. float_of_int (x, y, 0))) scales /. n
	| Four ->
		let raw = Simplex_noise.noise4d rand_src in
		fun (x, y) ->
			sum (fun s -> raw Vectors.FourD.(s *. float_of_int (x, y, 0, 0))) scales /. n

let run oc rng dimx dimy noise_dims scales output_mode =
	let noise = make_noise rng noise_dims scales in
	match output_mode with
	| Bw threshold -> output_bw oc noise dimx dimy threshold
	| Grays max_val -> output_grays oc noise dimx dimy max_val
	| Colour (max_val, col) -> output_colour oc noise dimx dimy max_val col

let _ =
	let dimx = ref 0
	and dimy = ref 0
	and noise_dims = ref "2"
	and seed = ref 0
	and scales = ref []
	and output_mode_str = ref "grays"
	and max_pbm_val = ref 255
	and threshold = ref 0.
	and col_r, col_g, col_b = ref 0., ref 0., ref 1. in
	Args.(parse [
			"-s", Set_int seed,
				"seed";
			"-D", Symbol (["2"; "3"; "4"], fun s -> noise_dims := s),
				"number of dimensions of simplex noise (single slice shown if greater than two)";
			"-S", Float (fun s -> scales := s::!scales),
				"scaling for simplex noise";
			"-o", Symbol (["bw"; "grays"; "colour"], fun s -> output_mode_str := s),
				"output mode";
			"-t", Set_float threshold,
				"threshold for black and white output";
			"-c", Tuple [Set_float col_r; Set_float col_g; Set_float col_b],
				"base colour for colour output, as three values in [0, 1]";
			"-m", Set_int max_pbm_val,
				"maximum PBM grey/colour value";
		] [
			"dimx", Set_int dimx, "x dimension";
			"dimy", Set_int dimy, "y dimension";
		]);
	if List.length !scales == 0 then
		scales := [1.];
	let output_mode =
		match !output_mode_str with
		| "bw" -> Bw !threshold
		| "grays" -> Grays !max_pbm_val
		| "colour" -> Colour (!max_pbm_val, (!col_r, !col_g, !col_b))
		| _ -> assert false in
	let noise_dim =
		match !noise_dims with
	  | "2" -> Two
		| "3" -> Three
		| "4" -> Four
		| _ -> assert false in
	let rng = Random.State.make [| !seed |] in
	run stdout rng !dimx !dimy noise_dim !scales output_mode
