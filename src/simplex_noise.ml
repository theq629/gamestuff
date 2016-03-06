(*
Simplex noise.
Based on the public-domain Java code by Stefan Gustavson and Peter Eastman, which is available at http://www.itn.liu.se/~stegu/simplexnoise/SimplexNoise.java.
Note that use of simplex noise in some jurisdictions and applications is affected by USA patent 6867776.
*)

type random_source = unit -> float

let hash_size = 256

let make_perm random grad =
	let perm = Array.init (hash_size * 2) (fun i -> int_of_float (random i *. float_of_int (hash_size - 1))) in
	let perm_mod = Array.init (hash_size * 2) (fun i -> perm.(i) mod (Array.length grad)) in
		perm, perm_mod

let noise2d =
	let module Vec = Vectors.TwoD in
	let grad = [| (-1., -1.); (-1., 0.); (-1., 1.); (0., -1.); (0., 1.); (1., -1.); (1., 0.); (1., 1.); |] in
	let f = (sqrt 3. -. 1.) /. 2.
	and g = (3. -. sqrt 3.) /. 6. in
	let skew p =
		let s = Vec.sumf p *. f in
		Vec.(int_of_float (map floor (p +. all s))) in
	let unskew q =
		let t = float_of_int (Vec.sumi q) *. g in
		Vec.(float_of_int q -. all t) in
	let corner_offets_grid (x, y) =
		if x > y then 1, 0 else 0, 1 in
	let corner_offsets_world wc0 gc0 =
		Vec.(
			wc0 -. float_of_int gc0 +. all g,
			wc0 -. all 1. +. 2. *. all g
		) in
	let value perm perm_mod wp =
		let contrib p (i, j) (di, dj) =
			let t = 0.5 -. Vec.(p **. p) in
			if t < 0. then 0.
			else t**4. *. Vec.(grad.(perm_mod.(Pervasives.(i + di + perm.(j + dj)))) **. p) in
		let gp = skew wp in
		let wc0 = Vec.(wp -. unskew gp) in
		let gc0 = corner_offets_grid wc0 in
		let wc1, wc2 = corner_offsets_world wc0 gc0 in
		let modgp = Vec.map ((land) (hash_size - 1)) gp in
		70. *. (contrib wc0 modgp Vec.(all 0) +. contrib wc1 modgp gc0 +. contrib wc2 modgp Vec.(all 1)) in
	fun random ->
		let perm, perm_mod = make_perm random grad in
		value perm perm_mod

let noise3d =
	let module Vec = Vectors.ThreeD in
	let grad = [| -1., -1., 0.; -1., 1., 0.; 1., -1., 0.; 1., 1., 0.; -1., 0., -1.; -1., 0., 1.; 0., -1., -1.; 0., -1., 1.; 0., 1., -1.; 0., 1., 1.; 1., 0., -1.; 1., 0., 1. |] in
	let f = 1. /. 3.
	and g = 1. /. 6. in
	let skew p =
		let s = Vec.sumf p *. f in
		Vec.(int_of_float (map floor (p +. all s))) in
	let unskew q =
		let t = float_of_int (Vec.sumi q) *. g in
		Vec.(float_of_int q -. all t) in
	let corner_offets_grid (x0, y0, z0) =
		if x0 >= y0 then begin
			if y0 >= z0 then (1, 0, 0), (1, 1, 0)
			else if x0 >= z0 then (1, 0, 0), (1, 0, 1)
			else (0, 0, 1), (1, 0, 1)
		end else begin
			if y0 < z0 then (0, 0, 1), (0, 1, 1)
			else if x0 < z0 then (0, 1, 0), (0, 1, 1)
			else (0, 1, 0), (1, 1, 0)
		end in
	let corner_offsets_world wc0 gc0 gc1 =
		Vec.(
			wc0 -. float_of_int gc0 +. all g,
			wc0 -. float_of_int gc1 +. 2. *. all g,
			wc0 -. all 1. +. 3. *. all g
		) in
	let value perm perm_mod wp =
		let contrib p (i, j, k) (di, dj, dk) =
			let t = 0.6 -. Vec.(p **. p) in
			if t < 0. then 0.
			else t**4. *. Vec.(grad.(perm_mod.(Pervasives.(i + di + perm.(j + dj + perm.(k + dk))))) **. p) in
		let gp = skew wp in
		let wc0 = Vec.(wp -. unskew gp) in
		let gc0, gc1 = corner_offets_grid wc0 in
		let wc1, wc2, wc3 = corner_offsets_world wc0 gc0 gc1 in
		let modgp = Vec.map ((land) (hash_size - 1)) gp in
		32. *. (contrib wc0 modgp Vec.(all 0) +. contrib wc1 modgp gc0 +. contrib wc2 modgp gc1 +. contrib wc3 modgp Vec.(all 1)) in
	fun random ->
		let perm, perm_mod = make_perm random grad in
		value perm perm_mod

let noise4d =
	let module Vec = Vectors.FourD in
	let grad = [| -1., -1., -1., 0.; -1., -1., 1., 0.; -1., 1., -1., 0.; -1., 1., 1., 0.; 1., -1., -1., 0.; 1., -1., 1., 0.; 1., 1., -1., 0.; 1., 1., 1., 0.; -1., -1., 0., -1.; -1., -1., 0., 1.; -1., 0., -1., -1.; -1., 0., -1., 1.; -1., 0., 1., -1.; -1., 0., 1., 1.; -1., 1., 0., -1.; -1., 1., 0., 1.; 0., -1., -1., -1.; 0., -1., -1., 1.; 0., -1., 1., -1.; 0., -1., 1., 1.; 0., 1., -1., -1.; 0., 1., -1., 1.; 0., 1., 1., -1.; 0., 1., 1., 1.; 1., -1., 0., -1.; 1., -1., 0., 1.; 1., 0., -1., -1.; 1., 0., -1., 1.; 1., 0., 1., -1.; 1., 0., 1., 1.; 1., 1., 0., -1.; 1., 1., 0., 1. |] in
	let f = (sqrt 5. -. 1.) /. 4.
	and g = (5. -. sqrt 5.) /. 20. in
	let skew p =
		let s = Vec.sumf p *. f in
		Vec.(int_of_float (map floor (p +. all s))) in
	let unskew q =
		let t = float_of_int (Vec.sumi q) *. g in
		Vec.(float_of_int q -. all t) in
	let corner_offsets_grid (w0, x0, y0, z0) =
		let rank_w, rank_x, rank_y, rank_z =
			let w, x, y, z = ref 0, ref 0, ref 0, ref 0 in
			incr (if w0 > x0 then w else x);
			incr (if w0 > y0 then w else y);
			incr (if w0 > z0 then w else z);
			incr (if x0 > y0 then x else y);
			incr (if x0 > z0 then x else z);
			incr (if y0 > z0 then y else z);
			!w, !x, !y, !z in
		let make t =
			let coord c = if c >= t then 1 else 0 in
			(coord rank_w, coord rank_x, coord rank_y, coord rank_z) in
		(make 3, make 2, make 1) in
	let corner_offsets_world wc0 gc0 gc1 gc2 =
		Vec.(
			wc0 -. float_of_int gc0 +. all g,
			wc0 -. float_of_int gc1 +. 2. *. all g,
			wc0 -. float_of_int gc2 +. 3. *. all g,
			wc0 -. all 1. +. 4. *. all g
		) in
	let value perm perm_mod wp =
		let contrib p (h, i, j, k) (dh, di, dj, dk) =
			let t = 0.6 -. Vec.(p **. p) in
			if t < 0. then 0.
			else t**4. *. Vec.(grad.(perm_mod.(Pervasives.(h + dh + perm.(i + di + perm.(j + dj + perm.(k + dk)))))) **. p) in
		let gp = skew wp in
		let wc0 = Vec.(wp -. unskew gp) in
		let gc0, gc1, gc2 = corner_offsets_grid wc0 in
		let wc1, wc2, wc3, wc4 = corner_offsets_world wc0 gc0 gc1 gc2 in
		let modgp = Vec.map ((land) (hash_size - 1)) gp in
		27. *. (contrib wc0 modgp Vec.(all 0) +. contrib wc1 modgp gc0 +. contrib wc2 modgp gc1 +. contrib wc3 modgp gc2 +. contrib wc4 modgp Vec.(all 1)) in
	fun random ->
		let perm, perm_mod = make_perm random grad in
		value perm perm_mod
