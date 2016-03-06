(*
System for retryable computations where we might get stuck and need to backtrack.
*)

type 'a t = Ok of 'a | Failed

let retry ~max_tries f =
	let rec run tries =
		if tries < max_tries then begin
			match f () with
			| Ok x -> Ok x
			| Failed -> run (tries + 1)
		end else Failed in
	run 0

let map f r =
	match r with
	| Ok x -> Ok (f x)
	| Failed -> Failed

let return x =
	Ok x

let ( >>= ) r f =
	match r with
	| Ok x -> f x
	| Failed -> Failed
