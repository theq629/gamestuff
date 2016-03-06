(*
Slightly improved argument parsing.
*)

include Arg

let spec_type_string spec =
	Arg.(match spec with
	| Bool _ -> Some "a boolean"
	| String _ -> Some "a string"
	| Set_string _ -> Some "a string"
	| Int _ -> Some "an integer"
	| Set_int _ -> Some "an integer"
	| Float _ -> Some "a float"
	| Set_float _ -> Some "a float"
	| Symbol (ss, _) -> Some ("one of: " ^ String.concat " " ss)
	| _ -> None
	)

let arg_error key spec arg =
	Arg.Bad (Printf.sprintf "wrong argument '%s'; argument '%s'%s"
			arg
			key
			(match (spec_type_string spec) with Some ts -> " expects " ^ ts | None -> "")
		)

let rec parse_spec key spec doc arg =
	try
		Arg.(match spec with
		| Unit f ->
			f ();
			[]
		| Bool f ->
			f (bool_of_string arg);
			[]
		| Set r ->
			r := true;
			[]
		| Clear r ->
			r := false;
			[]
		| String f ->
			f arg;
			[]
		| Set_string r ->
			r := arg;
			[]
		| Int f ->
			f (int_of_string arg);
			[]
		| Set_int r ->
			r := int_of_string arg;
			[]
		| Float f ->
			f (float_of_string arg);
			[]
		| Set_float r ->
			r := float_of_string arg;
			[]
		| Tuple [] ->
			[]
		| Tuple (first_sub_spec::sub_spec) ->
			parse_spec key first_sub_spec doc arg
			@ List.map (fun s -> key, s, doc) sub_spec
		| Symbol (ss, f) ->
			if not (List.mem arg ss) then
				raise (arg_error key spec arg);
			f arg;
			[]
		| Rest f ->
			f arg;
			[]
		)
	with Failure _ ->
		raise (arg_error key spec arg)

let make_anon_fun anon_spec =
	let rem_spec = ref anon_spec in
	let expect_done () =
		match !rem_spec with
		| [] -> ()
		| _ ->
			raise (Arg.Bad "not enough arguments") in
	let anon_fun arg =
		match !rem_spec with
		| (key, spec, doc)::rest ->
			let to_push = parse_spec key spec doc arg in
			rem_spec := to_push @ rest
		| [] ->
			raise (Arg.Bad "too many arguments") in
	anon_fun, expect_done

let make_usage_msg ?name argv opt_spec anon_spec =
	let anon_spec_strs = List.map (fun (n, _, _) -> n) anon_spec in
	let spec_strs =
		match opt_spec with
		| [] -> anon_spec_strs
		| _ -> "[options]" :: anon_spec_strs in
	let prog_name = 
		match name with
		| Some n -> n
		| None -> argv.(0) in
	String.concat " " ("usage:"::prog_name::spec_strs)

let wrap_parse ?name argv opt_spec anon_spec f =
	let show_error msg =
		let short_msg =
			try
				let i = String.index msg '\n' in
				String.sub msg 0 i
			with Not_found -> msg in
			prerr_string "error: ";
			prerr_string short_msg;
			prerr_newline () in
	let show_usage () =
		let full_spec = anon_spec @ opt_spec in
		let usage_msg = make_usage_msg ?name argv opt_spec anon_spec in
		prerr_string (Arg.usage_string full_spec usage_msg) in
	try
		let anon_fun, expect_done = make_anon_fun anon_spec in
		f anon_fun;
		expect_done ()
	with
	| (Arg.Help _) ->
		show_usage ();
		exit 0
	| (Arg.Bad m) ->
		show_error m;
		show_usage ();
		exit 2

let parse_argv ?name ?current argv opt_spec anon_spec =
	wrap_parse ?name argv opt_spec anon_spec begin fun anon_fun ->
		Arg.parse_argv ?current argv opt_spec anon_fun ""
	end

let parse_argv_dynamic ?name ?current argv opt_spec anon_spec =
	wrap_parse ?name argv !opt_spec anon_spec begin fun anon_fun ->
		Arg.parse_argv_dynamic ?current argv opt_spec anon_fun ""
	end

let parse ?name opt_spec anon_spec =
	parse_argv ?name Sys.argv opt_spec anon_spec

let parse_dynamic ?name opt_spec anon_spec =
	parse_argv_dynamic ?name Sys.argv opt_spec anon_spec
