(*
PBM image output.
*)

let write_bw oc dimx dimy f =
	output_string oc "P1\n";
	output_string oc (string_of_int dimx);
	output_char oc ' ';
	output_string oc (string_of_int dimy);
	output_char oc '\n';
	for y = 0 to dimy - 1 do
		for x = 0 to dimx - 1 do
			if x > 0 then
				output_char oc ' ';
			output_char oc (if f (x, y) then '1' else '0')
		done;
		output_char oc '\n'
	done

let write_grays oc dimx dimy max_val f =
	output_string oc "P2\n";
	output_string oc (string_of_int dimx);
	output_char oc ' ';
	output_string oc (string_of_int dimy);
	output_char oc '\n';
	output_string oc (string_of_int max_val);
	output_char oc '\n';
	for y = 0 to dimy - 1 do
		for x = 0 to dimx - 1 do
			if x > 0 then
				output_char oc ' ';
			output_string oc (string_of_int (f (x, y)))
		done;
		output_char oc '\n'
	done

let write_colour oc dimx dimy max_val f =
	output_string oc "P3\n";
	output_string oc (string_of_int dimx);
	output_char oc ' ';
	output_string oc (string_of_int dimy);
	output_char oc '\n';
	output_string oc (string_of_int max_val);
	output_char oc '\n';
	for y = 0 to dimy - 1 do
		for x = 0 to dimx - 1 do
			if x > 0 then
				output_char oc ' ';
			let r, g, b = f (x, y) in
			output_string oc (string_of_int r);
			output_char oc ' ';
			output_string oc (string_of_int g);
			output_char oc ' ';
			output_string oc (string_of_int b)
		done;
		output_char oc '\n'
	done
