(*
Tilemap structures.
*)

module Location =
	struct
		type t = int * int

		let compare = compare
		let leq = ( < )
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

module Base =
	struct
		module Location = Location

		type 'a t =
			{
				dimx : int;
				dimy : int;
				cells : 'a array array;
			}

		let dim m =
			m.dimx, m.dimy

		let dimx m =
			m.dimx

		let dimy m =
			m.dimy
	end

module Square =
	struct
		include Base

		let max_neighbours = 8

		let is_boundary map (x, y) =
			x == 0 || x == map.dimx - 1 || y == 0 || y == map.dimy - 1

		let is_valid map (x, y) =
			x >= 0 && x < map.dimx && y >= 0 && y < map.dimy

		let init (dimx, dimy) f =
			let cells = Array.init dimx begin fun x ->
					Array.init dimy begin fun y ->
						f (x, y)
					end
				end in
			{
				dimx = dimx;
				dimy = dimy;
				cells = cells;
			}

		let map m f =
			let cells = Array.mapi begin fun x col ->
					Array.mapi begin fun y v ->
						f (x, y) v
					end col
				end m.cells in
			{
				dimx = m.dimx;
				dimy = m.dimy;
				cells = cells;
			}

		let update m f =
			for y = 0 to m.dimy - 1 do
				for x = 0 to m.dimx - 1 do
					m.cells.(x).(y) <- f (x, y) m.cells.(x).(y)
				done
			done

		let get m (x, y) =
			m.cells.(x).(y)

		let set m (x, y) v =
			m.cells.(x).(y) <- v

		let neighbours m (x, y) f =
			let max_x = m.dimx - 1 in
			begin
				let y1 = y - 1 in
				if y1 >= 0 then begin
					if x > 0 then
						f (x - 1, y1);
					f (x, y1);
					if x < max_x then
						f (x + 1, y1)
				end
			end;
			begin
				if x > 0 then
					f (x - 1, y);
				if x < max_x then
					f (x + 1, y)
			end;
			begin
				let y1 = y + 1 in
				if y1 < m.dimy then begin
					if x > 0 then
						f (x - 1, y1);
					f (x, y1);
					if x < max_x then
						f (x + 1, y1)
				end
			end

		let iter m f =
			for y = 0 to m.dimy - 1 do
				for x = 0 to m.dimx - 1 do
					f (x, y) m.cells.(x).(y)
				done
			done
	end

module Hex_horizontal =
	struct
		include Base

		let row_dimx dimx y =
			dimx - if y mod 2 == 0 then 0 else 1

		let max_neighbours = 6

		let is_boundary map (x, y) =
			x == 0 || x == row_dimx map.dimx y - 1 || y == 0 || y == map.dimy - 1

		let is_valid map (x, y) =
			x >= 0 && x < row_dimx map.dimx y && y >= 0 && y < map.dimy

		let init (dimx, dimy) f =
			let cells = Array.init dimy begin fun y ->
					Array.init (row_dimx dimx y) begin fun x ->
						f (x, y)
					end
				end in
			{
				dimx = dimx;
				dimy = dimy;
				cells = cells;
			}

		let map m f =
			let cells = Array.mapi begin fun y row ->
					Array.mapi begin fun x v ->
						f (x, y) v
					end row
				end m.cells in
			{
				dimx = m.dimx;
				dimy = m.dimy;
				cells = cells;
			}

		let update m f =
			for y = 0 to m.dimy - 1 do
				for x = 0 to row_dimx m.dimx y - 1 do
					m.cells.(y).(x) <- f (x, y) m.cells.(y).(x)
				done
			done

		let get m (x, y) =
			m.cells.(y).(x)

		let set m (x, y) v =
			m.cells.(y).(x) <- v

		let neighbours m (x, y) f =
			let par_y = y mod 2 in
			let max_x = row_dimx m.dimx y - 1 in
			begin
				let y1 = y - 1 in
				if y1 >= 0 then begin
					if par_y == 0 then begin
						if x > 0 then
							f (x - 1, y1);
						f (x, y1)
					end else begin
						f (x, y1);
						if x < max_x then
							f (x + 1, y1)
					end
				end
			end;
			begin
				if x > 0 then
					f (x - 1, y);
				if x < max_x then
					f (x + 1, y)
			end;
			begin
				let y1 = y + 1 in
				if y1 < m.dimy then begin
					if par_y == 0 then begin
						if x > 0 then
							f (x - 1, y1);
						f (x, y1)
					end else begin
						f (x, y1);
						if x < max_x then
							f (x + 1, y1)
					end
				end
			end

		let iter m f =
			for y = 0 to m.dimy - 1 do
				for x = 0 to row_dimx m.dimx y - 1 do
					f (x, y) m.cells.(x).(y)
				done
			done
	end

module Hex_vertical =
	struct
		include Base

		let row_dimy dimy x =
			dimy - if x mod 2 == 0 then 0 else 1

		let max_neighbours = 6

		let is_boundary map (x, y) =
			x == 0 || x == map.dimx - 1 || y == 0 || y == row_dimy map.dimy x - 1

		let is_valid map (x, y) =
			x >= 0 && x < map.dimx && y >= 0 && y < row_dimy map.dimy x

		let init (dimx, dimy) f =
			let cells = Array.init dimx begin fun x ->
					Array.init (row_dimy dimy x) begin fun y ->
						f (x, y)
					end
				end in
			{
				dimx = dimx;
				dimy = dimy;
				cells = cells;
			}

		let map m f =
			let cells = Array.mapi begin fun x col ->
					Array.mapi begin fun y v ->
						f (x, y) v
					end col
				end m.cells in
			{
				dimx = m.dimx;
				dimy = m.dimy;
				cells = cells;
			}

		let update m f =
			for x = 0 to m.dimx - 1 do
				for y = 0 to row_dimy m.dimy x - 1 do
					m.cells.(x).(y) <- f (x, y) m.cells.(x).(y)
				done
			done

		let get m (x, y) =
			m.cells.(x).(y)

		let set m (x, y) v =
			m.cells.(x).(y) <- v

		let neighbours m (y, x) f =
			let par_x = x mod 2 in
			let max_y = row_dimy m.dimy x - 1 in
			begin
				let y1 = x - 1 in
				if y1 >= 0 then begin
					if par_x == 0 then begin
						if y > 0 then
							f (y - 1, y1);
						f (y, y1)
					end else begin
						f (y, y1);
						if y < max_y then
							f (y + 1, y1)
					end
				end
			end;
			begin
				if y > 0 then
					f (y - 1, x);
				if y < max_y then
					f (y + 1, x)
			end;
			begin
				let y1 = x + 1 in
				if y1 < m.dimx then begin
					if par_x == 0 then begin
						if y > 0 then
							f (y - 1, y1);
						f (y, y1)
					end else begin
						f (y, y1);
						if y < max_y then
							f (y + 1, y1)
					end
				end
			end

		let iter m f =
			for x = 0 to m.dimx - 1 do
				for y = 0 to row_dimy m.dimy x - 1 do
					f (y, x) m.cells.(x).(y)
				done
			done
	end
