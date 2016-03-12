module Make = functor (M : Tilemap.S) ->
	struct
		module Fringe =
			struct
				type 'a t =
					{
						last_seen : int M.t;
						scores : float M.t;
						indices : int M.t;
						locs : Tilemap.Location.t array;
						mutable size : int;
						mutable use_id : int;
					}

				let make tilemap =
					let dimx, dimy = M.dim tilemap in
					let area = dimx * dimy in
					{
						last_seen = M.map tilemap (fun _ _ -> min_int);
						scores = M.map tilemap (fun _ _ -> infinity);
						indices = M.map tilemap (fun _ _ -> -1);
						locs = Array.make area (0, 0);
						size = 0;
						use_id = min_int + 1;
					}

				let reset f =
					f.size <- 0;
					if f.use_id == max_int then begin
						M.update f.last_seen (fun _ _ -> min_int);
						f.use_id <- min_int
					end;
					f.use_id <- f.use_id + 1

				let rec heapify_down f i s =
					let go ci cs =
						if cs < s then
							let n = f.locs.(i) in
							let cn = f.locs.(ci) in
							f.locs.(i) <- cn;
							f.locs.(ci) <- n;
							M.set f.indices cn i;
							M.set f.indices n ci;
							heapify_down f ci s in
					let c1i = 2 * i + 1 in
					let c2i = c1i + 1 in
					if c1i >= f.size then begin
						if c2i < f.size then
							go c2i (M.get f.scores f.locs.(c2i))
					end else begin
						if c2i >= f.size then begin
							go c1i (M.get f.scores f.locs.(c1i))
						end else begin
							let c1s = M.get f.scores f.locs.(c1i)
							and c2s = M.get f.scores f.locs.(c2i) in
							if c1s <= c2s
								then go c1i c1s
								else go c2i c2s
						end
					end

				let rec fix_up f i s =
					let pi = (i - 1) / 2 in
					let pn = f.locs.(pi) in
					let ps = M.get f.scores pn in
					if ps > s then begin
						let n = f.locs.(i) in
						f.locs.(i) <- pn;
						f.locs.(pi) <- n;
						M.set f.indices pn i;
						M.set f.indices n pi;
						fix_up f pi s
					end

				let insert f n ns =
					let i =
						if M.get f.last_seen n < f.use_id then -1
						else M.get f.indices n in
					assert (i < f.size);
					M.set f.scores n ns;
					M.set f.last_seen n f.use_id;
					if i >= 0 then begin
						let ns = M.get f.scores n in
						heapify_down f i ns;
						fix_up f i ns;
					end else begin
						let i = f.size in
						f.size <- i + 1;
						f.locs.(i) <- n; 
						M.set f.indices n i;
						fix_up f i ns
					end

				let pop f =
					if f.size > 0 then begin
						let fn = f.locs.(0) in
						f.size <- f.size - 1;
						let ln = f.locs.(f.size) in
						f.locs.(0) <- ln;
						M.set f.indices ln 0;
						heapify_down f 0 (M.get f.scores ln);
						Some fn
					end else None

				let pop_score f =
					match pop f with
					| Some n -> Some (n, M.get f.scores n)
					| None -> None

				let known_dist f n =
					if M.get f.last_seen n < f.use_id then infinity
					else M.get f.scores n
			end

		let floodfill ~map =
			let fringe = Fringe.make map in
			fun ?(neighbour_weight=fun _ _ _ _ -> 1.) ~visit ~starts ->
				let rec run () =
					match Fringe.pop fringe with
					| None -> ()
					| Some n ->
						let dn = Fringe.known_dist fringe n in
						visit map n dn;
						M.neighbours map n begin fun m ->
							let d = dn +. neighbour_weight map n m dn in
							if d < Fringe.known_dist fringe m then
								Fringe.insert fringe m d
						end;
						run ()
					in
				Fringe.reset fringe;
				List.iter begin fun n ->
					Fringe.insert fringe n 0.;
				end starts;
				run ()

		let dijkstra ~map =
			let fringe = Fringe.make map in
			let start_prev_mark = (-1, -1) in
			let prevs = M.map map (fun _ _ -> start_prev_mark) in
			fun ?(neighbour_weight=fun _ _ _ _ -> 1.) ~visit ~starts ->
				let rec find_path path n =
					if n = start_prev_mark then path
					else find_path (n::path) (M.get prevs n) in
				let rec run () =
					match Fringe.pop fringe with
					| None -> None
					| Some n ->
						let dn = Fringe.known_dist fringe n in
						if visit map n dn then begin
							Some (find_path [] n);
						end else begin
							M.neighbours map n begin fun m ->
								let d = dn +. neighbour_weight map n m dn in
								if d < Fringe.known_dist fringe m then
									Fringe.insert fringe m d
							end;
							run ()
						end
					in
				Fringe.reset fringe;
				List.iter begin fun n ->
					Fringe.insert fringe n 0.;
					M.set prevs n start_prev_mark
				end starts;
				run ()
	end
