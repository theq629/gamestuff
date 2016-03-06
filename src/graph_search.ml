(*
General graph search for any node type with a comparison function.
*)

open Containers

module type NODE =
	sig
		type t
		val compare : t -> t -> int
		val leq : t -> t -> bool
	end

module Make = functor (Node : NODE) ->
	struct
		module Node_map = Map.Make(Node)
		module Node_set = Set.Make(Node)

		(*
		 * Mutable fringe structure to manage distances and order of nodes.
		 *)
		module Fringe =
			struct
				type t =
					{
						dists : float Node_map.t ref;
						popped : Node_set.t ref;
						queue_insert : Node.t -> unit;
						queue_pop : unit -> Node.t option;
					}

				let make () =
					let dists = ref Node_map.empty in
					let popped = ref Node_set.empty in
					let known_dist n =
						match Node_map.get n !dists with
						| Some d -> d
						| None -> infinity in
					let module Node_ord =
						struct
							type t = Node.t
							let leq n1 n2 = (known_dist n1) <= (known_dist n2)
						end in
					let module Queue = CCHeap.Make(Node_ord) in
					let queue = ref Queue.empty in
					let queue_insert n =
						queue := Queue.insert n !queue in
					let rec queue_pop () =
						match Queue.take !queue with
						| None ->
							None
						| Some (rest, n) ->
							queue := rest;
							if Node_set.mem n !popped then begin
								(* we have to skip elements we've seen already since our queue doesn't enforce uniqueness *)
								queue_pop ()
							end else begin
								popped := Node_set.add n !popped;
								Some n
							end in
					{
						dists = dists;
						popped = popped;
						queue_insert = queue_insert;
						queue_pop = queue_pop;
					}

				let known_dist f n =
					match Node_map.get n !(f.dists) with
					| None -> infinity
					| Some d -> d

				let insert f n d =
					f.dists := Node_map.add n d !(f.dists);
					f.queue_insert n

				let pop f =
					f.queue_pop ()
			end

		let floodfill ~neighbours ~visit ~starts =
			let fringe = Fringe.make () in
			let rec run () =
				match Fringe.pop fringe with
				| None -> ()
				| Some n ->
					let dn = Fringe.known_dist fringe n in
					visit n dn;
					neighbours n dn begin fun m dnm ->
						let d = dn +. dnm in
						if d < Fringe.known_dist fringe m then
							Fringe.insert fringe m d
					end;
					run ()
				in
			List.iter begin fun n ->
				Fringe.insert fringe n 0.;
			end starts;
			run ()

		let dijkstra ~neighbours ~visit ~starts =
			let fringe = Fringe.make () in
			let prevs = ref Node_map.empty in
			let rec find_path path n =
				if List.mem n starts then n::path
				else
					let m =
						try Node_map.find n !prevs
						with Not_found -> assert false in
					find_path (n::path) m in
			let rec run () =
				match Fringe.pop fringe with
				| None -> None
				| Some n ->
					let dn = Fringe.known_dist fringe n in
					if visit n dn then begin
						Some (find_path [] n);
					end else begin
						neighbours n dn begin fun m dnm ->
							let d = dn +. dnm in
							if d < Fringe.known_dist fringe m then begin
								Fringe.insert fringe m d;
								prevs := Node_map.add m n !prevs
							end
						end;
						run ()
					end
				in
			List.iter begin fun n ->
				Fringe.insert fringe n 0.;
			end starts;
			run ()
	end
