module type Graph = sig
	type node
	type graph
	type edge = node * node
	val empty : graph
	val add_node : graph -> (graph * node)
	val connect : graph -> edge -> int -> graph
	val disconnect : graph -> edge -> graph
	val capacity : graph -> edge -> int 
	val neighbors : graph -> node -> (node * int) list
	val outgoing : graph -> node -> edge list
	val size : graph -> int
	val source : graph -> node
	val sink : graph -> node
	val nodes : graph -> node list
	val print : graph -> unit
	val print_node : node -> unit
	val copy : graph -> graph
end 

module type FlowNetwork = sig 
	type flow 
	type graph
	type path
	val make_graph : string list -> (string * string * int) list -> graph
	val max_flow : graph -> flow
 	val flow_capacity : graph -> flow -> int
 	val print : graph -> unit
end 

module type PathSearch = sig 
	type path
	type graph
	val find_path : graph -> path option
end 

(* Implements BFS as a path finding procedure. This makes the max flow
   algorithm the Edmonds-Karp algorithm *)
module BFS (G : Graph) : PathSearch with type path = G.edge list and type graph = G.graph = struct
	type graph = G.graph
	type node = G.node
	type edge = node * node
 	type path = edge list

	let find_path (g : graph) : path option = 
		(* places the backpointer path for the graph in previous *)
 		let rec bfs (frontier : node Queue.t) (previous : (node,node) Hashtbl.t) : unit = 
 			let update_data parent f p candidate : unit = 
 				if Hashtbl.mem p candidate then () else
 				begin Queue.add candidate f;
 				Hashtbl.add p candidate parent end in 
 			if Queue.is_empty frontier then () else
 			let cur = Queue.pop frontier in
 			G.neighbors g cur |> List.split |> fst |> List.iter (update_data cur frontier previous);
 			if Hashtbl.mem previous (G.sink g) then () else
 			bfs frontier previous in 
 		(* transforms a list of nodes into a list of edges between them, but reverses order *)
 		let rec edgify (acc : path) : node list -> path = function
 			| n1 :: n2 :: tl -> edgify ((n1, n2) :: acc) (n2::tl)
 			| _ -> acc in 
 		let q = Queue.create () in 
 		Queue.add (G.source g) q;
 		let parents = G.size g |> Hashtbl.create in
 		bfs q parents;
 		let rec extract_path : node list -> node list = function
 			| h :: t -> let prev = Hashtbl.find parents h in 
 						if prev = G.source g then prev :: h :: t else
 						extract_path (prev :: h :: t)
 			| _ -> failwith "impossible" in
 		try begin 
 			Some (extract_path [G.sink g] |> edgify [])
 		end with Not_found -> None
end 

(* Implements DFS as a path finding procedure. This makes the max flow
   algorithm the Ford-Fulkerson algorithm *)
module DFS (G : Graph) : PathSearch with type path = G.edge list and type graph = G.graph = struct
	type graph = G.graph
	type node = G.node
	type edge = node * node
 	type path = edge list

	let find_path (g : graph) : path option = 
		(* places the backpointer path for the graph in previous *)
 		let rec dfs (frontier : node Stack.t) (previous : (node,node) Hashtbl.t) : unit = 
 			let update_data parent f p candidate : unit = 
 				if Hashtbl.mem p candidate then () else
 				begin Stack.push candidate f;
 				Hashtbl.add p candidate parent end in 
 			if Stack.is_empty frontier then () else
 			let cur = Stack.pop frontier in
 			G.neighbors g cur |> List.split |> fst |> List.iter (update_data cur frontier previous);
 			if Hashtbl.mem previous (G.sink g) then () else
 			dfs frontier previous in 
 		(* transforms a list of nodes into a list of edges between them, but reverses order *)
 		let rec edgify (acc : path) : node list -> path = function
 			| n1 :: n2 :: tl -> edgify ((n1, n2) :: acc) (n2::tl)
 			| _ -> acc in 
 		let q = Stack.create () in 
 		Stack.push (G.source g) q;
 		let parents = G.size g |> Hashtbl.create in
 		dfs q parents;
 		let rec extract_path : node list -> node list = function
 			| h :: t -> let prev = Hashtbl.find parents h in 
 						if prev = G.source g then prev :: h :: t else
 						extract_path (prev :: h :: t)
 			| _ -> failwith "impossible" in
 		try begin 
 			Some (extract_path [G.sink g] |> edgify [])
 		end with Not_found -> None
end 

module FlowNetworkMaker (G : Graph) 
		(S : PathSearch with type path = G.edge list and type graph = G.graph) 
		: FlowNetwork with type graph = G.graph = struct
	type node = G.node
 	type edge = G.edge
 	type path = S.path
 	type flow = (edge, int) Hashtbl.t
 	type graph = G.graph

 	(* Note: name of source must be 'source', and name of sink must be 'sink'. Thus, v
 	   should be all nodes not including the source and the sink *)
 	let make_graph (v : string list) (e : (string * string * int) list) : graph = 
 		let add_by_name (g, lst) name = 
 			let g', n = G.add_node g in (g', (name, n) :: lst) in
 		let add_all_by_name (g : graph) : graph * ((string * node) list) =
 			let g', name_map = List.fold_left add_by_name (g, []) v in 
 			(g', ("source", G.source g) :: ("sink", G.sink g) :: name_map) in
 		let net, name_map = add_all_by_name G.empty in 
 		let add_edge_by_name g (name1, name2, c) =
 			let e = (List.assoc name1 name_map, List.assoc name2 name_map) in 
 			G.connect g e c in
 		List.fold_left add_edge_by_name net e

 	let print = G.print 

 	let reverse ((s,t) : edge) : edge = (t,s) 

 	let min_capacity (g : graph) (p : path) : int = 
 		let minc (c : int) (e : edge) : int = 
 			G.capacity g e |> min c in
 		List.fold_left minc max_int p 

 	let update_flow (f : flow) (p : path) (delta : int) = 
 		let update (fl : flow) (e : edge) : flow = 
 			let old = try (Hashtbl.find fl e)
 					  with Not_found -> 0 in
 			let old_back = try (reverse e |> Hashtbl.find fl)
 					  	   with Not_found -> 0 in
 		 	Hashtbl.replace fl e (old + delta);
 		 	Hashtbl.replace fl (reverse e) (old_back - delta);
 		 	fl in
 		List.fold_left update f p

	let update_flow_network (g : graph) (delta : int) (p : path) : graph = 
		let update_capacity (gf : graph) (e : edge) : graph = 
			let old = G.capacity gf e in 
			let old_back = G.capacity gf (reverse e) in
			let gf' = G.connect gf e (old - delta) in
			G.connect gf' (reverse e) (old_back + delta) in
		List.fold_left update_capacity g p

	let print_edge (n1,n2) : unit = 
		print_string "(";
		G.print_node n1;
		print_string ",";
		G.print_node n2;
		print_string ")"

	let print_path p : unit = List.iter (print_edge) p; print_endline ""

 	let max_flow (g : graph) : flow = 
 		let rec helper (f : flow) (gf : graph) : flow = 
 			match S.find_path gf with 
 			| None -> f
 			| Some p -> let delta = min_capacity gf p in
 						let gf' = update_flow_network gf delta p in
	 					helper (update_flow f p delta) gf' in
 		G.copy g |> helper (Hashtbl.create (G.size g))

 	let flow_capacity (g : graph) (f : flow) : int = 
 		G.source g |> G.outgoing g |> 
 		List.fold_left (fun acc x -> try acc + Hashtbl.find f x with Not_found -> acc) 0 
end