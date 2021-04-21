(* Module type for accessing nodes and edges in a graph 
   Abstracted this way to allow easy swapping in and out 
   of implementations *)
module type Graph = sig
	(* types *)
	type node
	type graph
	type edge = node * node

	(* Creates an empty graph, with an unconnected source and sink *)
	val empty : graph

	(* Creates a new unconnected node in the graph *)
	val add_node : graph -> (graph * node)

	(* Connects node1 to node2 with capacity int in graph *)
	val connect : graph -> edge -> int -> graph

	(* Disconnects node1 to node2 in graph *)
	val disconnect : graph -> edge -> graph

	(* Returns the capacity of the edge from node 1 to node 2 in graph. 
	   Is 0 if the edge does not exist *)
	val capacity : graph -> edge -> int 

	(* Returns a list of neighbors and their edge capacities of node in graph *)
	val neighbors : graph -> node -> (node * int) list

	(* Returns a list of outgoing edges from a node *)
	val outgoing : graph -> node -> edge list

	(* Number of nodes in the graph *)
	val size : graph -> int

	(* Returns the source *)
	val source : graph -> node

	(* Returns the sink *)
	val sink : graph -> node

	(* Returns a list of the nodes in the graph *)
	val nodes : graph -> node list

	(* Prints the graph *)
	val print : graph -> unit

	(* Print node *)
	val print_node : node -> unit

	(* Copies the graph *)
	val copy : graph -> graph
end 

(* Graph representation as an matrix:
   Value [n] at position [i],[j] means that
   capacity on edge (i,j) is n    *)
module MatrixGraph : Graph = struct
	type node = int
	type 'a matrix = 'a array array 
	type graph = int matrix
	type edge = node * node

	let (empty : graph) =
		let source = [| 0; 0 |] in 
		let sink = [| 0; 0 |] in
		[| source; sink |]

	let add_node (g : graph) : graph * node =
		let extend_with_zero (a : int array) : int array = Array.append a [| 0 |] in
		let old_size = Array.length g.(0) in 
		let extended_graph = Array.map extend_with_zero g in 
		let new_node = Array.make (old_size + 1) 0 in
		(Array.append extended_graph [|new_node|], old_size)

	let size : graph -> int = Array.length

	let outgoing (g : graph) (from : node) : edge list =
		let out = ref [] in 
		let add_edge (dest : node) (c : int) : unit = 
			if c <= 0 then () else out := (from, dest) :: (!out) in
		Array.iteri add_edge g.(from);
		!out

	let connect (g : graph) ((from, dest) : edge) (c : int) : graph =
		g.(from).(dest) <- c; g

	let disconnect (g : graph) ((from, dest) : edge) : graph =
		g.(from).(dest) <- 0; g

	let capacity (g : graph) ((from, dest) : edge) : int =
		g.(from).(dest)

	let source (_ : graph) : node = 0

	let sink (_ : graph) : node = 1

	let nodes (g : graph) : node list =
		let rec list_0_n (n : int) (acc : int list) = 
			if n = 0 then acc else
			list_0_n (n - 1) ((n - 1) :: acc) in
		list_0_n (size g) []

	let neighbors (g : graph) (n :node) : (node * int) list =
		g.(n) |> Array.to_list |> List.combine (nodes g) |> List.filter (fun (_, c) -> c > 0) 

	let print : graph -> unit = 
		let print_row (a : int array) : unit = Array.iter (Printf.printf "%d,") a; print_endline "" in
		Array.iter print_row

	let print_node n : unit = 
		if n = 0 then print_string "source"
		else if n = 1 then print_string "sink" 
		else print_int n 

	let copy : graph -> graph = 
		Array.map (Array.copy)
end

(* Graph representation as a list of lists 
   This is the idiomatic way, but it's slow *)

module NaiveGraph : Graph = struct
	type node = string
	type graph = (node * ((node * int) list)) list
	type edge = node * node

	let empty = [("source", []); ("sink", [])] 

	let add_node : graph -> (graph * node) =
		let ncounter = ref 0 in 
		fun (g : graph) -> 
			let n = string_of_int !ncounter in 
			ncounter := !ncounter + 1;
			(n, []) :: g, n	

	let neighbors (g : graph) (n : node) : (node * int) list = List.assoc n g

	let disconnect (g : graph) ((from, dest) : edge) : graph = 
		let ns = neighbors g from |> List.remove_assoc dest in
		(from, ns) :: (List.remove_assoc from g)

	let connect (g : graph) ((from, dest) : edge) (c : int) : graph = 
		if c <= 0 then disconnect g (from, dest) else
		let ns = neighbors g from |> List.remove_assoc dest in
		(from, (dest, c) :: ns) :: (List.remove_assoc from g)

	let capacity (g : graph) ((from, dest) : edge) : int = 
		try neighbors g from |> List.assoc dest with Not_found -> 0

	let outgoing (g : graph) (n : node) : edge list = 
		neighbors g n |> List.split |> fst |> List.map (fun d -> (n,d))

	let size : graph -> int = List.length

	let source (_ : graph) : node = "source"

	let sink (_ : graph) : node = "sink"

	let print_node : node -> unit = print_string

	let print (g : graph) : unit = 
		let print_node_with_edges (n, ns) = 
			Printf.printf "%s\n" n;
			List.iter (fun (n', c) -> Printf.printf "(%s,%d)" n' c) ns;
			print_endline "" in
		List.iter print_node_with_edges g

	let copy (g : graph) : graph = g

	let nodes (g : graph) : node list = List.split g |> fst
end 

(* Graph representation as an array:
   Node [i] is at position i in the array, and 
   contains a list of (node, capacity) pairs 
   representing outgoing edges*)

module ArrayGraph : Graph = struct
	type node = int
	type graph = ((node * int) list) array
	type edge = node * node

	let empty = [|[];[]|]

	let size : graph -> int = Array.length

	let add_node (g : graph) : (graph * node) =
		let num = size g in
		Array.append g [|[]|], num

	let disconnect (g : graph) ((from, dest) : edge) : graph = 
		g.(from) <- List.remove_assoc dest g.(from); 
		g

	let connect (g : graph) ((from, dest) : edge) (c :  int) : graph = 
	 	let g' = disconnect g (from, dest) in
		begin if c > 0 then g'.(from) <- (dest, c) :: g'.(from) end; 
		g'

	let capacity (g : graph) ((from, dest) : edge) : int = 
		try List.assoc dest g.(from) with Not_found -> 0

	let neighbors : graph -> node -> (node * int) list = Array.get

	let outgoing (g : graph) (n : node) : edge list = 
		g.(n) |> List.split |> fst |> List.map (fun d -> (n, d)) 

	let source (_ : graph) : node = 0

	let sink (_ : graph) : node = 1

	let print_node (n : node) : unit = 
		if n = 0 then print_string "source"
		else if n = 1 then print_string "sink" 
		else print_int n 

	let print (g : graph) : unit = 
		let print_edge (n, c) = 
			print_string "("; print_node n; print_string ","; print_int c; print_string ")" in
		let print_node_with_edges (n : node) e =
			print_node n; print_endline "";
			List.iter print_edge e; print_endline "" in
		Array.iteri print_node_with_edges g

	let copy : graph -> graph = Array.copy

	let nodes (g : graph) : node list = 
		let rec list_0_n (n : int) (acc : int list) = 
			if n = 0 then acc else
			list_0_n (n - 1) ((n - 1) :: acc) in
		list_0_n (size g) []
end 