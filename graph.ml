(* Module type for accessing nodes and edges in a graph 
   Abstracted this way to allow easy swapping in and out 
   of implementations *)
module type Graph = sig
	(* types *)
	type node
	type 'a graph
	type edge = node * node

	(* Creates an empty graph, with an unconnected source and sink *)
	val empty : unit -> 'a graph

	(* Creates a new unconnected node in the graph *)
	val add_node : 'a graph -> 'a -> ('a graph * node)

	(* Connects node1 to node2 with capacity int in graph *)
	val connect : 'a graph -> edge -> int -> 'a graph

	(* Disconnects node1 to node2 in graph *)
	val disconnect : 'a graph -> edge -> 'a graph

	(* Returns the capacity of the edge from node 1 to node 2 in graph. 
	   Is 0 if the edge does not exist *)
	val capacity : 'a graph -> edge -> int 

	(* Returns a list of neighbors and their edge capacities of node in graph *)
	val neighbors : 'a graph -> node -> (node * int) list

	(* Returns a list of outgoing edges from a node *)
	val outgoing : 'a graph -> node -> edge list

	(* Number of nodes in the graph *)
	val size : 'a graph -> int

	(* Returns the source *)
	val source : 'a graph -> node

	(* Returns the sink *)
	val sink : 'a graph -> node

	(* Returns a list of the nodes in the graph *)
	val nodes : 'a graph -> node list

	(* Prints the graph *)
	val print : 'a graph -> unit

	(* Print node *)
	val print_node : node -> unit

	(* Copies the graph *)
	val copy : 'a graph -> 'a graph

	(* Returns the contents of a node (None if source or sink) *)
	val contents : 'a graph -> node -> 'a option
end 

(* Graph representation as an matrix:
   Value [n] at position [i],[j] means that
   capacity on edge (i,j) is n    *)
module MatrixGraph : Graph = struct
	type node = int
	type 'a matrix = 'a array array 
	type 'a graph = {
		mat: int matrix;
		conts: 'a option array }
	type edge = node * node

	let empty (_ : unit) : 'a graph =
		let source = [| 0; 0 |] in
		let sink = [| 0; 0 |] in {
			mat = [| source; sink |];
			conts = [| None; None |] }

	let add_node (g : 'a graph) (value: 'a) : 'a graph * node =
		let extend_with_zero (a : int array) : int array = Array.append a [| 0 |] in
		let old_size = Array.length g.mat.(0) in 
		let extended_graph = Array.map extend_with_zero g.mat in 
		let new_node = Array.make (old_size + 1) 0 in ({
			mat = Array.append extended_graph [|new_node|];
			conts = Array.append g.conts [| Some value |]; }, old_size)

	let size (g: 'a graph): int = g.mat |> Array.length 

	let outgoing (g : 'a graph) (from : node) : edge list =
		let out = ref [] in 
		let add_edge (dest : node) (c : int) : unit = 
			if c <= 0 then () else out := (from, dest) :: (!out) in
		Array.iteri add_edge g.mat.(from);
		!out

	let connect (g : 'a graph) ((from, dest) : edge) (c : int) : 'a graph =
		g.mat.(from).(dest) <- c; g

	let disconnect (g : 'a graph) ((from, dest) : edge) : 'a graph =
		g.mat.(from).(dest) <- 0; g

	let capacity (g : 'a graph) ((from, dest) : edge) : int =
		g.mat.(from).(dest)

	let source (_ : 'a graph) : node = 0

	let sink (_ : 'a graph) : node = 1

	let nodes (g : 'a graph) : node list =
		let rec list_0_n (n : int) (acc : int list) = 
			if n = 0 then acc else
			list_0_n (n - 1) ((n - 1) :: acc) in
		list_0_n (size g) []

	let neighbors (g : 'a graph) (n :node) : (node * int) list =
		g.mat.(n) |> Array.to_list |> List.combine (nodes g) |> List.filter (fun (_, c) -> c > 0) 

	let print (g : 'a graph) : unit = 
		let print_row (a : int array) : unit = Array.iter (Printf.printf "%d,") a; print_endline "" in
		Array.iter print_row g.mat

	let print_node n : unit = 
		if n = 0 then print_string "source"
		else if n = 1 then print_string "sink" 
		else print_int n 

	let copy (g : 'a graph): 'a graph = {
			mat = Array.map (Array.copy) g.mat;
			conts = Array.copy g.conts; }

	let contents (g: 'a graph) (n: node) : 'a option = g.conts.(n)
end
