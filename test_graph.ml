open Graph 
open Flow_network 

module G = MatrixGraph
module S = BFS(G)
module N = FlowNetworkMaker (G) (S)

let _ =
	let g : string G.graph = G.empty () in
	let source = G.source g in
	let sink = G.sink g in
	let (g, n1) = G.add_node g "N1" in
	let (g, n2) = G.add_node g "N2" in
	let (g, n3) = G.add_node g "N3" in
	let (g, n4) = G.add_node g "N4" in
	let g = G.connect g (source, n1) 100 in
	let g = G.connect g (source, n2) 100 in
	let g = G.connect g (n1, n3) 1 in
	let g = G.connect g (n2, n4) 1 in
	let g = G.connect g (n3, sink) 100 in
	let g = G.connect g (n4, sink) 100 in
	let min_cut = N.min_cut g in
	List.map (fun x -> match (G.contents g x) with
		| None -> Printf.printf "source/sink\n"
		| Some y -> Printf.printf "%s\n" y) min_cut