open Graph

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

module type FlowNetwork = sig 
  type flow 
  type 'a graph
  type path
  type node
  val max_flow : 'a graph -> flow
  val min_cut : 'a graph -> node list
  val flow_capacity : 'a graph -> flow -> int
  val print : 'a graph -> unit
end 

module type PathSearch = sig 
  type path
  type 'a graph
  type node
  val find_path : 'a graph -> node -> (path, node list) either
end 

(** Implements BFS as a path finding procedure. This makes the max flow
    algorithm the Edmonds-Karp algorithm *)
module BFS (G : Graph) : PathSearch with type path = G.edge list and type node = G.node and type 'a graph = 'a G.graph = struct
  type 'a graph = 'a G.graph
  type node = G.node
  type edge = node * node
  type path = edge list

  let find_path (g : 'a graph) (source: node) : (path, node list) either = 
    (* places the backpointer path for the graph in previous *)
    let rec bfs (frontier : node Queue.t) (previous : (node,node) Hashtbl.t) (explored : (node, unit) Hashtbl.t) : unit = 
      let update_data parent f p candidate : unit = 
        if Hashtbl.mem p candidate then () else
          begin Queue.add candidate f;
            Hashtbl.add p candidate parent end in 
      if Queue.is_empty frontier then () else
        let cur = Queue.pop frontier in
        Hashtbl.replace explored cur ();
        G.neighbors g cur |> List.split |> fst |> List.iter (update_data cur frontier previous);
        if Hashtbl.mem previous (G.sink g) then () else
          bfs frontier previous explored in 
    (* transforms a list of nodes into a list of edges between them, but reverses order *)
    let rec edgify (acc : path) : node list -> path = function
      | n1 :: n2 :: tl -> edgify ((n1, n2) :: acc) (n2::tl)
      | _ -> acc in 
    let q = Queue.create () in 
    Queue.add source q;
    let parents = G.size g |> Hashtbl.create in
    let explored = G.size g |> Hashtbl.create in
    bfs q parents explored;
    let rec extract_path : node list -> node list = function
      | h :: t -> let prev = Hashtbl.find parents h in 
        if prev = source then prev :: h :: t else
          extract_path (prev :: h :: t)
      | _ -> failwith "impossible" in
    try begin 
      Left (extract_path [G.sink g] |> edgify [])
    end with Not_found -> (Right (Hashtbl.fold (fun k v ks -> k::ks) explored []))
end 

module FlowNetworkMaker (G : Graph) 
    (S : PathSearch with type path = G.edge list and type node = G.node and type 'a graph = 'a G.graph) 
  : FlowNetwork with type node = G.node and type 'a graph = 'a G.graph = struct
  type node = G.node
  type edge = G.edge
  type path = S.path
  type flow = (edge, int) Hashtbl.t
  type 'a graph = 'a G.graph

  let print = G.print 

  let reverse ((s,t) : edge) : edge = (t,s) 

  let min_capacity (g : 'a graph) (p : path) : int = 
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

  let update_flow_network (g : 'a graph) (delta : int) (p : path) : 'a graph = 
    let update_capacity (gf : 'a graph) (e : edge) : 'a graph = 
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

  let max_flow (g : 'a graph) : flow = 
    let rec helper (f : flow) (gf : 'a graph) : flow = 
      match S.find_path gf (G.source gf) with 
      | Right _ -> f
      | Left p -> let delta = min_capacity gf p in
        let gf' = update_flow_network gf delta p in
        helper (update_flow f p delta) gf' in
    G.copy g |> helper (Hashtbl.create (G.size g))

  let min_cut (g : 'a graph) : node list = 
    let rec helper (f : flow) (gf : 'a graph) : node list = 
      match S.find_path gf (G.source gf) with 
      | Right l -> l
      | Left p -> let delta = min_capacity gf p in
        let gf' = update_flow_network gf delta p in
        helper (update_flow f p delta) gf' in
    G.copy g |> helper (Hashtbl.create (G.size g))

  let flow_capacity (g : 'a graph) (f : flow) : int = 
    G.source g |> G.outgoing g |> 
    List.fold_left (fun acc x -> try acc + Hashtbl.find f x with Not_found -> acc) 0 
end