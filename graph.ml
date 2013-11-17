(* Graph Algorithms *)

module type ADJ = 
  sig
    (* A graph represented as a mutable list of vertices *)
    type graph
    (* A graph vertex in the form (v, incoming, outgoing) *)
    type vertex
    (* An edge in the form source -> dest -> weight *)
    type edge
    val create : unit -> graph
    val is_empty : graph -> bool
    (* Adds a new vertex to a graph and returns the graph with an added vertex *)
    val add_vertex : graph -> int -> graph
  end

module Graph : ADJ = struct
  type vertex = V of int * ((vertex * int) list ref) * ((vertex * int) list ref) 
  type edge = vertex * vertex * int
  type graph = vertex list ref

  let create () = ref []

  let is_empty g =
    match !g with
    | [] -> true
    | _  -> false

  let vertex_find graph vertex = 
    let rec aux find g v =
      match g with 
      [] -> None
      | x::xs -> if v = x then Some(x) else find xs v
    in find !graph vertex

  let add_vertex graph v = 
    let new_vertex = V (v, ref [], ref [])
    in graph := new_vertex :: !graph;
    graph
end
