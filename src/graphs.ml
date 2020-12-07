let rec find x lst =
  match lst with
  | [] -> false
  | h :: t -> if x = h then true else find x t

let rec get_value2 arr_item =
  match arr_item with 
  | [] -> 0
  | n::[] -> n 
  | c::n -> get_value2 n;;

(* Define graph to be array of lists*)
class graph n =
  object (self)
    val graph_struct = Array.make n ([] : int list)
    method add_edge n1 n2 = 
      graph_struct.(n1) <- (n2 :: graph_struct.(n1))
    method is_edge n1 n2 =                           
      let result = find n2 graph_struct.(n1) in
      result 
    method size =                          
      n 
    method adjacents n1 = 
      let l = graph_struct.(n1) in 
      l 
  end;;
(* binary heap implementation *)
class binaryHeap n = 
  object (self)
    val arr = Array.make n 0
    val mutable n = 0
    method swap_arr i j = 
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    method push x =
      assert (n < Array.length arr);
      arr.(n) <- x;
      let parent ind = (ind - 1) / 2 in 
      let rec reorder = function
        | ind when arr.(ind) < arr.(parent ind) -> 
            self#swap_arr ind (parent ind);
            reorder(parent ind)
        | _ -> ()
      in reorder n;
      n <- n + 1
    method pop = 
      assert ( n > 0);
      let val_res = arr.(0) in 
      arr.(0) <- arr.(n - 1);
      n <- n - 1;
      let rec reorder ind =
        let ch = ind * 2 + 1 in
        (* use ()  as Nones to return when out of bounds, etc.*)
        if ch < n then
          (
            let c =
              if ch + 1 >= n then
                ch
              else if arr.(ch) > arr.(ch + 1) then
                ch + 1
              else
                ch
            in
            if arr.(ind) >= arr.(c) then
              (
                self#swap_arr ind c;
                reorder c
              )
            else 
              ()
          )
        else
          () 
      in reorder 0;
      val_res
  end;;
(* min pq with a decrement key option *)
class minPriorityQueue n =
  object (self)
    val arr = Array.make n []
    val mutable n = 0
    method swap_arr i j = 
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    method get_value arr_item =
      match arr_item with 
      | [] -> 0
      | c::n -> c
    method push x =
      assert (n < Array.length arr);
      arr.(n) <- x;
      let parent ind = (ind - 1) / 2 in 
      let rec reorder = function
        | ind when ((self#get_value arr.(ind)) < (self#get_value arr.(parent ind))) -> 
            self#swap_arr ind (parent ind);
            reorder(parent ind)
        | _ -> ()
      in reorder n;
      n <- n + 1
    method pop = 
      assert ( n > 0);
      let val_res = arr.(0) in 
      arr.(0) <- arr.(n - 1);
      n <- n - 1;
      let rec reorder ind =
        let ch = ind * 2 + 1 in
        if ch >= n then
          ()
        else (
          let c =
            if ch + 1 >= n then
              ch
            else if (self#get_value arr.(ch)) > (self#get_value arr.(ch + 1)) then
              ch + 1
            else
              ch
          in
          if (self#get_value arr.(ind)) < (self#get_value arr.(c)) then
            ()
          else (
            self#swap_arr ind c;
            reorder c
          )
        )
      in reorder 0;
      val_res
    method size =
      n
    method dec_key v prior=
      let rec find_arr arra val1 ind =
        if (ind >= Array.length arra) then 0
        else if (get_value2 arra.(ind) = val1) then ind 
        else find_arr arra val1 (ind+1)
      in let b = find_arr arr v 0 in
      arr.(b) <- [prior;v];
      let parent ind = (ind - 1) / 2 in 
      let rec reorder = function
        | ind when ((self#get_value arr.(ind)) < (self#get_value arr.(parent ind))) -> 
            self#swap_arr ind (parent ind);
            reorder(parent ind)
        | _ -> ()
      in reorder b;
  end;;




let dist_calc dist u v queue = 
  if (dist.(u) + 1 < dist.(v)) then
    dist.(v) <- dist.(u) + 1;
  queue#dec_key v (dist.(u) + 1);;


let rec process_adj adjs dist val1 queue = 
  match adjs with 
  | [] -> ()
  | v::rest -> (dist_calc dist val1 v queue;
                process_adj rest dist val1 queue);;

let rec process_queue graph src (queue:minPriorityQueue) dist =
  if queue#size <> 0 then (
    let val1 = queue#pop in
    let node = get_value2 val1 in
    let adjs = graph#adjacents node in
    process_adj adjs dist (get_value2 val1) queue;
    process_queue graph src queue dist
  )
  else
    dist;;

(* djisktra distances calculations; finds all distances 
   Only issue you have to intialze priority queue*)
let djisktra (graph : graph) n src (queue:minPriorityQueue)  = 
  let distance = Array.make n Int.max_int in
  distance.(src) <- 0;
  queue#dec_key src 0;
  process_queue graph src queue distance;
  distance ;; 
(*
Example inputs: 
let g = new graph 10;;
g#add_edge 0 1;;
g#add_edge 1 2;;
g#add_edge 2 4;;
g#add_edge 4 5;;
g#add_edge 5 3;; 
g#add_edge 2 3;;

let q = new minPriorityQueue 10;;
q#push [Int.max_int;1];;
q#push [Int.max_int;2];;
q#push [Int.max_int;3];;
q#push [Int.max_int;0];; 
q#push [Int.max_int;4];;
q#push [Int.max_int;5];; 
djisktra g 10 0 q;; 

Output: 
Distance to each point from 0 to that point (the index is the vertex number)
[|0; 1; 2; 3; 3; 4; 2147483647; 2147483647; 2147483647; 2147483647|]

Some amount of testing was done to check if this solution is correct.
*)