open OImages;;
open Images;;
open Rgb24;;



(*
#use "/usr/lib/ocaml/3.09.2/camlimages/images.mli";;
#use "/usr/lib/ocaml/3.09.2/camlimages/rgb24.mli";;
*)

exception Format;;
exception Color of (int * int * int);;


type links = bool * bool * bool * bool;;

type color = White | Black;;

type coords = int * int;;

class['a] region = object
  val nodes:('a Queue.t) = Queue.create ()
  val nbrs:(coords, 'a) Hashtbl.t
   = Hashtbl.create 127
  
  method nodes = nodes
  method nbrs = nbrs
end;;

(* type node = WhiteN of (links * region * White) | BlackN of (links * Black);; *)
type node = WhiteN of (links * node region) | BlackN of (links);;

(* type neighbour = node * coords *)



(* let factor = 0.75 *)

(* let mk_region () =
  { nodes=Queue.create (), nbrs=Hashtbl.create 127 () };; *)





let pix = 2;;

let load_img () =
  let file = "input.png" in
  let fmt, _ = Images.file_format file in
    print_endline (Images.extension fmt);
(*  match OImages.load file [] with
  | Rgb24 img -> img
  | _ -> raise Format *)
    let img = OImages.load file [] in
      (file, fmt, OImages.rgb24 img)
;;

let merge_regions target source =
  if target == source then ()
  else begin
    Queue.transfer source#nodes target#nodes;
(*    Hashtbl.iter
      (fun k v -> Hashtbl.add target#nbrs k v)
      source#nbrs *)
  end
;;

let adjust_region regions matrix (i, j) (k, l) =
  let node = matrix.(i).(j)
  and nbr = matrix.(k).(l)
  in
    match node, nbr with
    | WhiteN (links, source), WhiteN (_, target)  ->
	Hashtbl.remove regions (Oo.id source);
	Hashtbl.add regions (Oo.id target) target;
	merge_regions target source;
	matrix.(i).(j) <- WhiteN (links, target)
	  
    | BlackN (_), WhiteN (_, target)              ->
(*	Hashtbl.add target#nbrs (i, j) node *) ()

    | WhiteN (_, target), BlackN (_)              ->
(*	Hashtbl.add target#nbrs (k, l) node; *)
	Hashtbl.add regions (Oo.id target) target
	  
    | BlackN (_), BlackN (_)                      -> ()
;;

let adjust_links matrix i j =
  let w = (if i=0 then false else true) in
  let n = (if j=0 then false else true) in

  let node = matrix.(i).(j) in
  let node =
    if w then
    begin
      let wnode = matrix.(i-1).(j) in
      match (wnode, node) with
      | WhiteN _, WhiteN _
      | BlackN _, BlackN _ -> node
      | BlackN (wn, _, ws, ww), WhiteN ((n, e, s, _), region)
      | WhiteN ((wn, _, ws, ww), region), BlackN (n, e, s, _) ->
	  let wlinks = (wn, false, ws, ww) in
	  let links = (n, e, s, false) in
	  let (wnode, node) = match wnode with
	    | BlackN _ -> BlackN (wlinks), WhiteN (links, region)
	    | WhiteN _ -> WhiteN (wlinks, region), BlackN (links)
	  in matrix.(i-1).(j) <- wnode;
	  node
    end
    else node
  in 

  let node =
    if n then
    begin
      let nnode = matrix.(i).(j-1) in
      match (nnode, node) with
      | WhiteN _, WhiteN _
      | BlackN _, BlackN _ -> node
      | BlackN (nn, ne, _, nw), WhiteN ((_, e, s, w), region)
      | WhiteN ((nn, ne, _, nw), region), BlackN (_, e, s, w) ->
	  let nlinks = (nn, ne, false, nw) in
	  let links = (false, e, s, w) in
	  let (nnode, node) = match nnode with
	    | BlackN _ -> BlackN (nlinks), WhiteN (links, region)
	    | WhiteN _ -> WhiteN (nlinks, region), BlackN (links)
	  in matrix.(i).(j-1) <- nnode;
	  node
    end
    else node
  in matrix.(i).(j) <- node
;;

let build_regions (matrix: node array array) regions =
  let () = match matrix.(0).(0) with
  | WhiteN (_, first) -> 
      Hashtbl.add regions (Oo.id first) first
  | _ -> ()
  in
    
  Array.iteri (fun i arr ->
    Array.iteri (fun j _ ->
      if j>0 then 
	adjust_region regions matrix (i, j) (i, j-1);
      if i>0 then
	adjust_region regions matrix (i, j) (i-1, j);
      
      adjust_links matrix i j
    ) arr
  ) matrix
;;

(*let punch matrix =
()  
;;*)

let build () : string * Images.format * node array array * int * int =
  let (file, format, img) = load_img () in
  let (w, h) = (img#width, img#height) in
  let (w, h) = (w - (w mod pix), h - (h mod pix)) in
  
  let mk_cell x y =
(*    match x, y with
    | (w/pix, _) | (h/pix, _) -> ()
    | (_, _) -> *)

    let xfrom = x*pix in
    let xuntil = xfrom + pix in
    let yfrom = y*pix in
    let yuntil = yfrom + pix in
    
    let rec color i j white black =
      match i, j with
      | (x, y) when y=yuntil -> (white, black)
      | (x, y) when x=xuntil && y<yuntil -> color xfrom (j+1) white black
      | (_, _)       ->
	  let rgb = img#get i j in
	    match (rgb.r, rgb.g, rgb.b) with
	    | 0, 0, 0        -> color (i+1) j white (black+1)
	    | 255, 255, 255  -> color (i+1) j (white+1) black
	    | _, _, _        -> raise (Color (rgb.r, rgb.g, rgb.b))
    in
    let (wh, bl) = color xfrom yfrom 0 0 in
    let links = (yfrom>0, xuntil<w, yuntil<h, xfrom>0) in
      if (wh > bl)
      then
	let region = new region in
	let node = WhiteN (links, region) in
	Queue.push node region#nodes;
	node
      else BlackN (links)
  in
      
	
(*  let mk_subarr x = Array.init (h/pix) (mk_cell x) in *)
  print_endline ("starting array: " ^ (string_of_int (w/pix)) ^ ", " ^ (string_of_int (h/pix)));
  let matrix = Array.init (w/pix)
                          (fun x -> Array.init (h/pix) (mk_cell x))
  in print_endline ("ready array");
  let regions = Hashtbl.create 127 in
    build_regions matrix regions;
    print_endline "ready regions";
    (*punch matrix;*)
    (file, format, matrix, w/pix, h/pix)
;;
