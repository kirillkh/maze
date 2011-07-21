open Build;;
open Color;;

(*
type links = bool * bool * bool * bool

type color = White | Black

type banks = bank list
and bank = { nodes:node list, n:banks, e:banks, s:banks, w:banks }
and node = WhiteN of (links * bank) | BlackN of (links * bank)

let create_bank () =
  { nodes=[], n=[], e=[], s=[], w=[] };;
*)

exception Dimensions of (int * int);;

Random.init 1234567;;


type offset = (int -> int * int);;
type fetcher = (int -> node);;

let build_fetcher matrix (offs:offset) =
  fun times ->
    let (x, y) = offs times in matrix.(x).(y)
;;


let draw_line (color: node) (lim: int) (toggle_mark: int->unit)
              (is_mine: int -> bool) (fetch:fetcher) : int =
  let punch from upto =
    let pos = from + (Random.int (upto-from)) in
      toggle_mark pos
  in
  
  let rec draw pos first_span span_start was_mine :int =
    if pos = lim then
      if first_span=(-1) then pos
      else if was_mine then
	(punch span_start pos; first_span)
      else first_span
    else

      if (is_mine pos) then (
	  let span_start =
	    if was_mine then span_start else pos
	  in
	    (toggle_mark pos;
	    draw (pos + 1) first_span span_start true)
      ) else (
	  if first_span=(-1) then
(*	    if was_mine then  *)
	      draw (pos + 1) pos (-1) false
(*	    else 
	      draw (pos + 1) first_span (-1) false  *)
	  else
	    let _ =
	      if was_mine then punch span_start pos
	    in
	      draw (pos + 1) first_span (-1) false
      )
  in draw 0 (-1) 0 true

      (*
      match (fetch pos), color with
      | WhiteN _, BlackN _   | BlackN _, WhiteN _  ->
	  if first_span=(-1) then
(*	    if was_mine then  *)
	      draw (pos + 1) pos (-1) false
(*	    else 
	      draw (pos + 1) first_span (-1) false  *)
	  else
	    let _ =
	      if was_mine then punch span_start pos
	    in
	      draw (pos + 1) first_span (-1) false
      | WhiteN _, WhiteN _   | BlackN _, BlackN _   ->
	  let span_start =
	    if was_mine then span_start else pos
	  in
	    (toggle_mark pos;
	    draw (pos + 1) first_span span_start true)
  in draw 0 (-1) 0 true
     *)
;;

let rec poly (matrix: node array array) color (xfrom, xto) (yfrom, yto) store path lvl =
  let (wid, hei) = (xto - xfrom, yto - yfrom) in
    match wid, hei with
    | w, h when (w<1 or h<1) -> raise (Dimensions (w, h))
    | 1, _   | _, 1          -> ()
    | _, _                   ->
	let (xsum, ysum) = (xfrom + xto), (yfrom + yto) in
	let cx = (xsum/2) + (xsum mod 2) in
	let cy = (ysum/2) + (ysum mod 2) in
	
	let toggle_mark (select_mark: links -> links) x y =
	  let node = matrix.(x).(y) in
	  let new_node = match node with
	  | WhiteN (links, region) ->
	      WhiteN (select_mark links, region)
	  | BlackN links ->
	      BlackN (select_mark links)
	  in matrix.(x).(y) <- new_node
	in
	let toggle_hor (n, e, s, w) = (not n, e, s, w) in
	let toggle_ver (n, e, s, w) = (n, e, s, not w) in

	let is_mine impl fetch pos =
	  let node = fetch pos in
	    match node, color with
	    | WhiteN _, BlackN _
	    | BlackN _, WhiteN _  ->
		false
	    | WhiteN (links, _), WhiteN _
	    | BlackN links, BlackN _ ->
		impl links
	in
	let ism_hor (n, e, s, w) = (n=true) in
	let ism_ver (n, e, s, w) = (w=true) in

	
	let togn pos =  toggle_mark toggle_ver cx (cy - pos - 1) in
	let fetch pos = matrix.(cx).(cy - pos - 1) in
	let nspan = draw_line color (cy - yfrom) togn (is_mine ism_ver fetch) fetch
	  
	in
	let toge pos =  toggle_mark toggle_hor (cx + pos) cy in
	let fetch pos = matrix.(cx + pos).(cy) in
	let espan = draw_line color (xto - cx) toge (is_mine ism_hor fetch) fetch
	  
	in
	let togs pos =  toggle_mark toggle_ver cx (cy + pos) in
	let fetch pos = matrix.(cx).(cy + pos) in
	let sspan = draw_line color (yto - cy) togs (is_mine ism_ver fetch) fetch
	  
	in
	let togw pos =  toggle_mark toggle_hor (cx - pos - 1) cy in
	let fetch pos = matrix.(cx - pos - 1).(cy) in
	let wspan = draw_line color (cx - xfrom) togw (is_mine ism_hor fetch) fetch
	  
	
	in
	  if (nspan=0 || espan=0 || sspan=0 || wspan=0) then begin
	    (*if (nspan > 0) then begin let i = (Random.int nspan) in togn i; print_endline (string_of_int i) end;*)
	    if (nspan > 0) then togn (Random.int nspan) else print_endline ((string_of_int lvl) ^","^ (string_of_int nspan));
	    if (espan > 0) then toge (Random.int espan);
	    if (sspan > 0) then togs (Random.int sspan);
	    if (wspan > 0) then togw (Random.int wspan);
	  end
	  else begin
	    let out = (Random.int 4) in
	    let (nout, eout, sout, wout) = (out=0, out=1, out=2, out=3) in
	      if (not nout) then togn (Random.int nspan);
	      if (not eout) then toge (Random.int espan);
	      if (not sout) then togs (Random.int sspan);
	      if (not wout) then togw (Random.int wspan);
	  end;

	  store path lvl;
	  
	  poly matrix color (xfrom, cx) (yfrom, cy)   store (path(*^"1"*)) (lvl+1);
	  poly matrix color (cx, xto) (yfrom, cy)   store (path(*^"2"*)) (lvl+1);
	  poly matrix color (cx, xto) (cy, yto)   store (path(*^"3"*)) (lvl+1);
	  poly matrix color (xfrom, cx) (cy, yto)   store (path(*^"4"*)) (lvl+1);
	  (*
	  poly matrix color (xfrom, cx) (yfrom, cy)   store (path^"1") (lvl+1);
	  poly matrix color (cx, xto) (yfrom, cy)   store (path^"2") (lvl+1);
	  poly matrix color (cx, xto) (cy, yto)   store (path^"3") (lvl+1);
	  poly matrix color (xfrom, cx) (cy, yto)   store (path^"4") (lvl+1); *)
	  
;;


let pix = Build.pix;;
let ln_wid = 1;;

let picture matrix =
  let black = { r=0; g=0; b=0 } in
  let white = { r=255; g=255; b=255 } in
  let (wid, hei) = (Array.length matrix), (Array.length matrix.(0)) in
  let img = new OImages.rgb24_filled (wid*pix+1) (hei*pix+1) white in
(*  and white = { r=255, g=255, b=255 } in *)
  
  Array.iteri (fun i arr ->
    Array.iteri (fun j _ ->
      let (n, w) = match matrix.(i).(j) with
      | WhiteN ((n, e, s, w), _)
      | BlackN (n, e, s, w) ->
	  (n, w)
      in
      let (xfrom, xto) = (i*pix, i*pix + pix) in
      let (yfrom, yto) = (j*pix, j*pix + pix) in
	if not n then begin
	    for k=xfrom to xto do
	      (img#set k yfrom black) done
	end;
	
	if not w then begin
	    for k=yfrom to yto do
	      (img#set xfrom k black) done
	end
    ) arr
  ) matrix;
  
  img
;;

let store name format matrix path id =
  if id != -1 then () else
  let img = picture matrix in
  img#save (name ^ ".maze." ^ path ^ (string_of_int id)) (Some format) [];;

let maze () =
  let (name, format, matrix, xlim, ylim) = (Build.build ()) in
  let white = WhiteN((true, true, true, true), new Build.region) in
  let black = BlackN(true, true, true, true) in
    store name format matrix "" (-2);
    
    poly matrix white (0, xlim) (0, ylim)    (store (name^"_wh") format matrix) "" 0;
    poly matrix black (0, xlim) (0, ylim)    (store (name^"_bl") format matrix) "" 0;

(*    let img = picture matrix in
      store name format img *)
    store name format matrix "" (-1)
;;

maze ();;
