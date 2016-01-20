open Batteries
open Gg

module Pt = Pdftransform
type trans_m = Pt.transform_matrix

type path_st =
| Nil
| Drawing of p2 * p2

type text_st_writing = {
  tm: trans_m;
  tlm: trans_m;
}

type text_st =
| Nil
| Writing of text_st_writing

type graphical_st = {
  frame: box2;
  mutable path: path_st;
  mutable text: text_st;
  mutable transform: trans_m;
}

let collect (pdf: Pdf.t) (page: Pdfpage.t) =
  let num x = match x with
    | Pdf.Integer n -> float_of_int n
    | Pdf.Real f -> f
    | _ -> failwith "NaN"
  in
  let Pdf.Array [llx; lly; urx; ury] = page.Pdfpage.mediabox in
  let llx = num llx and lly = num lly
  and urx = num urx and ury = num ury in
  
  let frame = Box2.v (V2.v llx lly) (V2.v (urx -. llx) (ury -. lly)) in
  
  let ctx = {
    frame;
    path = Nil;
    text = Nil;
    transform = Pt.i_matrix;
  } in

  let ctx_stack = Queue.create () in
  
  let pdf = Pdfpage.change_pages false pdf [page] in
  let ops = Pdfops.parse_operators pdf
    page.Pdfpage.resources page.Pdfpage.content in

  let lines = ref []
  and rects = ref []
  and texts = ref []
  in

  let img (p: p2) =
    let (ix, iy) = Pt.transform_matrix
      ctx.transform
      (V2.x p, V2.y p)
    in
    V2.v ix iy
  in

  let timg (tm: trans_m) (p: p2) =
    let (ix, iy) = Pt.transform_matrix
      (Pt.matrix_compose tm ctx.transform)
      (V2.x p, V2.y p)
    in
    V2.v ix iy
  in

  let rec process op =
    let open Pdfops in
    match op with
    | Op_q -> Queue.push (ctx |> Obj.repr |> Obj.dup |> Obj.obj) ctx_stack
    | Op_Q -> let old_ctx = Queue.pop ctx_stack in
              ctx.path <- old_ctx.path;
              ctx.text <- old_ctx.text;
              ctx.transform <- old_ctx.transform
    | Op_cm m ->
      ctx.transform <- Pt.matrix_compose m ctx.transform;
    | Op_m (x, y) ->
      ctx.path <- Drawing (V2.v x y, V2.v x y)
    | Op_l (x', y') ->
      begin match ctx.path with
      | Nil -> print_endline "errr Op_l and no Op_m before?"
      | Drawing (init, v) ->
        let v' = V2.v x' y' in
        ctx.path <- Drawing (init, v');
        lines := (img v, img v') :: !lines
      end
    | Op_h ->
      begin match ctx.path with
      | Nil -> print_endline "errr Op_h and no Op_m before?"
      | Drawing (init, v) ->
        ctx.path <- Nil; (* ? *)
        lines := (img v, img init) :: !lines
      end
    | Op_re (x, y, w, h) ->
      rects := (Box2.v (img (V2.v x y)) (img (V2.v w h))) :: !rects
        
    | Op_BT ->
      ctx.text <- Writing { tm = Pt.i_matrix; tlm = Pt.i_matrix }
    | Op_ET ->
      ctx.text <- Nil
    | Op_TD (tx, ty)
    | Op_Td (tx, ty) ->
      begin match ctx.text with
      | Nil -> print_endline "err Op_Td and no Op_BT before?"
      | Writing { tm; tlm } ->
        let m = Pt.matrix_compose (Pt.mktranslate tx ty) tlm in
        ctx.text <- Writing { tm = m; tlm = m }
      end
    | Op_Tm m ->
      ctx.text <- Writing { tm = m; tlm = m }
    | Op_T' ->
      process (Op_Td (0., 0.))
    | Op_Tj txt ->
      begin match ctx.text with
      | Nil -> print_endline "err Op_Tj and no Op_BT before?"
      | Writing { tm; _ } ->
        texts := (timg tm V2.zero, txt) :: !texts
      end
    | Op_' txt -> process Op_T'; process (Op_Tj txt)
    | Op_'' (_, _, txt) -> process (Op_' txt)
    | Op_TJ (Pdf.Array l) -> (* i'm lazy *)
      let txt = l
        |> List.filter_map (function Pdf.String s -> Some s | _ -> None)
        |> List.reduce (^)
      in
      process (Op_Tj txt)

    | _ -> ()
  in

  List.iter process ops;
  (List.rev !lines, List.rev !rects, List.rev !texts)


(*****************
 * Debug functions
 *****************)

(* let collect_page1 pdfname = *)
(*   let pdf = Pdfread.pdf_of_file None None pdfname in *)
(*   let page1 = List.hd (Pdfpage.pages_of_pagetree pdf) in *)
(*   collect pdf page1 *)


(* let draw_lines lines = *)
(*   let dl (p1, p2) = *)
(*     Graphics.moveto (V2.x p1 |> int_of_float) (V2.y p1 |> int_of_float); *)
(*     Graphics.lineto (V2.x p2 |> int_of_float) (V2.y p2 |> int_of_float) *)
(*   in *)
(*   List.iter dl lines *)

(* let draw_rects rects = *)
(*   let dr b = *)
(*     Graphics.draw_rect *)
(*       (Box2.ox b |> int_of_float) *)
(*       (Box2.oy b |> int_of_float) *)
(*       (Box2.w b |> int_of_float) *)
(*       (Box2.h b |> int_of_float) *)
(*   in *)
(*   List.iter dr rects *)

(* let draw_texts texts = *)
(*   let dt (pos, txt) = *)
(*     Graphics.moveto (V2.x pos |> int_of_float) (V2.y pos |> int_of_float); *)
(*     Graphics.draw_string txt *)
(*   in *)
(*   List.iter dt texts *)
