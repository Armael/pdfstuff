open Batteries
open Gg

module Pt = Pdftransform
type trans_m = Pt.transform_matrix

module type State = sig
  module Marked : sig
    type t = private <
      get : (string * Pdf.pdfobject option) option;
      set : (string * Pdf.pdfobject option) option -> unit;
      .. >
    val init : t
  end

  module Text : sig
    type t = private <
      transformation_matrix: trans_m;
      set_transformation_matrix : trans_m -> unit;
      line_matrix: trans_m;
      set_line_matrix : trans_m -> unit;
      (* rendering_matrix: trans_m; *)
      .. >
    val init : t
  end

  module Path : sig
    type t = private < .. >
    val init : t
  end

  module Graphics : sig
    (* Initialized at the beginning of each page with the values specified in
       Table 4.2. (we ignore device specific parameters)

       As we do not really care, our device coordinate system will simply match
       the user coordinate system. The CTM will consequently be initialized to
       the identity matrix.  *)

    (* Text state parameters that can be read and set outside of the text object
       level, at the page description level.

       [c], [w], [l], [rise] are expressed in *unscaled* text space units. This
       means that they are specified in a coordinate system that is defined by
       the text matrix Tm ([Text.t.transformation_matrix]), but is not scaled by
       the font size parameter [fs]. *)
    type text = {
      (* Character spacing *)
      c : float;

      (* Word spacing *)
      w : float;

      (* Horizontal scaling *)
      h : float;

      (* Leading *)
      l : float;

      (* Text font *)
      f : string;

      (* Text font size *)
      fs : float;

      (* Text rendering mode *)
      (* mode : int; *)

      (* Text rise *)
      rise : float;

      (* Text knockout *)
      (* k : float; *)
    }

    val text_init : text

    type t = private <
      transformation_matrix: trans_m; (* CTM *)
      set_transformation_matrix : trans_m -> unit;
      (* current clipping path : todo *)
      (* current color space : todo *)
      (* current color : todo *)
      text : text;
      set_text : text -> unit;
      (* line_width : float; *)
      (* line_cap : int; (enum) *)
      (* line_join : int; (enum) *)
      (* miter_limit : float; *)
      (* dash pattern : todo *)
      (* rendering_intent : string; *)
      (* stroke adjustment : todo *)
      (* blend mode : todo *)
      (* soft mask : todo *)
      (* alpha constant : todo *)
      (* alpha source : todo *)
      .. >
    val init : t
  end

  type page_desc = private <
    frame : Box2.t;
    st : Graphics.t;
    set_st : Graphics.t -> unit;
    stack : Graphics.t list;
    pop_st : unit;
    push_st : unit;
    .. >
  val page_desc_init : frame:Box2.t -> page_desc
end

module BaseState = struct
  module Marked = struct
    class t = object
      val mutable v : (string * Pdf.pdfobject option) option = None
      method get = v
      method set v' = v <- v'
    end
    let init = new t
  end

  module Text = struct
    class t = object
      val mutable transformation_matrix = Pt.i_matrix;
      method transformation_matrix = transformation_matrix
      method set_transformation_matrix m = transformation_matrix <- m

      val mutable line_matrix = Pt.i_matrix;
      method line_matrix = line_matrix
      method set_line_matrix m = line_matrix <- m
    end
    let init = new t
  end

  module Path = struct
    class t = object end
    let init = new t
  end

  module Graphics = struct
    type text = {
      c: float; w: float; h: float;
      l: float; f: string; fs: float;
      rise: float;
    }
    
    let text_init = {
      c = 0.; w = 0.; h = 100.;
      l = 0.;
      f = ""; (* dummy value *)
      fs = 0.; (* dummy value *)
      rise = 0.;
    }

    class t = object
      val mutable transformation_matrix = Pt.i_matrix;
      method transformation_matrix = transformation_matrix
      method set_transformation_matrix m = transformation_matrix <- m

      val mutable text = text_init
      method text = text
      method set_text t = text <- t
    end
    let init = new t
  end

  class page_desc (frame: Box2.t) = object
    method frame = frame
    val mutable st = Graphics.init
    method st = st
    method set_st s = st <- s
    val mutable stack = []
    method pop_st = match stack with
      | [] -> raise Stack.Empty (* fixme? *)
      | s :: ss ->
        st <- s;
        stack <- ss;
    method push_st = stack <- st :: stack
  end
  let page_desc_init ~frame = new page_desc frame
end

(* Rectangle of user space corresponding to the visible area *)
let page_box (doc: Pdf.t) (page: Pdfpage.t) =
  let (minx, miny, maxx, maxy) =
    Pdf.parse_rectangle
      (match Pdf.lookup_direct doc "/CropBox" page.Pdfpage.rest with
       | Some r -> r
       | None -> page.Pdfpage.mediabox) in
  Box2.v (V2.v minx miny) (V2.v (maxx -. minx) (maxy -. miny))

(*****************************************)

let (>||) o k =
  match o with
  | None -> k ()
  | Some x -> Some x

module Make = functor (State : State) ->
struct
  class type ['a] reader = object
    method page_description_level :
      State.page_desc -> State.Marked.t -> Pdfops.t list -> 'a -> 'a
    method text_object :
      State.page_desc -> State.Text.t -> State.Marked.t -> Pdfops.t list -> 'a -> 'a
    method path_object : State.page_desc -> State.Path.t -> Pdfops.t list -> 'a -> 'a
    method clipping_path_object : State.page_desc -> State.Path.t -> Pdfops.t list -> 'a -> 'a

    method general_graphics_state_op :
      State.page_desc -> Pdfops.t -> 'a -> 'a option
    method special_graphics_state_op :
      State.page_desc -> Pdfops.t -> 'a -> 'a option
    method color_op :
      State.page_desc -> Pdfops.t -> 'a -> 'a option
    method text_state_op :
      State.page_desc -> Pdfops.t -> 'a -> 'a option
    method marked_content_op_at_pagedesc :
      State.page_desc -> State.Marked.t -> Pdfops.t -> 'a -> 'a option
    method marked_content_op_at_textobj :
      State.page_desc -> State.Text.t -> State.Marked.t -> Pdfops.t -> 'a ->
      'a option
    method text_showing_op :
      State.page_desc -> State.Text.t -> Pdfops.t -> 'a -> 'a option
    method text_positioning_op :
      State.page_desc -> State.Text.t -> Pdfops.t -> 'a -> 'a option
    method path_construction_op :
      State.page_desc -> State.Path.t -> Pdfops.t -> 'a -> 'a option
    method path_painting_op :
      State.page_desc -> State.Path.t -> Pdfops.t -> 'a -> 'a option

    method page : Pdf.t -> Pdfpage.t -> 'a -> 'a
  end

  class ['a] read : ['a] reader = object(r)
    method page_description_level st m ops acc =
      let open Pdfops in
      (* Allowed operators:
         - General graphics state
         - Special graphics state
         - Color
         - Text state
         - Marked content
      *)

      match ops with
      | [] -> acc
      | op :: ops ->
        (r#general_graphics_state_op st op acc >|| fun _ ->
         r#special_graphics_state_op st op acc >|| fun _ ->
         r#color_op st op acc >|| fun _ ->
         r#text_state_op st op acc >|| fun _ ->
         r#marked_content_op_at_pagedesc st m op acc) |> function
        | Some acc ->
          r#page_description_level st m ops acc 
        | None ->
          match op with
          | Op_BT ->
            r#text_object st State.Text.init State.Marked.init ops acc
          | Op_m _
          | Op_re _ ->
            r#path_object st State.Path.init (op::ops) acc
          | Op_sh _ -> failwith "todo"
          | Op_Do _ -> failwith "todo"
          | InlineImage _ -> failwith "todo"
          | Op_Unknown s ->
            (* todo: not fail? *)
            failwith (Printf.sprintf "Pdfops.Op_Unknown %s" s)
          | _ -> (* invalid pdf *) assert false

    method text_object st text_st m ops acc =
      let open Pdfops in
      (* Allowed operators:
         - General graphics state
         - Color
         - Text state
         - Text-showing
         - Text-positioning
         - Marked-content
      *)

      match ops with
      | [] -> acc
      | op :: ops ->
        (r#general_graphics_state_op st op acc >|| fun _ ->
         r#color_op st op acc >|| fun _ ->
         r#text_state_op st op acc >|| fun _ ->
         r#text_showing_op st text_st op acc >|| fun _ ->
         r#text_positioning_op st text_st op acc >|| fun _ ->
         r#marked_content_op_at_textobj st text_st m op acc) |> function
        | Some acc ->
          r#text_object st text_st m ops acc
        | None ->
          match op with
          | Op_ET -> r#page_description_level st State.Marked.init ops acc
          | _ -> (* invalid pdf *) assert false

    method path_object st path_st ops acc =
      let open Pdfops in
      (* Allowed operators:
         - Path construction
      *)
      match ops with
      | [] -> acc
      | op :: ops ->
        (r#path_construction_op st path_st op acc) |> function
        | Some acc -> r#path_object st path_st ops acc
        | None ->
          match r#path_painting_op st path_st op acc with
          | Some acc ->
            r#page_description_level st State.Marked.init ops acc
          | None ->
            match op with
            | Op_W
            | Op_W' -> r#clipping_path_object st path_st ops acc
            | _ -> (* invalid pdf *) assert false

    method clipping_path_object st path_st ops acc =
      let open Pdfops in
      (* Allowed operators: none *)
      match ops with
      | [] -> acc
      | op :: ops ->
        match r#path_painting_op st path_st op acc with
        | Some acc ->
          r#page_description_level st State.Marked.init ops acc
        | None -> (* invalid pdf *) assert false

    method general_graphics_state_op st op acc =
      let open Pdfops in
      match op with
      (* General graphics state (w, J, j, M, d, ri, i, gs) *)
      | Op_w _
      | Op_J _
      | Op_j _
      | Op_M _
      | Op_d (_, _)
      | Op_ri _
      | Op_i _
      | Op_gs _ -> Some acc
      | _ -> None

    method special_graphics_state_op st op acc =
      let open Pdfops in
      match op with
      (* Special graphics state (q, Q, cm) *)
      | Op_q ->
        st#push_st; Some acc
      | Op_Q ->
        st#pop_st; Some acc
      | Op_cm m ->
        st#st#set_transformation_matrix
          (Pt.matrix_compose m st#st#transformation_matrix);
        Some acc
      | _ -> None

    method color_op st op acc =
      let open Pdfops in
      match op with
      (* Color (CS, cs, SC, SCN, sc, scn, G, g, RG, rg, K, k) *)
      | Op_CS _
      | Op_cs _
      | Op_SC _
      | Op_SCN _
      | Op_sc _
      | Op_scn _
      | Op_G _
      | Op_g _
      | Op_RG _
      | Op_rg _
      | Op_K _
      | Op_k _ -> Some acc
      | _ -> None

    method text_state_op st op acc =
      let open Pdfops in
      let open State in
      let open Graphics in
      match op with
      (* Text state (Tc, Tw, Tz, TL, Tf, Tr, Ts) *)
      | Op_Tc c ->
        st#st#set_text {st#st#text with c}; Some acc
      | Op_Tw w ->
        st#st#set_text {st#st#text with w}; Some acc
      | Op_Tz h ->
        st#st#set_text {st#st#text with h}; Some acc
      | Op_TL l ->
        st#st#set_text {st#st#text with l}; Some acc
      | Op_Tf (f, fs) ->
        st#st#set_text {st#st#text with f; fs}; Some acc
      | Op_Tr _ -> Some acc
      | Op_Ts rise ->
        st#st#set_text {st#st#text with rise}; Some acc
      | _ -> None

    method marked_content_op_at_pagedesc st m op acc =
      let open Pdfops in
      match op with
      (* Marked content (MP, DP, BMC, BDC, EMC) *)
      | Op_MP _
      | Op_DP _
      | Op_BMC _
      | Op_BDC _
      | Op_EMC -> Some acc (* todo *)
      | _ -> None

    method marked_content_op_at_textobj st text_st m op acc =
      let open Pdfops in
      match op with
      (* Marked content (MP, DP, BMC, BDC, EMC) *)
      | Op_MP _
      | Op_DP _
      | Op_BMC _
      | Op_BDC _
      | Op_EMC -> Some acc (* todo *)
      | _ -> None

    method text_showing_op st text_st op acc =
      let open Pdfops in
      match op with
      (* Text showing (Tj, TJ, ', '') *)
      | Op_Tj txt ->
        (*     begin match ctx.text with
               | Nil -> print_endline "err Op_Tj and no Op_BT before?"
               | Writing { tm; _ } ->
                texts := (timg tm V2.zero, txt) :: !texts
               end *)
        Some acc
      | Op_' txt ->
        (* process Op_T'; process (Op_Tj txt) *)
        failwith "todo"
      | Op_'' (_, _, txt) -> (* process (Op_' txt) *)
        failwith "todo"
      | Op_TJ (Pdf.Array l) -> (* i'm lazy *)
        (* let txt = l *)
        (*           |> List.filter_map (function Pdf.String s -> Some s | _ -> None) *)
        (*           |> List.reduce (^) *)
        (* in *)
        (* process (Op_Tj txt) *)
        failwith "todo"
      | _ -> None

    method text_positioning_op st text_st op acc =
      let open Pdfops in
      match op with
      (* Text positioning (Td, TD, Tm, T* ) *)
      | Op_Td (tx, ty)
      | Op_TD (tx, ty) ->


        (* begin match ctx.text with *)
        (*   | Nil -> print_endline "err Op_Td and no Op_BT before?" *)
        (*   | Writing { tm; tlm } -> *)
        (*     let m = Pt.matrix_compose (Pt.mktranslate tx ty) tlm in *)
        (*     ctx.text <- Writing { tm = m; tlm = m } *)
        (* end *)
        Some acc
      | Op_Tm m ->
        (* ctx.text <- Writing { tm = m; tlm = m } *)
        failwith "todo"
      | Op_T' ->
        (* process (Op_Td (0., 0.)) *)
        failwith "todo"
      | _ -> None

    method path_construction_op st path_st op acc =
      let open Pdfops in
      match op with
      (* Path construction (m, l, c, v, y, h, re) *)
      | Op_m (x, y) ->
        (* Some (V2.v x y, V2.v x y) *)
        failwith "todo"
      | Op_l (x', y') ->
        (* begin match ctx.path with *)
        (*   | Nil -> print_endline "errr Op_l and no Op_m before?" *)
        (*   | Drawing (init, v) -> *)
        (*     let v' = V2.v x' y' in *)
        (*     ctx.path <- Drawing (init, v'); *)
        (*     lines := (img v, img v') :: !lines *)
        (*   end *)
        failwith "todo"
      | Op_c (_, _, _, _, _, _) ->
        failwith "todo"
      | Op_v (_, _, _, _) ->
        failwith "todo"
      | Op_y (_, _, _, _) ->
        failwith "todo"
      | Op_h ->
        (* begin match ctx.path with *)
        (* | Nil -> print_endline "errr Op_h and no Op_m before?" *)
        (* | Drawing (init, v) -> *)
        (*   ctx.path <- Nil; (\* ? *\) *)
        (*   lines := (img v, img init) :: !lines *)
        (* end *)
        failwith "todo"
      | Op_re (x, y, w, h) ->
        (* rects := (Box2.v (img (V2.v x y)) (img (V2.v w h))) :: !rects *)
        failwith "todo"
      | _ -> None

    method path_painting_op st st_path op acc =
      let open Pdfops in
      match op with
      (* Path painting (S, s, f, F, f*, B, B*, b, b*, n) *)
      | Op_S
      | Op_s
      | Op_f
      | Op_F
      | Op_f'
      | Op_B
      | Op_B'
      | Op_b
      | Op_b'
      | Op_n -> failwith "todo"
      | _ -> None

    method page doc page acc =
      let frame = page_box doc page in
      let init = State.page_desc_init ~frame in
      let ops = Pdfops.parse_operators doc
          page.Pdfpage.resources page.Pdfpage.content in
      r#page_description_level init State.Marked.init ops acc
  end
end
  
(* let collect (pdf: Pdf.t) (page: Pdfpage.t) = *)
(*   let num x = match x with *)
(*     | Pdf.Integer n -> float_of_int n *)
(*     | Pdf.Real f -> f *)
(*     | _ -> failwith "NaN" *)
(*   in *)
(*   let Pdf.Array [llx; lly; urx; ury] = page.Pdfpage.mediabox in *)
(*   let llx = num llx and lly = num lly *)
(*   and urx = num urx and ury = num ury in *)
  
(*   let frame = Box2.v (V2.v llx lly) (V2.v (urx -. llx) (ury -. lly)) in *)
  
(*   let ctx = { *)
(*     frame; *)
(*     path = Nil; *)
(*     text = Nil; *)
(*     transform = Pt.i_matrix; *)
(*   } in *)

(*   let ctx_stack = Queue.create () in *)
  
(*   let pdf = Pdfpage.change_pages false pdf [page] in *)
(*   let ops = Pdfops.parse_operators pdf *)
(*     page.Pdfpage.resources page.Pdfpage.content in *)

(*   let lines = ref [] *)
(*   and rects = ref [] *)
(*   and texts = ref [] *)
(*   in *)

(*   let img (p: p2) = *)
(*     let (ix, iy) = Pt.transform_matrix *)
(*       ctx.transform *)
(*       (V2.x p, V2.y p) *)
(*     in *)
(*     V2.v ix iy *)
(*   in *)

(*   let timg (tm: trans_m) (p: p2) = *)
(*     let (ix, iy) = Pt.transform_matrix *)
(*       (Pt.matrix_compose tm ctx.transform) *)
(*       (V2.x p, V2.y p) *)
(*     in *)
(*     V2.v ix iy *)
(*   in *)

(*   let rec process op = *)
(*     let open Pdfops in *)
(*     match op with *)
(*     | Op_q -> Queue.push (ctx |> Obj.repr |> Obj.dup |> Obj.obj) ctx_stack *)
(*     | Op_Q -> let old_ctx = Queue.pop ctx_stack in *)
(*               ctx.path <- old_ctx.path; *)
(*               ctx.text <- old_ctx.text; *)
(*               ctx.transform <- old_ctx.transform *)
(*     | Op_cm m -> *)
(*       ctx.transform <- Pt.matrix_compose m ctx.transform; *)
(*     | Op_m (x, y) -> *)
(*       ctx.path <- Drawing (V2.v x y, V2.v x y) *)
(*     | Op_l (x', y') -> *)
(*       begin match ctx.path with *)
(*       | Nil -> print_endline "errr Op_l and no Op_m before?" *)
(*       | Drawing (init, v) -> *)
(*         let v' = V2.v x' y' in *)
(*         ctx.path <- Drawing (init, v'); *)
(*         lines := (img v, img v') :: !lines *)
(*       end *)
(*     | Op_h -> *)
(*       begin match ctx.path with *)
(*       | Nil -> print_endline "errr Op_h and no Op_m before?" *)
(*       | Drawing (init, v) -> *)
(*         ctx.path <- Nil; (\* ? *\) *)
(*         lines := (img v, img init) :: !lines *)
(*       end *)
(*     | Op_re (x, y, w, h) -> *)
(*       rects := (Box2.v (img (V2.v x y)) (img (V2.v w h))) :: !rects *)
        
(*     | Op_BT -> *)
(*       ctx.text <- Writing { tm = Pt.i_matrix; tlm = Pt.i_matrix } *)
(*     | Op_ET -> *)
(*       ctx.text <- Nil *)
(*     | Op_TD (tx, ty) *)
(*     | Op_Td (tx, ty) -> *)
(*       begin match ctx.text with *)
(*       | Nil -> print_endline "err Op_Td and no Op_BT before?" *)
(*       | Writing { tm; tlm } -> *)
(*         let m = Pt.matrix_compose (Pt.mktranslate tx ty) tlm in *)
(*         ctx.text <- Writing { tm = m; tlm = m } *)
(*       end *)
(*     | Op_Tm m -> *)
(*       ctx.text <- Writing { tm = m; tlm = m } *)
(*     | Op_T' -> *)
(*       process (Op_Td (0., 0.)) *)
(*     | Op_Tj txt -> *)
(*       begin match ctx.text with *)
(*       | Nil -> print_endline "err Op_Tj and no Op_BT before?" *)
(*       | Writing { tm; _ } -> *)
(*         texts := (timg tm V2.zero, txt) :: !texts *)
(*       end *)
(*     | Op_' txt -> process Op_T'; process (Op_Tj txt) *)
(*     | Op_'' (_, _, txt) -> process (Op_' txt) *)
(*     | Op_TJ (Pdf.Array l) -> (\* i'm lazy *\) *)
(*       let txt = l *)
(*         |> List.filter_map (function Pdf.String s -> Some s | _ -> None) *)
(*         |> List.reduce (^) *)
(*       in *)
(*       process (Op_Tj txt) *)

(*     | _ -> () *)
(*   in *)

(*   List.iter process ops; *)
(*   (List.rev !lines, List.rev !rects, List.rev !texts) *)


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
