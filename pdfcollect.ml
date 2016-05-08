open Gg

type trans_m = Pdftransform.transform_matrix

module type State = sig
  module MarkedBy : sig
    type t = private <
      get : (string * Pdf.pdfobject option) list;
      set : (string * Pdf.pdfobject option) list -> unit;
      .. >
    val init : t
  end

  module Text : sig
    type t = private <
      transformation_matrix: trans_m;
      set_transformation_matrix : trans_m -> unit;
      line_matrix: trans_m;
      set_line_matrix : trans_m -> unit;
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
      f : Pdf.pdfobject * Pdftext.font;

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
    pdf : Pdf.t; page : Pdfpage.t;
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
  module MarkedBy = struct
    class t = object
      val mutable v : (string * Pdf.pdfobject option) list = []
      method get = v
      method set v' = v <- v'
    end
    let init = new t
  end

  module Text = struct
    class t = object
      val mutable transformation_matrix = Pdftransform.i_matrix;
      method transformation_matrix = transformation_matrix
      method set_transformation_matrix m = transformation_matrix <- m

      val mutable line_matrix = Pdftransform.i_matrix;
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
      val mutable transformation_matrix = Pdftransform.i_matrix;
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
      State.page_desc -> State.MarkedBy.t -> Pdfops.t list -> 'a -> 'a
    method text_object :
      State.page_desc -> State.Text.t -> State.MarkedBy.t -> Pdfops.t list -> 'a -> 'a
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
      State.page_desc -> State.MarkedBy.t -> Pdfops.t -> 'a -> 'a option
    method marked_content_op_at_textobj :
      State.page_desc -> State.Text.t -> State.MarkedBy.t -> Pdfops.t -> 'a ->
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


  (* Utilities *)
  let rendering_matrix (st: State.page_desc) (text_st: State.Text.t) =
    let open State in
    let open Graphics in
    let text_params_m =
      Pdftransform.{
        a = st#st#text.fs *. st#st#text.h;
        b = 0.;
        c = 0.;
        d = st#st#text.fs;
        e = 0.;
        f = st#st#text.rise;
      } in
    Pdftransform.(matrix_compose
                    (matrix_compose
                       text_params_m
                       text_st#transformation_matrix)
                    st#st#transformation_matrix)

  let character_codes_of_text (st: State.page_desc) (text: string): int list =
    let open Pdfutil in
    let _, font = st#st#text.State.Graphics.f in
    (* Copy&paste from pdftext.ml glyphnames_and_codepoints_of_text, except we
       don't directly map the extractor on the caracter codes. *)
    (* For now, the only composite font encoding scheme we understand is
       /Identity-H *)
    let is_identity_h = function
      | Pdftext.CIDKeyedFont (_, _, Pdftext.Predefined "/Identity-H") -> true
      | _ -> false in
    if is_identity_h font then
    let chars = map int_of_char (explode text) in
      if odd (length chars) then raise (Pdf.PDFError "Bad Text") else
        map (fun (h, l) -> (h lsl 8) lor l) (pairs_of_list chars)
    else
      map (int_of_char) (explode text)

  let character_codes_width
      (st: State.page_desc)
      (char_codes: [`CharCodes of int list | `Adjust of float] list):
    float =
    let open Pdftext in
    let open State in
    let open State.Graphics in
    let width_char w c =
      let tw = if c = 32 then st#st#text.w else 0. in
      (w *. st#st#text.fs +. st#st#text.c +. tw) *.
      st#st#text.h
    in
    let width_adjust a =
      -. (a /. 1000.) *. st#st#text.fs *. st#st#text.h in
    let rec width_charcodes get_kern get_width = function
      | [] -> 0.
      | [h] -> width_char (get_width h) h
      | h :: h' :: t ->
        let w = get_width h and k = get_kern h h' in
        (width_char (w +. k) h) +.
        (width_charcodes get_kern get_width (h' :: t))
    in
    let width get_kern get_width = List.fold_left (fun w -> function
      | `CharCodes l -> w +. (width_charcodes get_kern get_width l)
      | `Adjust a -> w +. (width_adjust a)
    ) 0. in

    let get_width, get_kern =
      match snd st#st#text.f with
      | StandardFont (font, _) ->
        let _, widths, kerning = Pdfstandard14.afm_data font in
        (fun c ->
           try Hashtbl.find widths c |> float_of_int
           with Not_found -> 0.),
        (fun c c' ->
           try Hashtbl.find kerning (c, c') |> float_of_int
           with Not_found -> 0.)
      | SimpleFont font ->
        let widths = match font.fontmetrics with
          | None -> raise (Pdf.PDFError "No /Widths")
          | Some w -> w in
        (fun c -> widths.(c)), (fun _ _ -> 0.)
      | CIDKeyedFont (_, font, _) ->
        let default_width = float_of_int font.cid_default_width in
        (fun c ->
           try List.assoc c font.cid_widths with
              Not_found -> default_width),
        (fun _ _ -> 0.)
    in
    width get_kern get_width char_codes

  (* ---- *)

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
            r#text_object st State.Text.init State.MarkedBy.init ops acc
          | Op_m _
          | Op_re _ ->
            r#path_object st State.Path.init (op::ops) acc
          | Op_sh _
          | Op_Do _
          | InlineImage _ ->
            r#page_description_level st m ops acc
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
          | Op_ET -> r#page_description_level st State.MarkedBy.init ops acc
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
            r#page_description_level st State.MarkedBy.init ops acc
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
          r#page_description_level st State.MarkedBy.init ops acc
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
          (Pdftransform.matrix_compose m st#st#transformation_matrix);
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
      | Op_Tf (fontname, fs) ->
        let f =
          match Pdf.lookup_direct st#pdf "/Font" st#page.Pdfpage.resources with
          | None -> raise (Pdf.PDFError "Missing /Font in text extraction")
          | Some d ->
            match Pdf.lookup_direct st#pdf fontname d with
            | None -> raise (Pdf.PDFError "Missing font in text extraction")
            | Some d -> d, Pdftext.read_font st#pdf d
        in
        st#st#set_text {st#st#text with f; fs}; Some acc
      | Op_Tr _ -> Some acc
      | Op_Ts rise ->
        st#st#set_text {st#st#text with rise}; Some acc
      | _ -> None

    method private marked_content_aux m op acc =
      let open Pdfops in
      match op with
      (* Marked content (MP, DP, BMC, BDC, EMC) *)
      | Op_MP _
      | Op_DP (_, _) -> Some acc
      | Op_BMC tag ->
        m#set ((tag, None) :: m#get);
        Some acc
      | Op_BDC (tag, properties) ->
        m#set ((tag, Some properties) :: m#get);
        Some acc
      | Op_EMC ->
        (match m#get with
        | [] -> raise (Pdf.PDFError "EMC without a matching BMC or BDC")
        | _ :: xs -> m#set xs);
        Some acc
      | _ -> None

    method marked_content_op_at_pagedesc st m op acc =
      r#marked_content_aux m op acc

    method marked_content_op_at_textobj st text_st m op acc =
      r#marked_content_aux m op acc

    method text_showing_op st text_st op acc =
      let open Pdfops in
      let translate_tm tx ty =
        text_st#set_transformation_matrix (Pdftransform.(
          matrix_compose (mktranslate tx ty) text_st#transformation_matrix
        )) in
      let perform_Td tx ty =
        let new_m = Pdftransform.(
          matrix_compose (mktranslate tx ty) text_st#line_matrix
        ) in
        text_st#set_line_matrix new_m;
        text_st#set_transformation_matrix new_m in
      let perform_Tj txt =
        let tx = character_codes_width st
            [`CharCodes (character_codes_of_text st txt)] in
        translate_tm tx 0. in
      let perform_' txt =
        perform_Td 0. st#st#text.State.Graphics.l;
        perform_Tj txt in

      match op with
      (* Text showing (Tj, TJ, ', '') *)
      | Op_Tj txt -> perform_Tj txt; Some acc
      | Op_' txt -> perform_' txt; Some acc
      | Op_'' (aw, ac, txt) ->
        st#st#set_text {st#st#text with
                        State.Graphics.w = aw;
                        State.Graphics.c = ac};
        perform_' txt;
        Some acc
      | Op_TJ (Pdf.Array l) ->
        let charcodes = Pdfutil.option_map (function
          | Pdf.String txt ->
            Some (`CharCodes (character_codes_of_text st txt))
          | Pdf.Integer i ->
            Some (`Adjust (float_of_int i))
          | Pdf.Real f ->
            Some (`Adjust f)
          | _ -> None) l in
        translate_tm (character_codes_width st charcodes) 0.;
        Some acc
      | _ -> None

    method text_positioning_op st text_st op acc =
      let open Pdfops in
      let perform_Td tx ty =
        let new_m = Pdftransform.(
          matrix_compose (mktranslate tx ty) text_st#line_matrix
        ) in
        text_st#set_line_matrix new_m;
        text_st#set_transformation_matrix new_m in
      match op with
      (* Text positioning (Td, TD, Tm, T* ) *)
      | Op_Td (tx, ty) -> perform_Td tx ty; Some acc
      | Op_TD (tx, ty) ->
        st#st#set_text {st#st#text with State.Graphics.l = -. ty};
        perform_Td tx ty;
        Some acc
      | Op_Tm m ->
        text_st#set_line_matrix m;
        text_st#set_transformation_matrix m;
        Some acc
      | Op_T' ->
        perform_Td 0. st#st#text.State.Graphics.l;
        Some acc
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
      r#page_description_level init State.MarkedBy.init ops acc
  end
end
  

(* Font and glyph metrics

   After painting some glyphs (using e.g. Tj), we need to update the text matrix
   Tm. Section 5.3.3 gives the formula for computing the translation matrix. We
   need in particular the glyph's horizontal and vertical displacements.

   Type 0 fonts = composite fonts

   Other fonts = simple fonts

   + Simple fonts:

   nb: glyphs are single byte codes, obtained from the string; these codes index
   a table of 256 glyphs, the mapping from code to glyph is called the font's
   encoding.

   Simple fonts only support horizontal writing, therefore glyph metrics only
   include glyph width.

   - For the standard 14 type 1 fonts, font metrics are in fact predefined;
     Pdfstandard14 gives access to this data.  (special treatment for these
     fonts is deprecated as pdf 1.5, but camlpdf still treat them separately -
     see the Pdftext.font type)

   - Other simple fonts (spec §5.5): glyph widths parsed by camlpdf and stored
     in the fontmetrics array, indexed by character codes


   + Composite fonts:

   nb: a glyph is represented by a sequence of one or more bytes. camlpdf seems
   to lack an exported primitive to iterate on the character codes, knowing the
   encoding. glyphnames_and_codepoints_of_text does this, and maps the extractor
   on the character codes. -> c&p without mapping the extractor

   camlpdf handles only one CMap (type cmap_encoding): Predefined "/Identity-H"
   (not a big deal, other ones seem to be used for japanase/chinise/korean
   writing). Identity-H has 16 bits character codes.


   -----

   Encoding: 

   Association character code -> glyph

   -----

   To extract text from a string of character codes, just use
   Pdftext.text_extractor_of_font & co, it uses the /ToUnicode map if present
   etc. Support seems however limited for composite fonts.
*)


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
