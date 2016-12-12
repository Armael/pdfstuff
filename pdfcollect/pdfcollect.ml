open Gg

let illegal_op op level =
  Pdf.PDFError (Printf.sprintf "Illegal op at %s level: %s" level (Pdfops.string_of_op op))

let no_font_defined =
  Pdf.PDFError "No font defined"

type trans_m = Pdftransform.transform_matrix

module type State = sig
  module MarkedBy : sig
    type t = private <
      get : (string * Pdf.pdfobject option) list;
      set : (string * Pdf.pdfobject option) list -> unit;
      .. >
    val create : unit -> t
  end

  module Text : sig
    type t = private <
      transformation_matrix: trans_m;
      set_transformation_matrix : trans_m -> unit;
      line_matrix: trans_m;
      set_line_matrix : trans_m -> unit;
      .. >
    val create : unit -> t
  end

  module Path : sig
    type t = private < .. >
    val create : unit -> t
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
      f : (Pdf.pdfobject * Pdftext.font) option;

      (* Text font size *)
      fs : float;

      (* Text rendering mode *)
      (* mode : int; *)

      (* Text rise *)
      rise : float;

      (* Text knockout *)
      (* k : float; *)
    }

    val text_create : unit -> text

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
    val create : unit -> t
  end

  type page_desc = private <
    pdf : Pdf.t;
    page : Pdfpage.t;
    frame : Box2.t;
    st : Graphics.t;
    set_st : Graphics.t -> unit;
    stack : Graphics.t list;
    pop_st : unit;
    push_st : unit;
    .. >
  val page_desc_create :
    pdf:Pdf.t ->
    page:Pdfpage.t ->
    frame:Box2.t ->
    page_desc
end

module BaseState : State = struct
  module MarkedBy = struct
    class t = object
      val mutable v : (string * Pdf.pdfobject option) list = []
      method get = v
      method set v' = v <- v'
    end
    let create () = new t
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
    let create () = new t
  end

  module Path = struct
    class t = object end
    let create () = new t
  end

  module Graphics = struct
    type text = {
      c: float; w: float; h: float;
      l: float;
      f: (Pdf.pdfobject * Pdftext.font) option;
      fs: float;
      rise: float;
    }

    let text_create () = {
      c = 0.; w = 0.; h = 1.;
      l = 0.;
      f = None;
      fs = 0.; (* dummy value *)
      rise = 0.;
    }

    class t = object
      val mutable transformation_matrix = Pdftransform.i_matrix;
      method transformation_matrix = transformation_matrix
      method set_transformation_matrix m = transformation_matrix <- m

      val mutable text = text_create ()
      method text = text
      method set_text t = text <- t
    end
    let create () = new t
  end

  class page_desc (pdf: Pdf.t) (page: Pdfpage.t) (frame : Box2.t) = object
    method pdf = pdf
    method page = page
    method frame = frame
    val mutable st = Graphics.create ()
    method st = st
    method set_st s = st <- s
    val mutable stack = []
    method stack = stack
    method pop_st = match stack with
      | [] -> raise Stack.Empty (* fixme? *)
      | s :: ss ->
        st <- s;
        stack <- ss;
    method push_st = stack <- st :: stack
  end
  let page_desc_create ~pdf ~page ~frame = new page_desc pdf page frame
end

module CustomState = struct
  module MarkedBy = struct
    class t = object
      val base = BaseState.MarkedBy.create ()
      val mutable foobar = 3
      method get = base#get
      method set x = base#set x
      method incr = foobar <- foobar+1; foobar
    end
    let create () = new t
  end

  module Text = BaseState.Text
  module Path = BaseState.Path
  module Graphics = BaseState.Graphics
  type page_desc = BaseState.page_desc
  let page_desc_create = BaseState.page_desc_create
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

let (>||) res k =
  match res with
  | `BadOp -> k ()
  | `Ok -> `Ok

type op_result = [ `Ok | `BadOp ]

module Make = functor (State : State) ->
struct
  class type reader = object
    method page_description_level :
      State.page_desc -> State.MarkedBy.t -> Pdfops.t list -> unit
    method text_object :
      State.page_desc -> State.Text.t -> State.MarkedBy.t -> Pdfops.t list -> unit
    method path_object : State.page_desc -> State.Path.t -> Pdfops.t list -> unit
    method clipping_path_object : State.page_desc -> State.Path.t -> Pdfops.t list -> unit

    method general_graphics_state_op :
      State.page_desc -> Pdfops.t -> op_result
    method special_graphics_state_op :
      State.page_desc -> Pdfops.t -> op_result
    method color_op :
      State.page_desc -> Pdfops.t -> op_result
    method text_state_op :
      State.page_desc -> Pdfops.t -> op_result
    method marked_content_op_at_pagedesc :
      State.page_desc -> State.MarkedBy.t -> Pdfops.t -> op_result
    method marked_content_op_at_textobj :
      State.page_desc -> State.Text.t -> State.MarkedBy.t -> Pdfops.t ->
      op_result
    method text_showing_op :
      State.page_desc -> State.Text.t -> Pdfops.t -> op_result
    method text_positioning_op :
      State.page_desc -> State.Text.t -> Pdfops.t -> op_result
    method path_construction_op :
      State.page_desc -> State.Path.t -> Pdfops.t -> op_result
    method path_painting_op :
      State.page_desc -> State.Path.t -> Pdfops.t -> op_result

    method text_show :
      State.page_desc -> State.Text.t -> width:float -> string -> unit
    method text_position :
      State.page_desc ->
      State.Text.t ->
      ?tm:Pdftransform.transform_matrix ->
      ?lm:Pdftransform.transform_matrix ->
      unit -> unit

    method page : Pdf.t -> Pdfpage.t -> unit
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
    let _, font = match st#st#text.State.Graphics.f with
      | Some f -> f
      | None -> raise (Pdf.PDFError "Font not defined")
    in
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

  let adjust_width (st: State.page_desc) (adjust: float): float =
    let open State.Graphics in
    -. (adjust /. 1000.) *. st#st#text.fs *. st#st#text.h

  let glyphspace_to_textspace st (x, y) =
    let open Pdftext in
    let open State.Graphics in
    let glyphspace_to_textspace_m =
      let font = match st#st#text.f with
        | Some (_, f) -> f
        | None -> raise no_font_defined
      in
      match font with
      | SimpleFont { fonttype = Type3 ty3; _ } ->
        ty3.fontmatrix
      | _ ->
        Pdftransform.mkscale (0., 0.) (1. /. 1000.) 0.
    in
    Pdftransform.transform_matrix glyphspace_to_textspace_m
      (x, y)

  let char_width st w c =
    let open State.Graphics in
    let tw = if c = 32 then st#st#text.w else 0. in
    (w *. st#st#text.fs +. st#st#text.c +. tw) *.
    st#st#text.h

  let glyphspace_char_width st w c =
    char_width st (glyphspace_to_textspace st (w, 0.) |> fst) c

  let mk_get_width_kern (st: State.page_desc) =
    let open Pdftext in
    let open State.Graphics in
    let (_, f) = match st#st#text.f with
      | Some f -> f
      | None -> raise no_font_defined
    in
    match f with
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

  let get_width st = fst (mk_get_width_kern st)
  let get_kern st = snd (mk_get_width_kern st)

  let character_codes_width
      (st: State.page_desc)
      (char_codes: int list):
    float =
    let open Pdftext in
    let open State in
    let open State.Graphics in
    let rec width_charcodes get_kern get_width = function
      | [] -> 0.
      | [h] -> glyphspace_char_width st (get_width h) h
      | h :: h' :: t ->
        let w = get_width h and k = get_kern h h' in
        (glyphspace_char_width st (w +. k) h) +.
        (width_charcodes get_kern get_width (h' :: t))
    in
    width_charcodes (get_kern st) (get_width st) char_codes

  (* ---- *)

  class read : reader = object(r)
    method page_description_level st m ops =
      let open Pdfops in
      (* Allowed operators:
         - General graphics state
         - Special graphics state
         - Color
         - Text state
         - Marked content
      *)

      match ops with
      | [] -> ()
      | op :: ops ->
        (r#general_graphics_state_op st op >|| fun _ ->
         r#special_graphics_state_op st op >|| fun _ ->
         r#color_op st op >|| fun _ ->
         r#text_state_op st op >|| fun _ ->
         r#marked_content_op_at_pagedesc st m op) |> function
        | `Ok ->
          r#page_description_level st m ops
        | `BadOp ->
          match op with
          | Op_BT ->
            r#text_object st (State.Text.create ()) (State.MarkedBy.create ()) ops
          | Op_m _
          | Op_re _ ->
            r#path_object st (State.Path.create ()) (op::ops)
          | Op_sh _
          | Op_Do _
          | InlineImage _ ->
            r#page_description_level st m ops
          | Op_Unknown s ->
            (* todo: not fail? *)
            failwith (Printf.sprintf "Pdfops.Op_Unknown %s" s)
          | _ ->
            raise (illegal_op op "page description")

    method text_object st text_st m ops =
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
      | [] -> ()
      | op :: ops ->
        (r#general_graphics_state_op st op >|| fun _ ->
         r#color_op st op >|| fun _ ->
         r#text_state_op st op >|| fun _ ->
         r#text_showing_op st text_st op >|| fun _ ->
         r#text_positioning_op st text_st op >|| fun _ ->
         r#marked_content_op_at_textobj st text_st m op) |> function
        | `Ok ->
          r#text_object st text_st m ops
        | `BadOp ->
          match op with
          | Op_ET -> r#page_description_level st (State.MarkedBy.create ()) ops
          | _ -> raise (illegal_op op "text object")

    method path_object st path_st ops =
      let open Pdfops in
      (* Allowed operators:
         - Path construction
      *)
      match ops with
      | [] -> ()
      | op :: ops ->
        (r#path_construction_op st path_st op) |> function
        | `Ok -> r#path_object st path_st ops
        | `BadOp ->
          match r#path_painting_op st path_st op with
          | `Ok ->
            r#page_description_level st (State.MarkedBy.create ()) ops
          | `BadOp ->
            match op with
            | Op_W
            | Op_W' -> r#clipping_path_object st path_st ops
            | _ -> raise (illegal_op op "path object")

    method clipping_path_object st path_st ops =
      let open Pdfops in
      (* Allowed operators: none *)
      match ops with
      | [] -> ()
      | op :: ops ->
        match r#path_painting_op st path_st op with
        | `Ok ->
          r#page_description_level st (State.MarkedBy.create ()) ops
        | `BadOp -> raise (illegal_op op "clipping path object")

    method general_graphics_state_op st op =
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
      | Op_gs _ -> (* todo *) `Ok
      | _ -> `BadOp

    method special_graphics_state_op st op =
      let open Pdfops in
      match op with
      (* Special graphics state (q, Q, cm) *)
      | Op_q ->
        st#push_st; `Ok
      | Op_Q ->
        st#pop_st; `Ok
      | Op_cm m ->
        st#st#set_transformation_matrix
          (Pdftransform.matrix_compose m st#st#transformation_matrix);
        `Ok
      | _ -> `BadOp

    method color_op st op =
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
      | Op_k _ -> (* todo *) `Ok
      | _ -> `BadOp

    method text_state_op st op =
      let open Pdfops in
      let open State in
      let open Graphics in
      match op with
      (* Text state (Tc, Tw, Tz, TL, Tf, Tr, Ts) *)
      | Op_Tc c ->
        st#st#set_text {st#st#text with c}; `Ok
      | Op_Tw w ->
        st#st#set_text {st#st#text with w}; `Ok
      | Op_Tz scale ->
        st#st#set_text {st#st#text with h = scale /. 100. }; `Ok
      | Op_TL l ->
        st#st#set_text {st#st#text with l}; `Ok
      | Op_Tf (fontname, fs) ->
        let f =
          match Pdf.lookup_direct st#pdf "/Font" st#page.Pdfpage.resources with
          | None -> raise (Pdf.PDFError "Missing /Font in text extraction")
          | Some d ->
            match Pdf.lookup_direct st#pdf fontname d with
            | None -> raise (Pdf.PDFError "Missing font in text extraction")
            | Some d -> Some (d, Pdftext.read_font st#pdf d)
        in
        st#st#set_text {st#st#text with f; fs}; `Ok
      | Op_Tr _ -> (* todo *) `Ok
      | Op_Ts rise ->
        st#st#set_text {st#st#text with rise}; `Ok
      | _ -> `BadOp

    method private marked_content_aux m op =
      let open Pdfops in
      match op with
      (* Marked content (MP, DP, BMC, BDC, EMC) *)
      | Op_MP _
      | Op_DP (_, _) -> `Ok
      | Op_BMC tag ->
        m#set ((tag, None) :: m#get);
        `Ok
      | Op_BDC (tag, properties) ->
        m#set ((tag, Some properties) :: m#get);
        `Ok
      | Op_EMC ->
        (match m#get with
        | [] -> raise (Pdf.PDFError "EMC without a matching BMC or BDC")
        | _ :: xs -> m#set xs);
        `Ok
      | _ -> `BadOp

    method marked_content_op_at_pagedesc st m op =
      r#marked_content_aux m op

    method marked_content_op_at_textobj st text_st m op =
      r#marked_content_aux m op

    method text_showing_op st text_st op =
      let open Pdfops in
      let translate_tm tx ty =
        Pdftransform.(
          matrix_compose (mktranslate tx ty) text_st#transformation_matrix
        ) in
      let perform_Td tx ty =
        let new_m = Pdftransform.(
          matrix_compose (mktranslate tx ty) text_st#line_matrix
        ) in
        r#text_position st text_st ~tm:new_m ~lm:new_m ();
      in
      let perform_Tj txt =
        let width = character_codes_width st (character_codes_of_text st txt) in
        r#text_show st text_st ~width txt
      in
      let perform_' txt =
        perform_Td 0. st#st#text.State.Graphics.l;
        perform_Tj txt
      in

      match op with
      (* Text showing (Tj, TJ, ', '') *)
      | Op_Tj txt -> perform_Tj txt; `Ok
      | Op_' txt -> perform_' txt; `Ok
      | Op_'' (aw, ac, txt) ->
        st#st#set_text {st#st#text with
                        State.Graphics.w = aw;
                        State.Graphics.c = ac};
        perform_' txt;
        `Ok
      | Op_TJ (Pdf.Array l) ->
        List.iter (function
          | Pdf.String txt ->
            let width = character_codes_of_text st txt
                        |> character_codes_width st in
            r#text_show st text_st ~width txt
          | Pdf.Integer i ->
            let width = adjust_width st (float_of_int i) in
            r#text_position st text_st ~tm:(translate_tm width 0.) ()
          | Pdf.Real f ->
            let width = adjust_width st f in
            r#text_position st text_st ~tm:(translate_tm width 0.) ()
          | _ -> ()) l;
        `Ok
      | _ -> `BadOp

    method text_positioning_op st text_st op =
      let open Pdfops in
      let perform_Td tx ty =
        let new_m = Pdftransform.(
          matrix_compose (mktranslate tx ty) text_st#line_matrix
        ) in
        r#text_position st text_st ~tm:new_m ~lm:new_m ();
      in
      match op with
      (* Text positioning (Td, TD, Tm, T* ) *)
      | Op_Td (tx, ty) -> perform_Td tx ty; `Ok
      | Op_TD (tx, ty) ->
        st#st#set_text {st#st#text with State.Graphics.l = -. ty};
        perform_Td tx ty;
        `Ok
      | Op_Tm m ->
        r#text_position st text_st ~tm:m ~lm:m ();
        `Ok
      | Op_T' ->
        perform_Td 0. st#st#text.State.Graphics.l;
        `Ok
      | _ -> `BadOp

    method path_construction_op st path_st op =
      let open Pdfops in
      match op with
      (* Path construction (m, l, c, v, y, h, re) *)
      | Op_m (_, _)
      | Op_l (_, _)
      | Op_c (_, _, _, _, _, _)
      | Op_v (_, _, _, _)
      | Op_y (_, _, _, _)
      | Op_h
      | Op_re (_, _, _, _) -> (* todo *) `Ok
      | _ -> `BadOp

    method path_painting_op st path_st op =
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
      | Op_n -> (* todo *) `Ok
      | _ -> `BadOp

    method text_show _st text_st ~width _txt =
      text_st#set_transformation_matrix (Pdftransform.(
        matrix_compose (mktranslate width 0.) text_st#transformation_matrix
      ))

    method text_position st text_st ?tm ?lm () =
      (match tm with
       | Some m -> text_st#set_transformation_matrix m
       | None -> ());
      (match lm with
       | Some m -> text_st#set_line_matrix m
       | None -> ())

    method page doc page =
      let frame = page_box doc page in
      let init = State.page_desc_create ~pdf:doc ~page ~frame in
      let ops = Pdfops.parse_operators doc
          page.Pdfpage.resources page.Pdfpage.content in
      r#page_description_level init (State.MarkedBy.create ()) ops
  end
end
