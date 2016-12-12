open Gg

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
      transformation_matrix: Pdftransform.transform_matrix;
      set_transformation_matrix : Pdftransform.transform_matrix -> unit;
      line_matrix: Pdftransform.transform_matrix;
      set_line_matrix : Pdftransform.transform_matrix -> unit;
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
      transformation_matrix: Pdftransform.transform_matrix; (* CTM *)
      set_transformation_matrix : Pdftransform.transform_matrix -> unit;
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

module BaseState : State

type op_result = [ `Ok | `BadOp ]

module Make : functor (State: State) -> sig
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

  class read : reader

  (* Utilities *)

  (* Text space -> device space transformation matrix *)
  val rendering_matrix :
    State.page_desc -> State.Text.t -> Pdftransform.transform_matrix

  val character_codes_of_text :
    State.page_desc -> string -> int list

  (* In text space units *)
  val adjust_width : State.page_desc -> float -> float

  (* takes text space units, in text space units *)
  val char_width : State.page_desc -> float -> int -> float

  val glyphspace_to_textspace : State.page_desc -> float -> float

  val glyphspace_char_width : State.page_desc -> float -> int -> float

  (* In text space units *)
  val character_codes_width : State.page_desc -> int list -> float

  val get_width : State.page_desc -> int -> float
  val get_kern : State.page_desc -> int -> int -> float
end
