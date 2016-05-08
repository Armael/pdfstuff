open Gg

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
      transformation_matrix: Pdftransform.transform_matrix;
      set_transformation_matrix : Pdftransform.transform_matrix -> unit;
      line_matrix: Pdftransform.transform_matrix;
      set_line_matrix : Pdftransform.transform_matrix -> unit;
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

module BaseState : State

module Make : functor (State: State) -> sig
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

  class ['a] read : ['a] reader

  (* Utilities *)
  val rendering_matrix :
    State.page_desc -> State.Text.t -> Pdftransform.transform_matrix
  val character_codes_of_text :
    State.page_desc -> string -> int list
  val character_codes_width :
      State.page_desc -> [`CharCodes of int list | `Adjust of float] list ->
      float
end
