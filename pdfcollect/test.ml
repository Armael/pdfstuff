module M = Pdfcollect.Make (Pdfcollect.BaseState)

class myreader = object
  inherit M.read as super

  method text_show st text_st ~width txt =
    print_string txt;
    super#text_show st text_st ~width txt

  method text_position st text_st ?tm ?lm () =
    match tm with
    | Some m ->
      let ctm = text_st#transformation_matrix in
      let (_, _, _, _, tx, ty) = Pdftransform.decompose ctm in
      let (_, _, _, _, tx', ty') = Pdftransform.decompose m in

      let w = M.character_codes_of_text st "m"
              |> M.character_codes_width st in
      let w' = M.get_width st (Char.code 'm') in
      let w'' = M.glyphspace_char_width st w' 109 in
      let dx = (tx' -. tx) in
      (* if abs_float dy < 1. && abs_float (dx -. w) < 10. then *)
      (*   print_string " "; *)

      if dx >= 0. then (
        Printf.printf "<%f,%f,%f,%f>" dx w w' w'';
        Printf.printf "<Th: %f>" st#st#text.h;
        Printf.printf "<Tc: %f>" st#st#text.c
      );
      super#text_position st text_st ?tm ?lm ()
    | None ->
      super#text_position st text_st ?tm ?lm ()

  method text_state_op st op =
    let ret = super#text_state_op st op in
    (match op with
     | Op_Tf _ ->
       (* (match st#st#text.f with *)
       (*  | Some (_, f) -> *)
       (*    print_endline "\n"; *)
       (*    Printf.printf "Font Size: %f\n" (st#st#text.fs); *)
       (*    print_endline (Pdftext.string_of_font f) *)
       (*  | _ -> failwith "erp") *)
       ()
     | _ -> ());
    ret
end

class myreader' = object
  inherit M.read as super

  method text_show st text_st ~width txt =
    print_string txt;
    super#text_show st text_st ~width txt

  method text_position st text_st ?tm ?lm () =
    match tm with
    | Some m ->
      let ctm = text_st#transformation_matrix in
      let (_, _, _, _, tx, _) = Pdftransform.decompose ctm in
      let (_, _, _, _, tx', _) = Pdftransform.decompose m in

      let w = M.character_codes_of_text st "i"
              |> M.character_codes_width st in
      let dx = (tx' -. tx) in

      if dx >= w then print_string " ";
      super#text_position st text_st ?tm ?lm ()
    | None ->
      super#text_position st text_st ?tm ?lm ()
end

let _ =
  let r = new myreader' in
  let pdf = Pdfread.pdf_of_file None None Sys.argv.(1) in
  let page = List.hd (Pdfpage.pages_of_pagetree pdf) in
  r#page pdf page;
  print_endline ""
