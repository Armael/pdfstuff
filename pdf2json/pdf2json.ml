open Batteries
open CamomileLibraryDyn.Camomile
open Gg
module Json = Yojson.Basic

let det (x, y) (x', y') =
  x *. y' -. y *. x'

let cross (x, y) (x', y') =
  x *. x' -. y *. y'

let segment_intersect ((ax, ay), (bx, by)) ((cx, cy), (dx, dy)) =
  let u = bx -. ax
  and v = by -. ay
  and w = cx -. dx
  and x = cy -. dy in
  
  let d = det (u, v) (w, x) in
  if Float.equal d 0. then
    false
  else (
    let s = det (cx -. ax, cy -. ay) (w, x) /. d in
    let t = det (u, v) (cx -. ax, cy -. ay) /. d in
    s >= 0. && s <= 1. && t >= 0. && t <= 1.
  )

let iso_8859 = CharEncoding.of_name "ISO-8859-15"

module UTF8CaseMap = CaseMap.Make (UTF8)

let f2i (x, y) = (int_of_float x, int_of_float y)

let blocs_of_collect (lines: (p2 * p2) list) (texts: (p2 * string) list) =
  let cross_a_line p1 p2 =
    List.exists (fun (p3, p4) ->
      segment_intersect
        (V2.to_tuple p1, V2.to_tuple p2)
        (V2.to_tuple p3, V2.to_tuple p4)
    ) lines
  in
  List.fold_left (fun (pos, cur, blocs) (txtpos, txt) ->
    if cross_a_line pos txtpos then
      (txtpos, [(txtpos, txt)], (List.rev cur)::blocs)
    else
      (txtpos, (txtpos, txt)::cur, blocs)
  ) (V2.zero, [], []) texts
  |> Tuple3.get23 |> Tuple2.map1 List.rev |> uncurry List.cons |> List.rev

let string_block_of_raw (block: (p2 * string) list): string list =
  if block = [] then []
  else
    List.group_consecutive (fun (p1, _) (p2, _) ->
      abs_float (V2.y p1 -. V2.y p2) <= 2.)
      block
    |> List.map (List.map snd %> List.reduce (^))
    |> List.map (CharEncoding.recode_string
                   ~in_enc:iso_8859
                   ~out_enc:CharEncoding.utf8)
    |> List.map UTF8CaseMap.titlecase

type plat = { nom: string; choix: string list }
type jour = { jour_nb: int; jour: string; plats: plat list }
type menu = jour list

let plat_of_block (block: string list): plat =
  { nom = List.hd block;
    choix =
      let (c, cs) =
        List.fold_left (fun (cur_choice, acc) x ->
            if String.length x > 0 && x.[0] = '-' then
              (String.sub x 1 (String.length x - 1), cur_choice :: acc)
            else
              (cur_choice ^ " " ^ x, acc)
        ) ("", []) (List.tl block)
      in
      (* Remove the dummy "" *)
      List.tl (List.rev (c::cs))
  }

let menu_of_blocks (blocks: string list list): menu =
  let days = ["Lundi"; "Mardi"; "Mercredi"; "Jeudi"; "Vendredi"] in
  let is_day_block b = List.mem (List.hd b) days in

  blocks
  |> List.drop_while (neg is_day_block)

  |> List.fold_left (fun (cur_day, acc) x ->
    if is_day_block x then
      ([x], (List.rev cur_day) :: acc)
    else
      (x :: cur_day, acc)
  ) ([], [])
  |> (fun (x, xs) -> (List.rev x)::xs)
  |> List.rev

  |> List.filter_map (fun day ->
    if day = [] then None
    else
      let monthday = List.find_map (fun s ->
        try Some (Scanf.sscanf s " %d " identity)
        with End_of_file | Scanf.Scan_failure _ -> None
      ) (List.hd day) in
      Some { jour_nb = monthday;
             jour = (List.hd % List.hd) day;
             plats = List.map plat_of_block (List.tl day)
           }
  )

let json_of_menu (m: menu): Json.json =
  let open Json in
  `List (List.map (fun day ->
    `Assoc [
      "JourNb", `Int day.jour_nb;
      "Jour", `String day.jour;
      "Menu", `List (List.map (fun plat ->
        `Assoc [
          "Nom", `String plat.nom;
          "Contenu", `List (List.map (fun s -> `String s) plat.choix)
        ]
      ) day.plats)
    ]
  ) m)

let dump pdfname =
  let pdf = Pdfread.pdf_of_file None None pdfname in
  let page = List.find (fun page ->
    let ops = Pdfops.parse_operators pdf
      page.Pdfpage.resources
      page.Pdfpage.content in
    (try List.find (function
    | Pdfops.Op_Tj _ -> true
    | Pdfops.Op_TJ _ -> true
    | _ -> false) ops |> ignore; true with
      Not_found -> false)
  ) (Pdfpage.pages_of_pagetree pdf) in

  Pdfcollect.collect pdf page
  |> Tuple3.get13
  |> uncurry blocs_of_collect
  |> List.map string_block_of_raw
  |> List.filter ((<>) []) (* Remove empty blocks *)
  |> menu_of_blocks
  |> json_of_menu
  |> Json.to_string
  |> print_string

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | [pdfname] ->
    dump pdfname
  | _ -> Printf.printf "%s <input pdf>\n" Sys.argv.(0)
