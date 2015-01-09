(* SPEC:

   - On commence dans l'état "standard". Dans cet êtat on ne peut qu'écrire du
   texte en utilisant l'opérateur [Txt]; OU passer dans l'état "dessin"

   - On passe dans l'état "dessin" grâce à l'opérateur [Begin], et on le quitte
   grâce à [End] (on retourne alors à l'état "standard"). Dans l'état "dessin"
   on peut utiliser l'opérateur [Draw], et on ne peut plus utiliser [Txt].
*)

(* Flux brut en entrée; ne respecte potentiellement pas la spec *)
type tok =
| Txt of string
| Begin
| Draw of int
| End

type input = tok list

module Gadts = struct
  (* Les deux états internes de notre interpréteur parcourant le flux:
     "standard" et "dessin" *)
  type std_st = { txt : string }
  type drawing_st = { txt : string; drawing : int list }
    
  (* Liste de tokens "enrichie" (les tokens servent également de cellules
     cons) : le flux est "annoté" par l'état courant (l'annotation correspond au
     paramètre de [st]).

     Un tel flux respecte nécessairement la spec. *)
  type _ st =
  | Init : std_st st
  | Txt : std_st st * string -> std_st st
  | BDessin : std_st st -> drawing_st st
  | Draw : drawing_st st * int -> drawing_st st
  | EDessin : drawing_st st -> std_st st

  (* Fonctions effectuant la conversion "flux brut (de type [input]) -> _
     st". Il s'agit de lancer des erreurs lorsqu'un token ne respecte la spec
     (Txt dans l'état "dessin", ...) *)
  let rec std_st_of_toks (acc: std_st st): input -> std_st st = function
    | (Txt t)::xs -> std_st_of_toks (Txt (acc, t)) xs
    | Begin::xs ->
      let (st, toks) = end_drawing_st (BDessin acc) xs in
      std_st_of_toks st toks
    | (Draw i)::_ -> failwith "Begin needed"
    | End::_ -> failwith "Begin needed"
    | [] -> acc

  and end_drawing_st (acc: drawing_st st): input -> std_st st * input = function
    | (Txt _)::_ -> failwith "Txt forbidden in drawing block"
    | Begin::_ -> failwith "Begin forbidden in drawing block"
    | (Draw i)::xs -> end_drawing_st (Draw (acc, i)) xs
    | End::xs -> (EDessin acc, xs)
    | [] -> failwith "Drawing blocks must be closed"

  let parse_toks = std_st_of_toks Init

  let rec eval : type a. a st -> a = function
    | Init -> { txt = "" }
    | Txt (st, s) ->
      let sst : std_st = eval st in
      { txt = sst.txt ^ s }
    | BDessin st ->
      { txt = (eval st).txt; drawing = [] }
    | Draw (st, i) ->
      let dst : drawing_st = eval st in
      { dst with drawing = i::dst.drawing }
    | EDessin st ->
      let dss : drawing_st = eval st in
      (* Dummy drawing operation *)
      let the_drawing = List.fold_left
        (fun acc i -> (string_of_int i) ^ acc) "" dss.drawing
      in
      { txt = dss.txt ^ the_drawing }
end
