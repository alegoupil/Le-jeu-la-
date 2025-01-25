open Init_values
open BriquesInit

(** type d'une brique : coordonnées de son coin inférieur gauche et sa couleur *)
type br = (float * float) * Graphics.color
(** collection contenant toutes les briques *)
type t = br Quadtree.t

(** nombre de briques en longueur et en hauteur : les briques sont agencées sur
    une grille de briques avec toutes les briques de la même taille *)
let nb_briques_hauteur = BoxInit.width /. br_width
let nb_briques_largeur = BoxInit.height /. br_height

let point_de_contact (x, y) (vx, vy) =
  let norm = hypot vx vy in
  let scale k = (k *. vx /. norm, k *. vy /. norm) in
  let contact_x, contact_y = scale BallInit.radius in
  let marge_x, marge_y = scale (BallInit.radius +. 50.) in
  x +. contact_x, y +. contact_y, x -. marge_x, y -. marge_y

let%test "point_de_contact_horizontal" =
  let result = point_de_contact (0., 0.) (1., 0.) in
  result = (BallInit.radius, 0., -.BallInit.radius -. 50., 0.)

let%test "point_de_contact_vertical" =
  let result = point_de_contact (0., 0.) (0., 1.) in
  result = (0., BallInit.radius, 0., -.BallInit.radius -. 50.)

let calculer_coordonnees_brique (x, y) =
  let abs = floor (x /. br_width) *. br_width in
  let ord = floor (y /. br_height) *. br_height in
  (abs, ord), (abs +. br_width, ord +. br_height)

let%test "coin_inferieur_gauche" =
  let result = calculer_coordonnees_brique (0., 0.) in
  let attendu = ((0., 0.), (br_width, br_height)) in
  result = attendu

let%test "centre_brique" =
  let result = calculer_coordonnees_brique (br_width /. 2., br_height /. 2.) in
  let attendu = ((0., 0.), (br_width, br_height)) in
  result = attendu

let%test "point_proche_bord" =
  let result = calculer_coordonnees_brique (br_width /. 2. +. 1., br_height /. 2. +. 1.) in
  let attendu = ((0., 0.), (br_width, br_height)) in
  result = attendu

let contact_brique (brique : br) (pos_balle : float * float) (vecteur_vitesse : float * float) : bool * bool =
  let (coin_inf_x, coin_inf_y), _ = brique in
  let coin_sup_x = coin_inf_x +. br_width in
  let coin_sup_y = coin_inf_y +. br_height in
  let pc_x, pc_y, mc_x, mc_y = point_de_contact pos_balle vecteur_vitesse in
  let contact_vertical =
    (mc_x < coin_inf_x && pc_x >= coin_inf_x && snd pos_balle >= coin_inf_y && pc_y <= coin_sup_y)
    || (mc_x > coin_sup_x && pc_x <= coin_sup_x && snd pos_balle >= coin_inf_y && pc_y <= coin_sup_y)
  in
  let contact_horizontal =
    (mc_y < coin_inf_y && pc_y >= coin_inf_y && fst pos_balle >= coin_inf_x && fst pos_balle <= coin_sup_x)
    || (mc_y > coin_sup_y && pc_y <= coin_sup_y && fst pos_balle >= coin_inf_x && fst pos_balle <= coin_sup_x)
  in
  contact_vertical, contact_horizontal

let%test "contact_horizontal_seul" =
  let brique = ((0., 0.), Graphics.red) in
  let pos_balle = (0., 0.) in
  let vecteur_vitesse = (0., 1.) in
  contact_brique brique pos_balle vecteur_vitesse = (false, true)

let%test "contact_diagonal" =
  let brique = ((0., 0.), Graphics.red) in
  let pos_balle = (10., 20.) in
  let vecteur_vitesse = (10., 10.) in
  contact_brique brique pos_balle vecteur_vitesse = (true, true)

let%test "aucun_contact" =
  let brique = ((50., 50.), Graphics.red) in
  let pos_balle = (0., 0.) in
  let vecteur_vitesse = (1., 1.) in
  contact_brique brique pos_balle vecteur_vitesse = (false, false)

let detecter_contact briques (position_balle : float * float) (vecteur_vitesse : float * float) =
  let point_contact = fst (calculer_coordonnees_brique (fst (point_de_contact position_balle vecteur_vitesse))) in
  match Quadtree.get briques point_contact with
  | None -> false, false
  | Some brique -> contact_brique brique position_balle vecteur_vitesse


let maj_briques arbre_briques position_balle vecteur_vitesse =
  Quadtree.filter arbre_briques (fun brique ->
    let contact_vertical, contact_horizontal = contact_brique brique position_balle vecteur_vitesse in
    not (contact_vertical || contact_horizontal))

let inserer_brique arbre_briques brique =
  let coordonnees_alignees, _ = calculer_coordonnees_brique (fst brique) in
  Quadtree.insert arbre_briques coordonnees_alignees brique

let vide = Quadtree.empty ((0., 0.), (BoxInit.width, BoxInit.height))

let collection_briques br_list =
  List.fold_left inserer_brique vide br_list


let dessiner_brique ((x, y), couleur) =
  Graphics.set_color couleur;
  Graphics.fill_rect (int_of_float x) (int_of_float y) (int_of_float br_width) (int_of_float br_height)

let dessiner_briques briques = Quadtree.iter_val briques dessiner_brique
