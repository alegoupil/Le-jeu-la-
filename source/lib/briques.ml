open Config
open Quadtree

(** type d'une brique : coordonnées de son coin inférieur gauche et sa couleur *)
type br = (float * float) * Graphics.color
(** collection contenant toutes les briques *)
type t = br Quadtree.quadtree

(** nombre de briques en longueur et en hauteur : les briques sont agencées sur
    une grille de briques avec toutes les briques de la même taille *)
let nb_briques_hauteur = WindowConfig.width /. BrickConfig.width
let nb_briques_largeur = WindowConfig.height /. BrickConfig.height

(** [point_de_contact (x, y) (vx, vy)] calcule les points de contact de la balle en mouvement.
  @param (x, y) la position actuelle de la balle
  @param (vx, vy) le vecteur de vitesse de la balle
  @return un quadruplet [(cx, cy, mx, my)] où :
    - [(cx, cy)] représente le point de contact de la balle dans la direction du mouvement
    - [(mx, my)] représente le point opposé avec une marge supplémentaire *)
let point_de_contact (x, y) (vx, vy) =
  let norm = hypot vx vy in
  let scale k = (k *. vx /. norm, k *. vy /. norm) in
  let contact_x, contact_y = scale BallConfig.radius in
  let marge_x, marge_y = scale (BallConfig.radius +. 50.) in
  x +. contact_x, y +. contact_y, x -. marge_x, y -. marge_y

let%test "point_de_contact_horizontal" =
  let result = point_de_contact (0., 0.) (1., 0.) in
  result = (BallConfig.radius, 0., -.BallConfig.radius -. 50., 0.)

let%test "point_de_contact_vertical" =
  let result = point_de_contact (0., 0.) (0., 1.) in
  result = (0., BallConfig.radius, 0., -.BallConfig.radius -. 50.)

(** [calculer_coordonnees_brique (x, y)] calcule les coordonnées de la brique contenant le point [(x, y)].
  @param (x, y) les coordonnées d'un point dans l'espace du jeu
  @return un couple [(coin_inf, coin_sup)] représentant :
    - [coin_inf] : les coordonnées du coin inférieur gauche de la brique
    - [coin_sup] : les coordonnées du coin supérieur droit de la brique *)
let calculer_coordonnees_brique (x, y) =
  let abs = floor (x /. BrickConfig.width) *. BrickConfig.width in
  let ord = floor (y /. BrickConfig.height) *. BrickConfig.height in
  (abs, ord), (abs +. BrickConfig.width, ord +. BrickConfig.height)

let%test "coin_inferieur_gauche" =
  let result = calculer_coordonnees_brique (0., 0.) in
  let attendu = ((0., 0.), (BrickConfig.width, BrickConfig.height)) in
  result = attendu

let%test "centre_brique" =
  let result = calculer_coordonnees_brique (BrickConfig.width /. 2., BrickConfig.height /. 2.) in
  let attendu = ((0., 0.), (BrickConfig.width, BrickConfig.height)) in
  result = attendu

let%test "point_proche_bord" =
  let result = calculer_coordonnees_brique (BrickConfig.width /. 2. +. 1., BrickConfig.height /. 2. +. 1.) in
  let attendu = ((0., 0.), (BrickConfig.width, BrickConfig.height)) in
  result = attendu

(** [contact_brique brique pos_balle vecteur_vitesse] détermine si une balle entre en contact avec une brique.
  @param brique la brique représentée par ses coordonnées inférieures et ses dimensions
  @param pos_balle la position actuelle de la balle [(x, y)]
  @param vecteur_vitesse le vecteur de vitesse de la balle [(vx, vy)]
  @return un couple [(contact_vertical, contact_horizontal)] où :
    - [contact_vertical] est [true] si la balle entre en contact avec la brique sur une face verticale
    - [contact_horizontal] est [true] si la balle entre en contact avec la brique sur une face horizontale *)
let contact_brique (brique : br) (pos_balle : float * float) (vecteur_vitesse : float * float) : bool * bool =
  let (coin_inf_x, coin_inf_y), _ = brique in
  let coin_sup_x = coin_inf_x +. BrickConfig.width in
  let coin_sup_y = coin_inf_y +. BrickConfig.height in
  let pc_x, pc_y, mc_x, mc_y = point_de_contact pos_balle vecteur_vitesse in
  let contact_vertical =
    (mc_x < coin_inf_x && pc_x >= coin_inf_x && (snd pos_balle) >= coin_inf_y && pc_y <= coin_sup_y)
    || (mc_x > coin_sup_x && pc_x <= coin_sup_x && (snd pos_balle) >= coin_inf_y && pc_y <= coin_sup_y)
  in
  let contact_horizontal =
    (mc_y < coin_inf_y && pc_y >= coin_inf_y && (fst pos_balle) >= coin_inf_x && (fst pos_balle) <= coin_sup_x)
    || (mc_y > coin_sup_y && pc_y <= coin_sup_y && (fst pos_balle) >= coin_inf_x && (fst pos_balle) <= coin_sup_x)
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

(** [detecter_contact briques position_balle vecteur_vitesse] détecte si une collision entre la balle et une brique a lieu.
  @param briques un quadtree contenant les briques actuelles
  @param position_balle la position actuelle de la balle sous la forme [(x, y)]
  @param vecteur_vitesse le vecteur de vitesse actuel de la balle sous la forme [(dx, dy)]
  @return un couple [(contact_vertical, contact_horizontal)] indiquant :
    - [contact_vertical] : [true] si un contact vertical avec une brique est détecté
    - [contact_horizontal] : [true] si un contact horizontal avec une brique est détecté *)
let detecter_contact briques (position_balle : float * float) (vecteur_vitesse : float * float) =
  let pc_x, pc_y, _, _ = point_de_contact position_balle vecteur_vitesse in
  let point_contact = fst (calculer_coordonnees_brique (pc_x, pc_y)) in
  match Quadtree.get briques point_contact with
  | None -> false, false
  | Some brique -> contact_brique brique position_balle vecteur_vitesse

(** [maj_briques arbre_briques position_balle vecteur_vitesse] met à jour le quadtree des briques en supprimant celles qui entrent en collision avec la balle.
  @param arbre_briques un quadtree contenant les briques actuelles
  @param position_balle la position actuelle de la balle sous la forme [(x, y)]
  @param vecteur_vitesse le vecteur de vitesse actuel de la balle sous la forme [(dx, dy)]
  @return un couple [(nouvel_arbre, nombre_briques_supprimees)] où :
    - [nouvel_arbre] est le quadtree mis à jour sans les briques touchées
    - [nombre_briques_supprimees] est le nombre de briques qui ont été retirées *)
let maj_briques arbre_briques position_balle vecteur_vitesse =
  Quadtree.filtre_compter_retirer arbre_briques (fun brique ->
    let contact_vertical, contact_horizontal = contact_brique brique position_balle vecteur_vitesse in
    not (contact_vertical || contact_horizontal))

(** [inserer_brique br_qtree br] insère une nouvelle brique dans le quadtree à l'emplacement aligné correspondant.
  @param br_qtree un quadtree contenant les briques actuelles
  @param br une brique sous la forme [(coord, color)] où :
    - [coord] représente la position de la brique [(x, y)]
    - [color] est la couleur de la brique
  @return le quadtree [br_qtree] mis à jour avec la nouvelle brique insérée *)
let inserer_brique : br Quadtree.quadtree -> br -> br Quadtree.quadtree =
  fun br_qtree br ->
    let coord, color = br in
    let coord_aligned, _ = calculer_coordonnees_brique coord in
    insert br_qtree coord_aligned (coord_aligned, color)

let%test "insertion_unique" =
  let arbre_vide = Quadtree.vide ((0., 0.), (WindowConfig.width, WindowConfig.height)) in
  let brique = ((50., 50.), Graphics.red) in
  let coord_aligned, _ = calculer_coordonnees_brique (50., 50.) in
  let arbre_avec_brique = inserer_brique arbre_vide brique in
  Quadtree.get arbre_avec_brique coord_aligned = Some (coord_aligned, Graphics.red)

let%test "insertion_multiple" =
  let arbre_vide = Quadtree.vide ((0., 0.), (WindowConfig.width, WindowConfig.height)) in
  let brique1 = ((50., 50.), Graphics.red) in
  let brique2 = ((150., 50.), Graphics.blue) in
  let coord1, _ = calculer_coordonnees_brique (50., 50.) in
  let coord2, _ = calculer_coordonnees_brique (150., 50.) in
  let arbre = inserer_brique (inserer_brique arbre_vide brique1) brique2 in
  Quadtree.get arbre coord1 = Some (coord1, Graphics.red)
  && Quadtree.get arbre coord2 = Some (coord2, Graphics.blue)

let%test "insertion_duplicat" =
  let arbre_vide = Quadtree.vide ((0., 0.), (WindowConfig.width, WindowConfig.height)) in
  let brique1 = ((50., 50.), Graphics.red) in
  let brique2 = ((50., 50.), Graphics.green) in
  let coord_aligned, _ = calculer_coordonnees_brique (50., 50.) in
  let arbre = inserer_brique (inserer_brique arbre_vide brique1) brique2 in
  (* Vérifie que la dernière brique insérée remplace la première *)
  Quadtree.get arbre coord_aligned = Some (coord_aligned, Graphics.green)


let vide = Quadtree.vide ((0., 0.), (WindowConfig.width, WindowConfig.height))

(** [collection_briques br_list] crée un quadtree contenant toutes les briques de la liste donnée.
  @param br_list une liste de briques sous la forme [(coord, color)] où :
    - [coord] représente la position de la brique [(x, y)]
    - [color] est la couleur de la brique
  @return un quadtree contenant toutes les briques *)
let collection_briques br_list =
  List.fold_left inserer_brique vide br_list

(** [dessiner_brique (coord, couleur)] dessine une brique avec la position et la couleur données.
  @param coord la position de la brique [(x, y)]
  @param couleur la couleur de la brique, représentée sous forme d'un entier RGB *)
let dessiner_brique ((x, y), couleur) =
  let colors = Graphics.rgb 0 0 0 in
  Graphics.set_color couleur;
  Graphics.fill_rect (int_of_float x) (int_of_float y) (int_of_float BrickConfig.width) (int_of_float BrickConfig.height);
  Graphics.set_color colors;
  Graphics.draw_rect (int_of_float x) (int_of_float y) (int_of_float BrickConfig.width) (int_of_float BrickConfig.height)

(** [dessiner_briques briques] dessine toutes les briques présentes dans le quadtree.
  @param briques un quadtree contenant des briques sous la forme [(coord, color)] *)
let dessiner_briques briques =
  Quadtree.map briques dessiner_brique
