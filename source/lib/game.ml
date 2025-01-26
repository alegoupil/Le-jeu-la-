open Iterator
open Config
open Input
type ball = (float * float) * (float * float) * bool
type raquette = float * float * bool
type score = int
type lives = int
type etat = raquette * ball * score * lives * (Briques.t * int)

(** [etat_init score lives] initialise l'état du jeu avec le score et le nombre de vies donnés.
  @param score le score initial du joueur
  @param lives le nombre initial de vies
  @return l'état initial du jeu sous la forme [(raquette, balle, score, vies, briques)] *)
let etat_init score lives briques=
 let raquette = 0., RacketConfig.position_y, false in
 let ball = BallConfig.initial_position, BallConfig.initial_speed, false in
 let score = score in
 let lives = lives in
 let briques = briques in
 raquette, ball, score, lives, briques

(** [integre dt flux] intègre un flux de valeurs avec un pas de temps [dt].
  @param dt le pas de temps utilisé pour l'intégration
  @param flux le flux de valeurs à intégrer
  @return un flux correspondant à l'intégrale des valeurs *)
let integre dt flux =
  assert (dt > 0.);
  let init = 0., 0. in
  let iter (acc1, acc2) (flux1, flux2) = acc1 +. (dt *. flux1), acc2 +. (dt *. flux2) in
  let rec acc = Tick (lazy (Some (init, Flux.map2 iter acc flux))) in
  acc


(** [contact_x br_qtree (x, y) (dx, dy)] vérifie si la balle entre en contact avec une surface verticale.
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return [true] si la balle entre en contact avec une surface verticale, [false] sinon *)
let contact_x br_qtree (x, y) (dx, dy) =
  (x > WindowConfig.width && dx >= 0.0)
  || (x < 0.0 && dx <= 0.0)
  || fst (Briques.detecter_contact br_qtree (x, y) (dx, dy))

(** [contact_y mouse_x br_qtree (x, y) (dx, dy)] vérifie si la balle entre en contact avec une surface horizontale.
  @param mouse_x position de la raquette
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return [true] si la balle entre en contact avec une surface horizontale, [false] sinon *)
let contact_y mouse_x br_qtree (x, y) (dx, dy) =
  (y > WindowConfig.height && dy >= 0.0)
  || Raquette.collision mouse_x (x, y) dy
  || snd (Briques.detecter_contact br_qtree (x, y) (dx, dy))

(** [rebond_x br_qtree (x, y) (dx, dy)] calcule la nouvelle vitesse sur l'axe x après un éventuel rebond horizontal.
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return nouvelle vitesse sur l'axe x après un rebond *)
let rebond_x br_qtree (x, y) (dx, dy) =
  if contact_x br_qtree (x, y) (dx, dy) then -.dx else dx

(** [rebond_y br_qtree mouse_x (x, y) (dx, dy)] calcule la nouvelle vitesse sur l'axe y après un éventuel rebond vertical.
  @param br_qtree arbre des briques
  @param mouse_x position de la raquette
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return nouvelle vitesse sur l'axe y après un rebond*)
let rebond_y br_qtree mouse_x (x, y) (dx, dy) =
  if contact_y mouse_x br_qtree (x, y) (dx, dy) then -.dy else dy


(** [update_score score nb_br_touched] met à jour le score du joueur en fonction des briques touchées.
  @param score le score actuel
  @param nb_br_touched le nombre de briques touchées depuis la dernière mise à jour
  @return un flux constant représentant le nouveau score *)
let update_score score nb_br_touched =
  assert (score >= 0 && nb_br_touched >= 0);
  Flux.constant (score + (nb_br_touched * BrickConfig.points_per_brick))

(** [update_raquette ()] génère le flux représentant la position et le mouvement de la raquette.
  @return un flux contenant la position, la vitesse et l'état du clic de la souris *)
let update_raquette () =
  Input.input_racket PhysicsConfig.timestep

(** [update_balle raquette_flux raquette ball br_qtree] crée le flux représentant l'état de la balle.
  @param raquette_flux flux de raquette (position, vitesse et état du clic)
  @param raquette la raquette actuelle
  @param ball l'état actuel de la balle
  @param br_qtree l'arbre des briques
  @return un flux représentant la position, la vitesse et l'état de lancement de la balle *)
let update_balle : raquette flux -> raquette -> ball -> Briques.t -> ball Flux.t =
 fun raquette_flux
     (mouse_x, mouse_dx, mouse_down)
     ((x, y), (dx, dy), is_launched)
     br_qtree ->
   let new_is_launched = is_launched || mouse_down in
   if new_is_launched
   then (
     let contact = Raquette.collision mouse_x (x, y) dy in
     let impulse = if contact then mouse_dx *. PhysicsConfig.impulse_factor else 0.0 in
     let ndx = rebond_x br_qtree (x, y) (dx, dy) +. impulse in
     let ndy = rebond_y br_qtree mouse_x (x, y) (dx, dy) in
     let a_flux = Flux.constant (0.0, -.PhysicsConfig.gravity) in
     let v_flux =
       Flux.map (fun (vx, vy) -> vx +. ndx, vy +. ndy) (integre PhysicsConfig.timestep a_flux)
     in
     let x_flux =
       Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre PhysicsConfig.timestep v_flux)
     in
     let is_launched_flux = Flux.constant new_is_launched in
     Flux.map3 (fun x v b -> x, v, b) x_flux v_flux is_launched_flux)
   else
     Flux.map2
       (fun (mouse_x, mouse_dx, _) dy ->
         ( (mouse_x, RacketConfig.position_y +. (BallConfig.radius /. 2.)),
           (mouse_dx, dy),
           new_is_launched ))
       raquette_flux
       (Flux.constant dy)


(** [update_quadrillage br_qtree ball] met à jour l'état des briques en fonction des collisions avec la balle.
  @param br_qtree l'arbre des briques actuel
  @param ball l'état actuel de la balle
  @return un flux contenant l'arbre des briques mis à jour et le nombre de briques touchées *)
let update_quadrillage : Briques.t -> ball -> (Briques.t * int) Flux.t =
 fun br_qtree ((x, y), (dx, dy), _) ->
   Flux.map
     (fun br_qtree -> Briques.maj_briques br_qtree (x, y) (dx, dy))
     (Flux.constant br_qtree)

(** [update_etat etat] met à jour l'état global du jeu en fonction des flux actuels.
  @param etat l'état actuel du jeu
  @return un flux contenant les mises à jour successives de l'état du jeu *)
let rec update_etat : etat -> etat Flux.t =
 fun etat ->
   let raquette, ball, score, lives, (br_qtree, nb_br_touched) = etat in
   let score_flux = update_score score nb_br_touched in
   let lives_flux = Flux.constant lives in
   let raquette_flux = update_raquette () in
   let ball_flux = update_balle raquette_flux raquette ball br_qtree in
   let briques_flux = update_quadrillage br_qtree ball in
   let update_cond : etat -> bool =
     fun ((mouse_x, _, mouse_down), ((x, y), (dx, dy), is_launched), _, _, (br_qtree, _)) ->
       ((not is_launched) && mouse_down)
       || contact_x br_qtree (x, y) (dx, dy)
       || contact_y mouse_x br_qtree (x, y) (dx, dy)
   in
   let death_cond : etat -> bool =
     fun (_, ((_, y), (_, dy), _), _, _, _) -> y < -.WindowConfig.margin && dy <= 0.0
   in
   let flux_continue =
     Flux.map5 (fun p b s l br -> p, b, s, l, br) raquette_flux ball_flux score_flux lives_flux briques_flux
   in
   let flux_death _ =
     if lives == 1 then Flux.vide else update_etat (etat_init score (lives - 1) (br_qtree, nb_br_touched))
   in
   Flux.unless (Flux.unless flux_continue update_cond update_etat) death_cond flux_death