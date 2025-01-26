(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open Config

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = PhysicsConfig.timestep (* Pas de temps défini dans la configuration *)
end

module Box = struct
  let marge = WindowConfig.margin
  let infx = 0.
  let infy = 0.
  let supx = WindowConfig.width
  let supy = WindowConfig.height
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float (Box.supx +. (2. *. Box.marge)))
    (int_of_float (Box.supy +. (2. *. Box.marge)))

(* Fonction pour dessiner l'état du jeu *)
let draw_state (raquette, ((bx, by), _, _), score, lives, (briques, _)) =
  (* Dessin des briques *)
  Briques.dessiner_briques briques;
  (* Dessin de la balle *)
  Graphics.set_color BallConfig.color;
  Graphics.fill_circle (int_of_float bx) (int_of_float by) (int_of_float BallConfig.radius);
  (* Dessin de la raquette *)
  let rx, _, _ = raquette in
  Graphics.set_color RacketConfig.color;
  Graphics.fill_rect
    (int_of_float (rx -. (RacketConfig.width /. 2.)))
    (int_of_float RacketConfig.position_y)
    (int_of_float RacketConfig.width)
    (int_of_float RacketConfig.height);
  (* Affichage du score et des vies *)
  Graphics.set_color Graphics.black;
  Graphics.moveto 10 (int_of_float (Box.supy +. 10.));
  Graphics.draw_string (Format.sprintf "Score: %d  Lives: %d" score lives)


(* Extrait le score courant d'un etat : *)
let score (_, _, score, _, _) = score

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.uncons flux_etat with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* Dessiner l'état actuel *)
      draw_state etat;
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let final_score = loop flux_etat 0 in
  Format.printf "Score final : %d\n" final_score;
  Graphics.close_graph ()

(* Initialisation et lancement du jeu *)
let () =
  let etat_initial = etat_init 0 GameConfig.initial_lives in
  let flux_etat = update_etat etat_initial in
  draw flux_etat