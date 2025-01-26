(* Configuration des paramètres du jeu *)

(** Dimensions et marges de la fenêtre de jeu *)
module WindowConfig = struct
  let height = 600.
  let width = 800.
  let margin = 10.
end

(** Propriétés de la balle *)
module BallConfig = struct
  let radius = 10.0

  let initial_position = (50.,50.)
  let initial_speed = (500.,500.)
  let color = Graphics.rgb 255 0 0
  
end

(** Paramètres de la physique *)
module PhysicsConfig = struct
  (* Gravité *)
  let gravity = 200.

  (* Pas de temps minimal : une valeur plus élevée correspond à un meilleur rafraîchissement si l'écran le supporte *)
  let timestep = 1. /. 60. (* 60 Hz *)

  (* Facteur d'impulsion appliqué à la vitesse de la palette *)
  let impulse_factor = 0.3
end

(** Configuration des briques *)
module BrickConfig = struct
  let height = 50.
  let width = 100.

  (* Points attribués par brique détruite *)
  let points_per_brick = 100

  (* Liste initiale des briques : ((position_x, position_y) * couleur) list *)
  let initial_bricks =
    let create_brick x y =
      (x, y), Graphics.rgb 255 255 0
    in
    (* Remplit l'écran de briques *)
    let rec fill_bricks acc x y max_x max_y =
      if x > max_x then fill_bricks acc 0. (y +. height) max_x max_y
      else if y > max_y then acc
      else fill_bricks (create_brick x y :: acc) (x +. width) y max_x max_y
    in
    fill_bricks [] 0. (4. *. height) WindowConfig.width WindowConfig.height
end

(** Configuration de la palette *)
module RacketConfig = struct
  let width = 100.0
  let height = 10.0
  let color = Graphics.rgb 0 0 0
  let position_y = 20.0
end

(** Autres paramètres de jeu *)
module GameConfig = struct
  let initial_lives = 3
end
