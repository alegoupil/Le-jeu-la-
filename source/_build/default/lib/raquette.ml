open Config
open RacketConfig
open BallConfig



(** [edge_coord_float] calcule les coordonnées du rectangle de la raquette en type float.
    @param mouse_x Position horizontale de la souris.
    @return Les coordonnées (x1, y1, x2, y2) du rectangle de la raquette. *)
let racket_coord_float mouse_x =
  ( mouse_x -. (width /. 2.),
    position_y -. (height /. 2.),
    mouse_x +. (width /. 2.),
    position_y +. (height /. 2.) )

(** [edge_coord_int] calcule les coordonnées du rectangle de la raquette en type int.
    @param mouse_x Position horizontale de la souris.
    @return Les coordonnées (x1, y1, x2, y2) du rectangle de la raquette sous forme entière. *)
let racket_coord_int mouse_x =
  ( int_of_float (mouse_x -. (width /. 2.)),
    int_of_float (position_y -. (height /. 2.)),
    int_of_float (mouse_x +. (width /. 2.)),
    int_of_float (position_y +. (height /. 2.)) )

(** [contact] vérifie si la balle entre en collision avec la raquette.
    @param mouse_x Position horizontale de la souris.
    @param (bx, by) Position actuelle de la balle.
    @param dy Vitesse verticale de la balle.
    @return [true] si la balle est en collision avec la raquette, [false] sinon. *)
let collision mouse_x (bx, by) dy =
  let by = by -. radius in
  let x1, _, x2, y2 = racket_coord_float mouse_x in
  bx >= x1 && bx <= x2 && by >= 0. && by <= y2 && dy <= 0.

(** [draw_raquette] dessine la raquette à l'écran.
    @param mouse_x Position horizontale de la souris. *)
let draw_raquette mouse_x =
  Graphics.set_color color;
  let x1, y1, x2, y2 = racket_coord_int mouse_x in
  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
