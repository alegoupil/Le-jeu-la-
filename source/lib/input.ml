open Iterator

(* flux de paires (abscisse souris, booléen vrai si bouton appuyé) *)
let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()

(* flux de position de la souris (abscisse uniquement) *)
let mouse_position =
  Flux.map (fun (x, _) -> x) mouse

(* flux des clics de souris (booléen vrai si bouton appuyé) *)
let mouse_click =
  Flux.map (fun (_, button_down) -> button_down) mouse

(** [mouse_with_velocity dt] retourne un flux contenant la position de la souris
  et sa vitesse approximée, calculée à partir du déplacement entre deux itérations
  @param dt pas de temps entre deux itérations
  @return flux de paires (position de la souris, vitesse de la souris) *)
let mouse_with_velocity dt =
  Flux.unfold
    (fun (prev_x, prev_time) ->
      let x, _ = Graphics.mouse_pos () in
      let current_time = Unix.gettimeofday () in
      let velocity = (float_of_int x -. prev_x) /. (current_time -. prev_time) in
      Some ((float_of_int x, velocity), (float_of_int x, current_time)))
    (0., Unix.gettimeofday ())

(** [input_racket dt] génère un flux contenant la position, la vitesse de la souris,
  et un booléen indiquant si le bouton est appuyé
  @param dt pas de temps entre deux itérations
  @return flux de raquette *)
let input_racket dt =
  let mouse_flux = mouse_with_velocity dt in
  Flux.map2
    (fun (x, velocity) button_down -> (x, velocity, button_down))
    mouse_flux
    mouse_click
