(** Module Input *)

(** Flux de paires (abscisse souris, booléen vrai si bouton appuyé) *)
val mouse : (float * bool) Flux.t

(** Flux de position de la souris (abscisse uniquement) *)
val mouse_position : float Flux.t

(** Flux des clics de souris (booléen vrai si bouton appuyé) *)
val mouse_click : bool Flux.t

(** [mouse_with_velocity dt] retourne un flux contenant la position de la souris
    et sa vitesse approximée, calculée à partir du déplacement entre deux itérations
    @param dt pas de temps entre deux itérations
    @return flux de paires (position de la souris, vitesse de la souris) *)
val mouse_with_velocity : float -> (float * float) Flux.t

(** [input_racket dt] génère un flux contenant la position, la vitesse de la souris,
    et un booléen indiquant si le bouton est appuyé
    @param dt pas de temps entre deux itérations
    @return flux de raquette *)
val input_racket : float -> (float * float * bool) Flux.t