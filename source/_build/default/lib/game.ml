open Iterator

open Config

open Briques
open Raquette
open Input
let game_hello () = print_endline "Hello, Newtonoiders!"

module FreeFall () =
  struct
    let (|+|) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
    let (|*|) k (x, y) = (k *. x, k *. y)

    let integre dt flux =
      let init = (0., 0.) in
      let rec acc =
        Tick (lazy (Some (init, Flux.map2 (fun a f -> a |+| (dt |*| f)) acc flux)))
      in acc

    let g = PhysicsConfig.gravity;;
    (* r = r0 + Integ dr
       dr = dr0 + Integ ddr
       ddr = 0, -g
     *)

    let run (position0, vitesse0) =
      let acceleration = Flux.constant (0., -. g) in (*à augmenter avec le score*)
      let vitesse      = Flux.(map2 ( |+| ) (constant vitesse0) (integre PhysicsConfig.timestep acceleration)) in
      let position     = Flux.(map2 ( |+| ) (constant position0) (integre PhysicsConfig.timestep vitesse)) in
      Flux.map2 (fun a b -> (a, b)) position vitesse
  end

let%test "vector_addition" = 
  let open FreeFall () in
  let v1 = (1.0, 2.0) in
  let v2 = (3.0, 4.0) in
  let result = v1 |+| v2 in
  result = (4.0, 6.0)
  
let%test "vector_scaling" =
  let open FreeFall () in
  let v = (1.0, 2.0) in
  let result = 2.0 |*| v in
  result = (2.0, 4.0)
  
let%test "freefall_integration" =
  let open FreeFall () in
  let flux = Flux.constant (0.0, -gravity) in
  match integre timestep flux with
  | Flux.Tick (lazy (Some ((x, y), _))) -> x = 0.0 && y = 0.0
  | _ -> false
  
let%test "freefall_run" =
  let open FreeFall () in
  let position0 = (0.0, 100.0) in
  let vitesse0 = (10.0, 0.0) in match run (position0, vitesse0) with
  | Flux.Tick (lazy ((pos, vel))) ->
  pos = position0 && vel = vitesse0
  | _ -> false 


module Bouncing () =
  struct
    (* version avec unfold sans récursivité directe *)
    let unless flux cond f_cond =
      Flux.unfold (fun (init, f) ->
          match Flux.uncons f with
               | None         -> None
               | Some (v, f') -> if not (init && cond v)
                                 then Some (v, (init, f'))
                                 else match Flux.uncons (f_cond v) with
                                      | None         -> None
                                      | Some (v, f') -> Some (v, (false, f'))
        ) (true, flux)

    (* version avec récursivité, donc paresse explicite *)
    let rec unless flux cond f_cond =
      Tick (lazy (
                match Flux.uncons flux with
                | None        -> None
                | Some (t, q) -> if cond t then Flux.uncons (f_cond t) else Some (t, unless q cond f_cond)
        ))

    (*Briques.detecter_contact briques (position_balle : float * float) (vecteur_vitesse : float * float) -> false, true contact horizontal*) 
    (** [contact] vérifie si la balle entre en collision avec la raquette.
    @param mouse_x Position horizontale de la souris.
    @param (bx, by) Position actuelle de la balle.
    @param dy Vitesse verticale de la balle.
    @return [true] si la balle est en collision avec la raquette, [false] sinon.
    let collision mouse_x (bx, by) dy *)
    
    let rebond ((x, y), (dx, dy)) (mx, mdx) =
      if (Raquette.collision mx (x,y) dy) then
        (x,y), (1. +. impulse_factor) *. dx, -. dy
      else 
        let (c_vertical, c_horizontal) = Briques.detecter_contact briques (x,y) (dx,dy) in
        (x, y),
        ((if c_horizontal then -. dx else dx),
        (if c_vertical then -. dy else dy))

    let contact ((x, y), (dx, dy)) =
      let (c_vertical, c_horizontal) = Briques.detecter_contact briques (x,y) (dx,dy) in
      c_vertical || c_horizontal || (Raquette.collision (fst Graphics.mouse_pos ()) (x,y) dy)


    module FF = FreeFall ()

    let rec run_aux etat0 m_flux =
      match Flux.uncons m_flux with
      | Some ((mx, mdx), m_flux_next) ->
        unless (FF.run etat0) contact (fun etat -> run_aux (rebond etat (mx,mdx)) m_flux_next)
    
    let run etat0 =
      let mouse_flux = mouse_with_velocity timestep in
      run_aux etat0 mouse_flux
  end

(*
Mise en place de quadtree (ne pas mettre de briques à cheval sur 2 quadtree)



Fonctions:
Position de la balle -> CalculNextPostion()
- Collision briques
- Colission Mur
- Collisions Raquette (si la plateforme arrive vite sur la balle, augmentation de la vitesse horrizontale)
Position de la raquette
- Placer au même endroit que la souris
- Calculer la vitesse ?


Gravité
Acceleration (plus la partie avance / briques cassées plus la balle "rebondi" fort)
Score
Vie
*)