(* interface des itérateurs (implémentée par les flux) *)
module type Intf = sig
  type 'a t

  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val uncons : 'a t -> ('a * 'a t) option
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t ->'f t
  (**insère un flux dans un autre en partant du premier élément qui ne vérifie pas le prédicat. *)
  val unless : 'a t -> ('a -> bool) -> ('a -> 'a t) -> 'a t
end

type 'a flux = Tick of ('a * 'a flux) option Lazy.t

module Flux : Intf with type 'a t = 'a flux = struct
  type 'a t = 'a flux = Tick of ('a * 'a t) option Lazy.t

  let vide = Tick (lazy None)
  let cons t q = Tick (lazy (Some (t, q)))
  let uncons (Tick (lazy flux)) = flux

  let rec apply f x =
    Tick
      (lazy
        (match uncons f, uncons x with
         | None, _ -> None
         | _, None -> None
         | Some (tf, qf), Some (tx, qx) -> Some (tf tx, apply qf qx)))

  let rec unfold f e =
    Tick
      (lazy
        (match f e with
         | None -> None
         | Some (t, e') -> Some (t, unfold f e')))

  let rec filter p flux =
    Tick
      (lazy
        (match uncons flux with
         | None -> None
         | Some (t, q) -> if p t then Some (t, filter p q) else uncons (filter p q)))

  let rec append flux1 flux2 =
    Tick
      (lazy
        (match uncons flux1 with
         | None -> uncons flux2
         | Some (t1, q1) -> Some (t1, append q1 flux2)))

  let constant c = unfold (fun () -> Some (c, ())) ()

  (* implantation rapide mais inefficace de map *)
  let map f i = apply (constant f) i
  let map2 f i1 i2 = apply (apply (constant f) i1) i2
  let map3 f i1 i2 i3 = apply (apply (apply (constant f) i1) i2) i3
  let map4 f i1 i2 i3 i4 = apply (apply (apply (apply (constant f) i1) i2) i3) i4
  let map5 f i1 i2 i3 i4 i5 = apply (apply (apply (apply (apply (constant f) i1) i2) i3) i4) i5
  let rec unless flux cond f_flux =
    Tick
      (lazy
        (match uncons flux with
         | None -> None
         | Some (a, fl) ->
           if cond a then uncons (f_flux a) else Some (a, unless fl cond f_flux)))
end
