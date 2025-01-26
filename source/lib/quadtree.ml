type coord = float * float
type case = coord * coord

type 'a quadtree =
  | Vide of case
  | Feuille of case * coord * 'a
  | Noeud of case * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree

let centre_case ((x1, y1), (x2, y2)) = (x1 +. x2) /. 2., (y1 +. y2) /. 2.

let%test _ = centre_case((0.,0.), (1.,1.)) = (0.5,0.5)
let%test _ = centre_case((0.,0.), (0.,0.)) = (0.,0.)
let nord_ouest : case -> case =
  fun case ->
  let (x1, y1), (_, _) = case in
  let x2, y2 = centre_case case in
  (x1, y1), (x2, y2)

let sud_ouest : case -> case =
  fun case ->
  let (_, y1), (x2, _) = case in
  let x1, y2 = centre_case case in
  (x1, y1), (x2, y2)

let nord_est : case -> case =
  fun case ->
  let (x1, _), (_, y2) = case in
  let x2, y1 = centre_case case in
  (x1, y1), (x2, y2)

let sud_est : case -> case =
  fun case ->
  let (_, _), (x2, y2) = case in
  let x1, y1 = centre_case case in
  (x1, y1), (x2, y2)
  
let%test _ = nord_ouest((1.,0.),(0.,1.)) = ((1.,0.),(0.5,0.5))
let%test _ = nord_est((1.,0.),(0.,1.)) = ((1.,0.5),(0.5,1.))
let%test _ = sud_ouest((1.,0.),(0.,1.)) = ((0.5,0.),(0.,0.5))
let%test _ = sud_est((1.,0.),(0.,1.)) = ((0.5,0.5),(0.,1.))

let vide case = Vide case

let rec get : 'a quadtree -> coord -> 'a option =
  fun tree (x, y) ->
    match tree with 
   | Vide _ ->  None
   | Feuille (_, _, elem) -> Some(elem)
   | Noeud (c,t1,t2,t3,t4) -> 
    let x1, y1 = centre_case c in
    if x < x1
    then if y < y1 then get t1 (x, y) else get t2 (x, y)
    else if y < y1
    then get t3 (x, y)
    else get t4 (x, y) 

let quadtree_test = Noeud (((1.,0.),(0.,1.)),Vide ((1.,0.),(0.5,0.5)),Feuille(((1.,0.5),(0.5,1.)),(0.75,0.75),1),Vide ((0.5,0.),(0.,0.5)),Vide ((0.5,0.5),(0.,1.)))
let coord_test1 = (0.75,0.75)
let coord_test2 = (0.,0.)
let%test _ = get quadtree_test coord_test1 = Some(1)
let%test _ = get quadtree_test coord_test2 = None

let rec insert : 'a quadtree -> coord -> 'a -> 'a quadtree =
  fun t c v ->
  match t with
  | Vide b -> Feuille (b, c, v)
  | Feuille (b, oc, ov) ->
    if c = oc
    then Feuille (b, c, v)
    else (
      let nt =
        Noeud (b, Vide (nord_ouest b), Vide (nord_est b), Vide (sud_ouest b), Vide (sud_est b))
      in
      let nt = insert nt oc ov in
      insert nt c v)
  | Noeud (b, q1, q2, q3, q4) ->
    let x, y = c in
    let cx, cy = centre_case b in
    (match x < cx, y < cy with
     | true, true -> Noeud (b, insert q1 c v, q2, q3, q4)
     | true, false -> Noeud (b, q1, insert q2 c v, q3, q4)
     | false, true -> Noeud (b, q1, q2, insert q3 c v, q4)
     | false, false -> Noeud (b, q1, q2, q3, insert q4 c v))

let%test _ = insert quadtree_test coord_test2 2 = Noeud (((1.,0.),(0.,1.)),Vide ((1.,0.),(0.5,0.5)),Feuille(((1.,0.5),(0.5,1.)),(0.75,0.75),1),Feuille(((0.5,0.),(0.,0.5)),(0.,0.),2),Vide ((0.5,0.5),(0.,1.)))

let clean_tree tree =
  match tree with
  | Vide _ -> tree
  | Feuille _ -> tree
  | Noeud (case, t1, t2, t3, t4) as noeud ->
    (match t1, t2, t3, t4 with
     | Vide _, Vide _, Vide _, Vide _ -> Vide case
     | Feuille (_, co, elem), Vide _, Vide _, Vide _ -> Feuille (case, co, elem)
     | Vide _, Feuille (_, co, elem), Vide _, Vide _ -> Feuille (case, co, elem)
     | Vide _, Vide _, Feuille (_, co, elem), Vide _ -> Feuille (case, co, elem)
     | Vide _, Vide _, Vide _, Feuille (_, co, elem) -> Feuille (case, co, elem)
     | _ -> noeud)

let%test _ = clean_tree quadtree_test = Feuille(((1.,0.),(0.,1.)),(0.75,0.75),1)

let rec retirer tree (x, y) =
  match tree with
  | Vide _ -> tree
  | Feuille (case, _, _) -> Vide case
  | Noeud (case, t1, t2, t3, t4) ->
    let x1, y1 = centre_case case in
    if x > x1 && y > y1 then clean_tree (Noeud (case, t1, retirer t2 (x, y), t3, t4)) else
    if x > x1 && y < y1 then clean_tree (Noeud (case, retirer t1 (x, y), t2, t3, t4)) else
    if x < x1 && y < y1 then clean_tree (Noeud (case, t1, t2, retirer t3 (x, y), t4))
    else clean_tree (Noeud (case, t1, t2 , t3, retirer t4 (x, y)))

let%test _ = retirer quadtree_test (0.75,0.75) = Vide ((1.,0.),(0.,1.))

let rec map tree f =
  match tree with
  | Vide _ -> ()
  | Feuille (_, _, elem) -> f elem
  | Noeud (_, t1, t2, t3, t4) ->
    map t1 f;
    map t2 f;
    map t3 f;
    map t4 f

let filtre_compter_retirer tree f =
  let rec aux tree count =
    match tree with
    | Vide _ -> tree, count
    | Feuille (case, _, elem) -> if f elem then tree, count else Vide case, count + 1
    | Noeud (case, t1, t2, t3, t4) ->
      let t1, ncount = aux t1 count in
      let t2, ncount = aux t2 ncount in
      let t3, ncount = aux t3 ncount in
      let t4, ncount = aux t4 ncount in
      clean_tree (Noeud (case, t1, t2, t3, t4)), ncount
  in
  aux tree 0

let%test _ = filtre_compter_retirer quadtree_test (fun x -> x>0) = (Feuille(((1.,0.),(0.,1.)),(0.75,0.75),1), 0)
let%test _ = filtre_compter_retirer quadtree_test (fun x -> x<0) = (Vide ((1.,0.),(0.,1.)), 1)
