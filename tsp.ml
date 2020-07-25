
(*PROBLEME VOYAGEUR DE COMMERCE*)


(* g: graphe pondéré, non orienté, complet d'ordre n,
implémenté sous forme de matrice d'adjacence M
symétrique et de taille n; pour tout i, m_{ii} = 0 *)

(* cycle hamiltonien de taille n: liste formée des entiers de 0 à n-1,
et commençant par 0; ex [0; 2; 1] : cycle 0 - 2 - 1 - 0 *)



type graphe = int array array


(*----------------------------------------------------------------------------*)
(*outils*)

let rec print_liste l = match l with
  | []        -> ()
  | x :: q    -> print_int x; Printf.printf " "; print_liste q

(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(*GENERATION GRAPHE ALEATOIRE*)
(* génération aléatoire d'un graphe complet pondéré et symétrique quelconque de taille n *)
let graphe_aleatoire n =
  let g = Array.make_matrix n n 0 in
  for i = 0 to n-1 do
    for j = i + 1 to n-1 do
      g.(i).(j) <- (Random.int 50) + 1;
      g.(j).(i) <- g.(i).(j);
      g.(i).(i) <- 0
    done;
  done;
  g

(* let test = graphe_aleatoire 5 *)

(* génération aléatoire d'un graphe complet pondéré de taille n
dont les poids respectent l'inégalité triangulaire *)

let dist_eucl (i, j) (k, l) =
  sqrt( (float_of_int (k - i))**2.0 +. (float_of_int (l - j))**2.0)

(* b: valeur limite abscisse/ordonnée *)
(* cette fonction peut devenir très lente si b est grand et si n est proche de b²*)
exception Valeur_invalide

let creer_liste_points n b =
  if n >= b*b then
    raise Valeur_invalide
  else
    begin
      Random.self_init ();
      let rec aux acc k = match k with
        | k when k = n  -> acc
        | _             -> let i = Random.int b and j = Random.int b in
                           if List.mem (i, j) acc then aux acc k
                           else aux ((i,j) :: acc) (k+1)
      in
      aux [] 0
    end

let points_to_graphe_eucl l =
  let n = List.length l in
  let t = Array.of_list l in
  let m = Array.make_matrix n n max_int in
  (* diagonale *)
  for i = 0 to n - 1 do
    m.(i).(i) <- 0
  done;
  (* triangles supérieur et inférieur (matrice symétrique) *)
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let d = int_of_float (dist_eucl t.(i) t.(j)) in
      m.(i).(j) <- d;
      m.(j).(i) <- d
    done
  done;
  m

(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(*FORCE BRUTE*)
(* poids d'un cycle hamiltonien dans un graphe (Carré/Mansuy)*)
(* exemple de cycle c : [0; 1; 2; 4; 3]; DOIT COMMENCER PAR 0  *)
(* la fonction poids_ch compte l'arête fermant le cycle, ici 3 - 0 *)

let poids_ch g ch =
  let rec aux ch = match ch with
    | []          -> 0
    | [x]         -> g.(0).(x)
    | x :: y :: q -> g.(x).(y) + aux (y :: q)
  in
  aux ch

(* liste de toutes les permutations de 0..n-1 commençant par 0 (Carré/Mansuy)*)
(* permutations de 1..n *)
let rec permutations_sans_zero n =
  let rec insertion k li = match k,li with
    | 0, _      -> n :: li
    | _, []     -> [n]
    | _, x :: q -> x :: (insertion (k - 1) q)
  in
  let rec inserer_n k lli = match k, lli with
    | _, []        -> []
    | -1, li :: qq -> inserer_n (n-1) qq
    | _, li :: qq  -> (insertion k li) :: (inserer_n (k-1) lli)
  in
  match n with
    | 1 -> [[1]]
    | _ -> let lli = permutations_sans_zero (n - 1) in
           inserer_n (n - 1) lli


(*Ajout du 0 au début de toutes les permutations*)
let permutations n =
  let lli = permutations_sans_zero (n - 1) in
  let rec aux lli = match lli with
    | []       -> []
    | li :: qq -> (0 :: li) :: aux qq
  in
  aux lli

(* Force brute *)
(* génération de toutes les permutations commençant par 0 *)
(* donc deux fois trop de cycles hamiltoniens (chaque cycle apparaît deux fois
une fois par sens de parcours) *)
let tsp g =
  let permut_possibles = permutations (Array.length g) in
  let rec poids_minimal ch poids_min permut_possibles = match permut_possibles with
    | []                      -> ch
    | li :: qq when  ch = []  -> poids_minimal li (poids_ch g li) qq
    | li :: qq                 -> let poids = poids_ch g li in
                              if poids < poids_min then
                                poids_minimal li poids qq
                              else
                                poids_minimal ch poids_min qq
  in
  poids_minimal [] 0 permut_possibles

(*----------------------------------------------------------------------------*)

(* FORCE BRUTE - 2eme version *)
(* construction du stream de permutations *)
type direction = G | D

let initialiser_directions a =
  Array.map (fun x -> (x, G)) a

let echanger a i j =
  let tempo = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tempo

let peut_bouger a i = match a.(i) with
  | (x, d) when d = G ->
            if i > 0 && x > (fst a.(i - 1)) then true else false
  | (x, d) when d = D ->
            let n = Array.length a in
            if i < n - 1 && x > (fst a.(i + 1)) then true else false
  | _                 -> failwith "(peut_bouger) erreur"

let bouger a i =
  let (x, d) = a.(i) in
  if peut_bouger a i then
    match d with
      | G -> echanger a i (i - 1)
      | D -> echanger a i (i + 1)
    else
      failwith "impossible d'effectuer le déplacement"

let trouver_plus_grand_a_bouger a =
  let rec aux acc i =
    let n = Array.length a in
    if i >= n then acc
    else if not (peut_bouger a i) then aux acc (i+1)
    else
      let x, _ = a.(i) in
      match acc with
        | None    -> aux (Some i) (i+1)
        | Some j  -> let new_acc = if x < fst a.(j) then acc else Some i in
                     aux new_acc (i+1)
    in
    aux None 0

let changer_direction d = match d with
  | G -> D
  | D -> G

let changer_direction_plus_grand x a =
  Array.iteri
    (fun i (y, d) -> if y > x then a.(i) <- (y, changer_direction d))
    a

let generateur_permutations l =
  let a = Array.of_list l |> initialiser_directions in
  let r = ref (Some l) in
  let next () =
    let p = !r in
      begin
        match trouver_plus_grand_a_bouger a with
          | None    -> r := None
          | Some i  ->
              let x, d = a.(i) in
                begin
                  bouger a i ;
                  changer_direction_plus_grand x a;
                  r := Some (Array.map fst a |> Array.to_list)
                end
      end;
      p
    in
    next

let permutations_stream l =
  let gen = generateur_permutations l in
  Stream.from (fun _ -> gen ())


let tester_permutations_stream l =
  let perm_stream = permutations_stream l in
  try
    while true do
      print_liste (Stream.next perm_stream);
      print_string "\n"
    done
  with Stdlib.Stream.Failure ->
    print_string "-> fin du stream\n\n"
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(* force brute, utilisant ce stream *)
(* attention: toutes les permutations doivent commencer par 0 *)
let tsp_force_brute g =
  let n = Array.length g in
  let l = List.tl (List.init n (fun x -> x)) in (* liste [1; 2; ...; n-1]*)
  let perm_stream = permutations_stream l in
  let poids = ref max_int and ch = ref [] in
    begin
      try
        while true do
          let c = Stream.next perm_stream in
          let ch_x = 0 :: c in  (* tous les cycles commencent par 0 *)
          let poids_x = poids_ch g ch_x in
            if poids_x < !poids then
              begin
                poids := poids_x;
                ch := ch_x
              end
        done
      with
      | Stdlib.Stream.Failure -> () (* fin du Stream : sortie normale *)
      | _                     -> failwith "exception inconnue"
    end;
    !ch
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(* approximation possible quand fonction poids vérifie l'inégalité triangulaire *)
(* dérivé de l'algorithme de Prim, implémentation directe *)
(* Carré/Mansuy; Tim Roughgarden *)


(* arbre couvrant de poids minimal, algorithme de Prim *)
(* X: sommets inclus dans l'arbre couvrant *)
(* en sortie: un arbre (non pondéré) sous forme de matrice d'adjacence *)
let acm_prim g =
  let n = Array.length g in
  let m = Array.make_matrix n n false in   (* arbre couvrant (sans les poids)*)
  let x = Array.make n false in            (* sommets de l'arbre couvrant *)
  x.(0) <- true;  (* on prend le premier sommet comme racine de l'arbre *)
  for k = 1 to n - 1 do (*on rajoute un sommet à chaque tour de boucle, donc n-1 tours de boucle*)
    (* choisir parmi toutes les aretes ayant une extrémité dans X, l'autre dans V-X
    celle de poids minimal (V : ensemble de tous les sommets)  *)
    let p = ref max_int and imin = ref (-1) and jmin = ref (-1) in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if x.(i) && not x.(j) then
          begin
            if g.(i).(j) < !p then
              begin
                p := g.(i).(j);
                imin := i;
                jmin := j
              end
          end
      done;
    done;
    x.(!jmin) <- true;
    m.(!imin).(!jmin) <- true;
    m.(!jmin).(!imin) <- true
  done;
  m



(* graphe: poids vérifient l'inégalité triangulaire *)
(* let g = points_to_graphe (creer_liste_points_triang 10 15)
let m = acm_prim g *)

(* let g_test = [|[|0; 5; 7; 2|]; [|5; 0; 7; 6|]; [|7; 7; 0; 6|]; [|2; 6; 6; 0|]|]
let m = acm_prim g_test *)

(* graphe: conversion de matrice d'ajacence à liste d'adjacence *)
(* gm: graphe sous forme de matrice d'adjacence (tableau de tableaux) de booléens*)
(* gl: graphe sous forme de liste d'adjacence (tableau de listes)*)
let conversion_matrice_liste gm =
  let n = Array.length gm in
  let gl = Array.make n [] in
  for i = 0 to n-2 do
    for j = i+1 to n-1 do (* la matrice est symétrique; le triangle supérieur suffit*)
        if gm.(i).(j) then
          begin
            gl.(i) <- j :: gl.(i);
            gl.(j) <- i :: gl.(j)
          end
    done
  done;
  gl


(* acm_gl: arbre sous forme de liste d'adjacence *)
(* renvoie un cycle hamiltonien à partir de l'arbre couvrant minimal *)
let acm_to_cycle_hamiltonien acm_gl =
  let n = Array.length acm_gl in
  let ch = ref [] in
  let vus = Array.make n false in
  vus.(0) <- true;
  let rec aux li = match li with
    | [] -> List.rev !ch
    | x :: q -> ch := x :: !ch; aux (voisins acm_gl.(x) @ q)
  and voisins li = match li with
    | [] -> []
    | y :: q when vus.(y) -> voisins q
    | y :: q -> vus.(y) <- true; y :: voisins q
  in
  aux [0]
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)

(* force brute, version récursive *)
(*
https://www.win.tue.nl/~kbuchin/teaching/2IL15/backtracking.pdf:
pseudo code  Algorithm TSP BruteForce2 (A, ℓ, lengthSoFar )
modifié: poids ET chemin hamiltonien
*)


(* permuter deux elements d'un tableau,
connaissant les indices des éléments à permuter.
Permutation "en place" *)
let permuter t i j =
  let tempo = ref t.(i) in
    t.(i) <- t.(j);
    t.(j) <- !tempo

let tsp_force_brute_rec g =
  let n = Array.length g in
  let s = Array.init n (fun i -> i) in (* sommets *)

  let rec aux g s l poids_en_cours =
    (* s: 0..l : sommets visités
       s: l+1..n-1: sommets non visités *)
    match l with
    | l when l = n -1 -> (* tous les sommets ont été visités *)
          let poids = (poids_en_cours + g.(s.(l)).(s.(0))) in
          let ch = Array.to_list s in
          poids, ch
    | _               ->
          (* on commence par prendre s.(l+1) comme sommet suivant du cycle *)
          let nouveau_poids = ref (poids_en_cours + g.(s.(l)).(s.(l+1))) in
          let poids_min_rec = ref max_int and ch_rec = ref [] and poids_min = ref max_int
              and ch = ref [] in
          let (pmr, cr) = aux g s (l+1) !nouveau_poids in
          poids_min_rec := pmr;
          poids_min := pmr; (* un seul cas testé: c'est donc le minimum*)
          ch_rec := cr;
          ch := cr;
          (* puis on teste les autres sommets possibles comme sommet suivant *)
          for i = l+2 to n-1 do
            permuter s (l+1) i; (* le sommet suivant est maintenant celui qui avait l'indice i *)
            nouveau_poids := poids_en_cours + g.(s.(l)).(s.(l+1));
            let (pmr2, chr2) = aux g s (l+1) !nouveau_poids in
            poids_min_rec := pmr2;
            ch_rec := chr2;
            if poids_min_rec < poids_min then
              begin
                poids_min := !poids_min_rec;
                ch := !ch_rec
              end;
            permuter s (l+1) i (* annulation de la permutation précédente *)
          done;
          !poids_min, !ch
  in
  aux g s 0 0
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)

(* backtracking, en modifiant force brute 3eme version, version récursive *)


let tsp_backtrack g =
  let n = Array.length g in
  let s = Array.init n (fun i -> i) in (* sommets *)

  let poids_min_en_cours = ref max_int in

  let rec aux g s l poids_en_cours =
    (* s: 0..l : sommets visités
       s: l+1..n-1: sommets non visités *)
    match l with
    | l when l = n -1 -> (* tous les sommets ont été visités *)

          let poids = poids_en_cours + g.(s.(n-1)).(s.(0)) in
          poids_min_en_cours := min poids !poids_min_en_cours;
          let ch_fin = Array.to_list s in poids, ch_fin

    | _               ->
          let nouveau_poids = ref(poids_en_cours + g.(s.(l)).(s.(l+1))) in
          let ch = ref [] in
          let poids_min = ref max_int in
          if nouveau_poids < poids_min_en_cours then
            begin
              let (pmr, chr) = aux g s (l+1) !nouveau_poids in
              poids_min := pmr;
              ch := chr
            end;
          for i = l+2 to n-1 do
            permuter s (l+1) i;
            nouveau_poids := poids_en_cours + g.(s.(l)).(s.(l+1));
            if nouveau_poids >= poids_min then
              () (*pruning*)
            else
              begin
                let (pmr,chr) = aux g s (l+1) !nouveau_poids in
                if pmr < !poids_min then
                  begin
                    poids_min := pmr;
                    ch := chr
                  end;
              end;
            permuter s (l+1) i
          done;
          !poids_min, !ch
  in
  aux g s 0 0

(*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*)



let g1 = [| [|0;2;4;3;9|];
            [|2;0;3;6;7|];
            [|4;3;0;6;6|];
            [|3;6;6;0;2|];
            [|9;7;6;2;0|]  |]

let g2 = [| [|0;1;4;2|];
            [|1;0;6;3|];
            [|4;6;0;5|];
            [|2;3;5;0|]  |]


let g3 = points_to_graphe_eucl (creer_liste_points 9 1000)

let () =
  let c = tsp g3 in
  print_string "Force brute, solution exacte\n";
  print_liste c ;
  print_newline ();
  print_string "Poids du cycle: ";
  print_int (poids_ch g3 c);
  print_newline ();
  print_newline ()

let () =
  let c = tsp_force_brute g3 in
  print_string "Force brute, deuxième version, solution exacte\n";
  print_liste c ;
  print_newline ();
  print_string "Poids du cycle: ";
  print_int (poids_ch g3 c);
  print_newline ();
  print_newline ()



let () =
  let p, c = tsp_force_brute_rec g3 in
  print_string "Force brute,troisième version (récursive), solution exacte\n";
  print_liste c ;
  print_newline ();
  print_string "Poids du cycle: ";
  print_int p;
  print_newline ();
  print_newline ()

let () =
  let p, c = tsp_backtrack g3 in
  print_string "Force brute,quatrième version (récursive avec backtracking), solution exacte\n";
  print_liste c ;
  print_newline ();
  print_string "Poids du cycle: ";
  print_int p;
  print_newline ();
  print_newline ()

let () =
  let acm_gm = acm_prim g3 in
  let acm_gl = conversion_matrice_liste acm_gm in
  let ch = acm_to_cycle_hamiltonien acm_gl in
  print_string "Approximation avec l'algorithme de Prim\n";
  print_liste ch;
  Printf.printf "\npoids (prim): %d\n" (poids_ch g3 ch);
  print_newline ()
