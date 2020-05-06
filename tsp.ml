
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
let creer_liste_points n b =
  Random.self_init ();
  let rec aux acc k = match k with
    | k when k = n  -> acc
    | _             -> let i = Random.int b and j = Random.int b in
                       if List.mem (i, j) acc then aux acc k
                       else aux ((i,j) :: acc) (k+1)
  in
  aux [] 0

let points_to_graphe l =
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


let g3 = points_to_graphe (creer_liste_points 9 1000)

let () =
  let c = tsp g3 in
  print_string "Force brute, solution exacte\n";
  print_liste c ;
  print_newline ();
  print_string "Poids du cycle: ";
  print_int (poids_ch g3 c);
  print_newline ();
  print_newline ();
