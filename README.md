
# TSP: Traveling Salesman Problem / Problème du voyageur de commerce

C'est un problème NP-complet. Donc, si P différent de NP
(conjecture la plus probable), il n'y aura pas d'algorithme exact en
temps polynomial.

## sources

- Carré/Mansuy, Option Informatique, MPSI MP/MP*, Vuibert, 2019
    - Problème du voyageur de commerce page 254
- Tim Roughgarden
    - chap 15 “Minimum Spanning Trees”
    (Algorithms illuminated Part 3 Greedy Algorithms and Dynamic Programming
    2019, Soundlikeyourself)
- Cormen et al, INTRODUCTION À L’ALGORITHMIQUE Cours et exercices, Dunon, 2eme ed, 2002
    - 35.2 PROBLÈME DU VOYAGEUR DE COMMERCE
- Advanced Algorithmics and Graph Theory with Python (Institut Mines Télécom)
[https://courses.edx.org/courses/course-v1:IMTx+NET04x+3T2018/course/](https://courses.edx.org/courses/course-v1:IMTx+NET04x+3T2018/course/)
- Kevin Buchin [https://www.win.tue.nl/~kbuchin/teaching/2IL15/backtracking.pdf](https://www.win.tue.nl/~kbuchin/teaching/2IL15/backtracking.pdf)



## force brute (solution exacte)

On teste toutes les permutations pour obtenir tous les chemins hamiltoniens,
et on choisit le ou l'un des chemins de poids minimal.

**python**: tsp.py

**OCaml**: tsp.ml

compilation et exécution pour OCaml

```
$ ocamlc -o tsp.native tsp.ml && ./tsp.native
```
ou
```
$ ocamlopt -o tsp.native tsp.ml && ./tsp.native
```

<br>

** EN COURS : **

## backtracking (solution exacte)

## arbre couvrant de poids minimal, algorithme de Prim (approximation de la solution)

## Prim accéléré en utilisant la structure de tas   (approximation de la solution)

## programmation dynamique

## algorithmes randomisés

## état de l'art
