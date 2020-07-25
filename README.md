
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

On teste toutes le permutations pour obtenir tous les chemins hamiltoniens,
et on choisit le ou l'un des chemins de poids minimal.


**OCaml**: tsp.ml

compilation et exécution

```
$ ocamlc -o fichier.native fichier.ml && ./fichier.native
```
ou
```
$ ocamlopt -o fichier.native fichier.ml && ./fichier.native
```

Deux versions:

- la première, fonction `tsp`, utilise une liste complète des permutations
générée à l'avance par la fonction `permutations` (solution du livre
Option Informatique, MPSI MP/MP*). La taille de la liste évolue donc comme
n!
Sur un ordinateur corei5 3320M, 8go RAM, ubuntu 18.04 on a

```
utop # permutations 10;;
Stack overflow during evaluation (looping recursion?).
```

- deuxième version, fonction `tsp_force_brute`: les permutations sont générées
l'une après l'autre suivant l'algorithme de Steinhauss-Johnson-Trotter,
et ce générateur est enveloppé par un Stream OCaml.

Sources:
- [http://typeocaml.com/2015/05/05/permutation/](http://typeocaml.com/2015/05/05/permutation/)
- [https://www.topcoder.com/generating-permutations/](https://www.topcoder.com/generating-permutations/)
- [https://fr.qwe.wiki/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm](https://fr.qwe.wiki/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm)


exemple, sur un graphe aléatoire de taille n = 12

```
➜ ocamlopt -o tsp.native unix.cmxa tsp.ml && ./tsp.native

graphe de taille : 12

0	5	20	2	24	13	15	28	17	34	43	20
5	0	22	41	7	5	39	34	43	39	6	34
20	22	0	44	9	20	18	40	33	11	5	29
2	41	44	0	1	31	12	6	45	32	27	41
24	7	9	1	0	6	2	5	4	27	20	1
13	5	20	31	6	0	30	41	2	27	15	32
15	39	18	12	2	30	0	14	11	24	12	12
28	34	40	6	5	41	14	0	8	16	36	14
17	43	33	45	4	2	11	8	0	45	5	33
34	39	11	32	27	27	24	16	45	0	23	43
43	6	5	27	20	15	12	36	5	23	0	18
20	34	29	41	1	32	12	14	33	43	18	0


0 - 1 - 5 - 8 - 10 - 2 - 9 - 7 - 6 - 11 - 4 - 3 -
poids: 79
temps d'exécution: 27.63663

```


**python**: tsp.py

pour avoir un itérateur sur les permutations, on utilise ici dans
`tsp_force_brute` une fonction de la bibliothèque standard

```python
pp = itertools.permutations(range(1, len(g)))
```

Fonctionne aussi sans stream avec la fonction récursive `tsp_force_brute_rec`

Pour n = 12, temps d'exécution : 300 secs
Pour n = 13 :
```

(force_brute) un graphe complet de taille 13:
[[ 0. 76. 48. 42. 92. 84. 26. 52. 59.  4. 32. 86. 85.]
 [76.  0. 15. 60. 22. 86. 40. 29. 42.  1. 60.  7. 89.]
 [48. 15.  0. 31. 15. 43. 34. 96. 86. 83. 22. 60. 58.]
 [42. 60. 31.  0. 86. 34. 53. 52. 35. 76. 46. 53. 87.]
 [92. 22. 15. 86.  0. 93. 26. 67. 86. 13. 78. 35. 40.]
 [84. 86. 43. 34. 93.  0.  4. 79.  2. 19. 11. 21. 22.]
 [26. 40. 34. 53. 26.  4.  0. 45. 73.  8. 15. 21. 93.]
 [52. 29. 96. 52. 67. 79. 45.  0. 23.  4. 40. 52. 41.]
 [59. 42. 86. 35. 86.  2. 73. 23.  0. 71. 52. 19. 65.]
 [ 4.  1. 83. 76. 13. 19.  8.  4. 71.  0. 95. 90. 97.]
 [32. 60. 22. 46. 78. 11. 15. 40. 52. 95.  0. 17. 16.]
 [86.  7. 60. 53. 35. 21. 21. 52. 19. 90. 17.  0.  3.]
 [85. 89. 58. 87. 40. 22. 93. 41. 65. 97. 16.  3.  0.]]
-> poids : 188.0; cycle: 0-3-2-4-1-11-12-10-6-5-8-7-9 (en 2563.222 seconde(s))

```
<br>

## backtracking (solution exacte)

Semblable à la force brute (on teste toutes les solutions). Différence :
lorsqu'on teste un nouveau cycle hamiltonien, on s'arrête dès que son
poids dépasse le poids du meilleur cycle déjà calculé.


statistiques sur 100 graphes aléatoires de taille n = 12:
(fonction stats_sur_backtrack)

```
count    100.000000
mean       1.082935
std        0.935841
min        0.130260
25%        0.453884
50%        0.762216
75%        1.400667
max        5.067200
```



## arbre couvrant de poids minimal, algorithme de Prim (approximation de la solution)

Utilisable si la fonction poids vérifie l'inégalité triangulaire.

**OCaml :** tsp.ml
