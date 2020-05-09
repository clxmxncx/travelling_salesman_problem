
# PROBLEME VOYAGEUR DE COMMERCE

# voir README.md

################################################################################

import itertools
import numpy as np
import pandas as pd
import math
import timeit

# https://tqdm.github.io/ barre de progression
from tqdm import tqdm


###############################################################################
def generer_graphe_aleatoire(n, valeur_max=100):
    g = np.ceil(np.random.rand(n,n)*valeur_max)
    # diagonale et symétrie
    for i in range(n):
        g[i,i] = 0
        for j in range(i+1, n):
            if i != j :
                g[j,i] = g[i,j]
    return g

def poids_ch(g, ch):
    '''
    g: graphe
    ch: chemin hamiltonien la forme (0, 4, 1, 3, 2);
    renvoie le poids de ce chemin hamiltonien , y compris de le poids
    de l'arête bouclant le cycle, sur l'ex: 2 - 0
    '''
    poids = 0
    n = len(ch)
    for i in range(n-1):
        poids += g[ch[i], ch[i+1]]
    poids += g[ch[0],ch[n-1]]
    return poids
###############################################################################


###############################################################################
# Force brute,
# version non récursive
def tsp_force_brute(g, progress_bar=False):
    '''
    entrée:
    g: graphe complet non orienté sous forme de matrice d'adjacence symétrique
    sortie:
    un cycle hamiltonien de poids minimal, commençant par le sommet 0
    remarque:
    génération de toutes les permutations commençant par 0
    donc deux fois trop de cycles hamiltoniens (chaque cycle apparaît deux fois,
    une fois par sens de parcours)
    '''
    pp = itertools.permutations(range(1, len(g)))
    poids_min = math.inf
    meilleur_ch = (0,)
    if progress_bar:
        pp = tqdm(pp)
    for ch in pp:
        ch = (0,) + ch # ajout du sommet initial au chemin
        poids = poids_ch(g, ch)
        if poids < poids_min:
            poids_min = poids
            meilleur_ch = ch
    return poids_min, meilleur_ch
###############################################################################


###############################################################################
#Force brute
#version récursive

def permuter(A, i, j):
    temp = A[i]
    A[i] = A[j]
    A[j] = temp

# https://www.win.tue.nl/~kbuchin/teaching/2IL15/backtracking.pdf:
# pseudo code  Algorithm TSP BruteForce2 (A, ℓ, lengthSoFar )
# modifié: poids ET chemin hamiltonien
def tsp_force_brute_aux(g, A, l, poids_en_cours):
    '''
    A: 0..l : sommets visités
    A: l+1..n-1: sommets non visités
    '''
    n = len(A)
    if l == n-1:
        poids_min = poids_en_cours + g[A[n-1], A[0]]  # on boucle le cycle
        ch = [] + A
    else:
        poids_min = math.inf
        ch = None
        for i in range(l+1, n):
            permuter(A, l+1, i) # selectionner A[i] comme sommet suivant
            nouveau_poids = poids_en_cours + g[A[l], A[l+1]]
            poids_min_rec, ch_rec = tsp_force_brute_aux(g, A, l+1, nouveau_poids)
            if poids_min_rec < poids_min:
                poids_min = poids_min_rec
                ch = ch_rec
            permuter(A, l+1, i) # annuler la selection
    return poids_min, ch

def tsp_force_brute_rec(g, progress_bar=False):
    n = len(g)
    A = list(range(n))
    poids, ch = tsp_force_brute_aux(g, A, 0, 0)
    return poids, ch
################################################################################


################################################################################
# backtracking
# https://www.win.tue.nl/~kbuchin/teaching/2IL15/backtracking.pdf

# https://www.win.tue.nl/~kbuchin/teaching/2IL15/backtracking.pdf modifié
# poids et chemin hamiltonien
def tsp_backtrack_aux(g, A, l, lengthSoFar, minCost):
    '''
    entrée:
    g: graphe complet non orienté sous forme de matrice d'adjacence symétrique
    A: 0..l : sommets visités
    A: l+1..n-1: sommets non visités
    sortie:
    un cycle hamiltonien de poids minimal, commençant par le sommet 0
    remarque:
    '''
    n = len(A)
    if l == n-1:
        minCost = min(minCost, lengthSoFar + g[A[n-1], A[0]])
        ch = [] + A
    else:
        ch = None
        for i in range(l+1, n):
            permuter(A, l+1, i) # selectionner A[i] comme sommet suivant
            newLength = lengthSoFar + g[A[l], A[l+1]]
            if newLength >= minCost: # la solution en cours ne peut être meilleure
                pass  # pruning, élagage: pas d'appel récursif
            else:
                cost_rec, ch_rec = tsp_backtrack_aux(g, A, l+1, newLength, minCost)
                if minCost > cost_rec:
                    minCost = cost_rec
                    ch = ch_rec
            permuter(A, l+1, i) # annuler la selection
    return minCost, ch

def tsp_backtrack(g, progress_bar=False):
    n = len(g)
    A = list(range(n))
    poids, ch = tsp_backtrack_aux(g, A, 0, 0, math.inf)
    return poids, ch
###############################################################################


###############################################################################
# DÉMOS, TESTS, MESURE DE TEMPS D'EXÉCUTION

# exemple du cours de Tim Roughgarden
# un cycle hamiltonien de poids minimal: 0213 ; un autre 0132
# poids: 13
g_timR = np.matrix([ [0,1,4,2], [1,0,6,3], [4,6,0,5], [2,3,5,0]])

methodes = {
    'force_brute': tsp_force_brute,
    'force_brute_rec': tsp_force_brute_rec,
    'backtrack': tsp_backtrack
}

# démo générique
def demo(g, methode='force_brute', progress_bar=False):
    print(f'\n({methode}) un graphe complet de taille {len(g)}:\n')
    print(g)
    print()
    debut = timeit.default_timer()
    poids, ch = methodes[methode](g, progress_bar)
    fin = timeit.default_timer()
    print(f"-> poids : {poids}; "
          f"cycle: {'-'.join(str(x) for x in ch)} "
          f'(en {(fin - debut):.3f} seconde(s))\n')

def comparer_resultats(n, max_iterations, methode1, methode2):
    '''
    n : taille des graphes aléatoires générés
    max_iterations: nombre maximum de comparaison effectuées
    compare les résultats, renvoie le premier graphe pour lequel
    les résultats différent;
    si tous les résultats sont identiques, renvoie None
    '''
    if methode1 not in methodes or methode2 not in methodes:
        print("méthode non disponible")
        import sys; sys.exit()

    it = 0
    poids1 = poids2 = 0
    while it < max_iterations and poids1 == poids2:
        g = generer_graphe_aleatoire(n)
        print()
        print(f'itération: {it}')
        print(g)
        poids1, _ = methodes[methode1](g)
        print(f'({methode1}), poids: {poids1}')
        n = len(g)
        A = list(range(n))
        poids2, _ = methodes[methode2](g)
        print(f'({methode2}), poids: {poids2}')
        it = it + 1

    if it == max_iterations:
        print(f'\nTous les résultats sont identiques ({max_iterations} graphes testés)\n')
        return None
    else:
        print('\n-> Résultats différents.\n')
        return g

def stats_sur_backtrack(n, max_iterations):
    temps_exec = []
    for i in range(max_iterations):
        g = generer_graphe_aleatoire(n)
        debut = timeit.default_timer()
        poids, _ =  tsp_backtrack(g)
        fin = timeit.default_timer()
        print(f'{i}: poids: {poids}; \t{(fin - debut):.7f} seconde(s)')
        temps_exec.append(fin - debut)

    s = pd.Series(temps_exec)
    description = s.describe()
    print('\nstatistiques:\n')
    print(description)

    return description
################################################################################


################################################################################
def menu():
    choice = input("""
                      A: démo force_brute
                      B: démo force_brute_rec
                      C: démo backtrack
                      D: stats backtrack
                      Q: quitter

                      Entrez votre choix: """)

    if choice == "A" or choice =="a":
        n = 10
        g = generer_graphe_aleatoire(n)
        demo(g, methode='force_brute', progress_bar=True)
    elif choice == "B" or choice =="b":
        n = 10
        g = generer_graphe_aleatoire(n)
        demo(g, methode='force_brute_rec')
    elif choice == "C" or choice =="c":
        n = 10
        g = generer_graphe_aleatoire(n)
        demo(g, methode='backtrack')
    elif choice=="D" or choice=="d":
        n = 10
        max_iterations = 100
        stats_sur_backtrack(n, max_iterations)
    elif choice=="Q" or choice=="q":
        import sys; sys.exit()
    else:
        print("\nA,B,C, ou D sont les seuls choix")
        menu()
################################################################################


################################################################################
if __name__ == '__main__':
    menu()
