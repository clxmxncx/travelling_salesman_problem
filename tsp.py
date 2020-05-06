
# PROBLEME VOYAGEUR DE COMMERCE

################################################################################
# Force brute
# version non récursive


import itertools
import numpy as np
import math
import timeit

# https://tqdm.github.io/ barre de progression
from tqdm import tqdm


def generer_graphe_aleatoire(n):
    g = np.floor(np.random.rand(n,n)*100)
    for i in range (n):
        for j in range (n):
            g[i, i] = 0
            g[i, j] = g[j, i]
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

def tsp_force_brute(g):
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
    pp = list(itertools.permutations(range(1, len(g))))
    meilleur_poids = math.inf
    meilleur_ch = (0,)

    for ch in tqdm(pp):
        ch = (0,) + ch
        poids = poids_ch(g,ch)
        if poids < meilleur_poids :
            meilleur_poids = poids
            meilleur_ch = ch

    return meilleur_ch, meilleur_poids


# exemple Tim R.
# un cycle hamiltonien de poids minimal: 0213 ; un autre 0132
# poids: 13
g_timR = np.matrix([ [0,1,4,2], [1,0,6,3], [4,6,0,5], [2,3,5,0]])


def demo(n):
    print(f'un graphe complet au hasard, de taille {n}:\n')
    g = generer_graphe_aleatoire(n)
    print(g)
    date_debut = timeit.default_timer()
    ch, poids = tsp_force_brute(g)
    date_fin = timeit.default_timer()
    print(f'\nresultat -> poids : {poids} et chemin: {ch}\n')
    print(f'temps d\'exécution : {date_fin - date_debut} ')
    print('\n')


if __name__ == '__main__':
    # print('une graphe dont le poids des cycles hamiltoniens minimaux est 13:')
    # print(g_timR)
    # ch, poids = tsp_force_brute(g_timR)
    # print(f'resultat -> poids : {poids} et chemin: {ch}')
    # print('\n')

    demo(10) # environ 1.5 sec sur corei5 3320M, 8go ram, ubuntu 18.04
    #demo(11) # environ 17 sec 
