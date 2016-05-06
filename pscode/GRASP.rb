def GRASP(){
    # n es el número de variables en la selección de características
    mascaras = [ random_greedy(n) from 1 to max_arranques ]
    mascaras = [ busqueda_local(m): m $\in$ mascaras ]
    tasas = [ tasa.clas(m): m $\in$ mascaras ]

    return mascaras [arg max tasas]
}
