def GRASP(){
    mascaras = [ random_greedy() from 1 to max_arranques ]
    mascaras = [ busqueda_local(m): m $\in$ mascaras ]
    tasas = [ tasa.clas(m): m $\in$ mascaras ]

    return mascaras [arg max tasas]
}
