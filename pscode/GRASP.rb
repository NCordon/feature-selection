def GRASP(){
    mascaras = [ random_greedy(n) from 1 to max_arranques ]
    mascaras = [ BL(m): m $\in$ mascaras ]
    tasas = [ tasa.clas(m): m $\in$ mascaras ]

    return mascaras [arg max tasas]
}
