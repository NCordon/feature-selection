def BMB(){
    mascaras = [ random_mask(n) from 0 to max_arranques ]
    mascaras = [ BL(m): m $\in$ mascaras ]
    tasas = [ tasa.clas(m): m $\in$ mascaras ]

    return mascaras [arg max tasas]
}
