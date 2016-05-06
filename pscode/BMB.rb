def BMB(){
    # n es el número de variables en la selección de características
    mascaras = [ random_mask(n) from 0 to max_arranques ]
    mascaras = [ busqueda_local(m): m $\in$ mascaras ]
    tasas = [ tasa.clas(m): m $\in$ mascaras ]

    return mascaras [arg max tasas]
}
