def BMB(){
    # n el número de predictores de cada dataset
    mascaras = random_init(n)
    mascaras = [ BL(m): m in mascaras ]
    tasas = [ tasa.clas(m): m in mascaras ]

    return mascaras [arg max tasas]
}
