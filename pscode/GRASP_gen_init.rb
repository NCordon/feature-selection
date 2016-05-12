def random_greedy(n){
    non_selected = {1,...n}
    mejor_mascara = {0,0...0}
    mejora = true

    do{
        mascaras = {}
        tasas = {}

        foreach j in non_selected{
            m = flip(mejor_mascara,j)
            mascaras.add(m)
            tasas.add(tasa(m))
        }

        umbral = alpha * (max(tasas) - min(tasas))
        mascaras.delete (which(max(tasas) - tasas > umbral))
        m = mascaras[ random() ]

        if (tasa(m) > tasa(mejor_mascara)){
            non_selected.delete(j : flip(mejor_mascara,j)=m)
            mejor_mascara = m
        }
        else
            mejora=false
    }while !non_selected.empty and mejora

    return mejor_mascara
}
