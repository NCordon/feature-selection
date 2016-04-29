def ILS(){
    # n es el número de variables en la selección de características
    mejor_mascara = [ random({1,0}) from i=0 to n ]
    num_mutaciones = prob_mutacion * n

    while(n_eval < max.arranques){
        mascara <- busqueda_local(mascara)

        if (tasa(mascara) > tasa(mejor_mascara)){
            mejor_mascara = mascara
        }

        mutaciones = [random({1...n}) from i=0 to num_mutaciones ]

        foreach m in mutaciones{
            mascara = flip(mascara, m)
        }
    }
    return mascara
}
