def ILS(){
    # n el número de predictores del dataset
    mascara = [ random({1,0}) from 0 to n ]
    mejor_mascara = mascara
    num_mutaciones = prob_mutacion * n

    while(n_eval < max.arranques){
        mascara <- BL(mascara)

        if (tasa(mascara) > tasa(mejor_mascara)){
            mejor_mascara = mascara
        }

        mutaciones = [random({1...n}) from 0 to num_mutaciones ]

        foreach m in mutaciones{
            mascara = flip(mejor_mascara, m)
        }
    }
    return mejor_mascara
}
