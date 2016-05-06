def ILS(){
    mejor_mascara = [ random({1,0}) from 0 to n ]
    num_mutaciones = prob_mutacion * n

    while(n_eval < max.arranques){
        mascara <- BL(mascara)

        if (tasa(mascara) > tasa(mejor_mascara)){
            mejor_mascara = mascara
        }

        mutaciones = [random({1...n}) from 0 to num_mutaciones ]

        foreach m in mutaciones{
            mascara = flip(mascara, m)
        }
    }
    return mascara
}
