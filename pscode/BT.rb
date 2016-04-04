def BT (data){
    ind_tabu = 0

    while evs_hechas < tope_evs {
        vecinos_generados = sample({1...n}, size=30)

        foreach mov in vecinos_generado{
            if mov is not tabu or criterio_asp(flip(mascara,mov)){
                if tasa_acierto(mascara) > tasa_acierto(mejor_vecino){
                    mejor_vecino = mascara
                    tabu = mov
                }
            }
        }
        if tasa_acierto(mejor_vecino) > tasa_acierto(mejor_mascara)
            mejor_mascara = mejor_vecino

        tabu_list[ind_tabu] = tabu
        ind_tabu = ind_tabu % tenencia_tabu
        evs_hechas = evs_hechas + 30
    }
    return mejor_mascara
}
