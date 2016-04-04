def BText(){
    while(evs_hechas < tope_evs)
        # Reinicialización
        if iteraciones_sin_mejora = 10
            u = aleatorio entre 0.0 y 1.0
            n = sum(frecuencias)

            mascara = if u < prob_diversificacion then mascara aleatoria
                      elif u < (prob_div + prob_intensificacion) then mejor_mascara
                      else {v: v_i = 1 if aleatorio(0,1)< 1 - frecuencias_i/n else 0}

            tenencia_tabu = if aleatorio(0,1) < 0.5 then tenencia_tabu*1.5
                            else tenencia_tabu*0.5

            reajusta ind_tabu
            iteraciones_sin_mejora = 0

        vecinos_generados = sample({1...n}, size=30)

        foreach mov in vecinos_generado
            if mov is not tabu or criterio_asp(flip(mascara,mov))
                if tasa_acierto(mascara) > tasa_acierto(mejor_vecino)
                    mejor_vecino = mascara
                    tabu = mov

        mascara = mejor_vecino

        if tasa_acierto(mascara) > tasa_acierto(mejor_mascara)
            mejor_mascara = mascara
        else iteraciones_sin_mejora++

        tabu_list[ind_tabu] = tabu
        ind_tabu = ind_tabu % tenencia_tabu
        evs_hechas = evs_hechas + 30
    return mejor_mascara
}
