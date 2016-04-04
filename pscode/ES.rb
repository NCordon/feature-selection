def ES (data){
    do{
        # Empezar nuevo vecindario
        vecinos_generados = 0
        exitosos = 0

        # Un enfriamiento
        do{
            i = random({1...n})
            vecino = flip(mascara, i)
            vecinos_generados++
            evaluaciones_hechas++

            tasa_vecino = tasa_acierto(vecino)
            mejora = tasa_vecino - tasa_acierto(mascara)
            u = random between 0.0 and 1.0

            if (mejora > 0 || u <= e^{delta/t_actual})){
                mascara = vecino
                exitosos++

                if (mejora > 0){
                  mejor_mascara = mascara
                }

            }
        }while   vecinos_generados < tope_vecinos
               & evaluaciones_hechas < tope_evs
               & exitosos < tope_exitosos

            t_actual = enfriar(t_actual)
            enfriamientos_hechos++

    }while    evaluaciones_hechas < tope_evs
            & enfriamientos_hechos < tope_enfriamientos
            & exitosos not 0

    return mejor_mascara
}
