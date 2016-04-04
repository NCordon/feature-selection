def ES (data){
    do{
        # Empezar nuevo vecindario
        vecinos_generados = 0
        exitosos = 0
        # Un enfriamiento
        do{
            vecino = flip(mascara, random({1...n}))
            vecinos_generados++
            evaluaciones_hechas++
            tasa_vecino = tasa_acierto(vecino)
            mejora = tasa_vecino - tasa_acierto(mascara)
            u = random between 0.0 and 1.0

            if (mejora > 0 || u <= e^{mejora/t_actual}){
                mascara = vecino
                exitosos++

                if (mejora > 0){
                  mejor_mascara = mascara
                }
            }
        }while   vecinos_generados < tope_vecinos & evs_hechas < tope_evs
               & exitosos < tope_exitosos

            t_actual = enfriar(t_actual)
            enfriamientos_hechos++

    }while    evs_hechas < tope_evs & enfriamientos_hechos < tope_enfriamientos
            & exitosos not 0

    return mejor_mascara
}
