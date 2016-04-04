def ES(){
    do
        vecinos_generados = 0
        exitosos = 0
        # Un enfriamiento
        do
            vecino = flip(mascara, random({1...n}))
            generados++
            evs_hechas++
            tasa_vecino = tasa(vecino)
            mejora = tasa_vecino - tasa(mascara)
            u = random between 0.0 and 1.0

            if (mejora > 0 || u <= e^{mejora/t_actual})
                mascara = vecino
                exitosos++

                if (mejora > 0) mejor_mascara = mascara

        while generados < max_vecinos and evs < tope_evs and exitosos < tope_exitosos
            t_actual = enfriar(t_actual)
            enfriamientos++

    while   evs_hechas < tope_evs and enfriamientos < tope_enfriam and exitosos != 0

    return mejor_mascara
}
