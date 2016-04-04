def BT (data){
    def criterio_asp(mascara){
        return tasa_acierto(mascara) > tasa_acierto(mejor_mascara)
    }

    while evs_hechas < tope_evs {
        vecinos_generados = sample({1...n}, size=30)

        foreach mov in vecinos_generado{
            if mov is tabu{
                if criterio_asp(flip(mascara,mov)){

                }
            }
        }

      tasa.actual
    })

    j <- which.max(evs)
    tasa.mejor.vecino <- evs[j]
    tabu.elem <- pos.vecinos[j]
    # Esto asigna a la solución actual el mejor vecino
    mask[tabu.elem] <- (mask[tabu.elem]+1)%%2

    if (tasa.mejor.vecino > tasa.best){
      mask.best <- mask
      tasa.best <- tasa.mejor.vecino
    }

    # Introducimos en la lista tabú el elemento que ha dado lugar
    # a la mejor solución del vecindario anterior
    tabu.list[tl.pos] <- tabu.elem
    tl.pos <- (tl.pos %% max.tabu) + 1

    # Actualizamos el número de evaluaciones
    n.eval <- n.eval + length(pos.vecinos)
  }
    return mejor_mascara
}
