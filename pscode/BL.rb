def BL(data){
    do{
        no_seleccionados = {1,2,...n}
        do{
            i = random(no_seleccionados)
            no_seleccionados = no_seleccionados -{i}
            evs_hechas++

            if (tasa_acierto(flip(mascara, i) > mejor_tasa){
                mascara = flip(mascara,i)
                mejor_tasa = tasa_acierto(mascara)
                mejora_encontrada = true
            }
          }while !no_seleccionados.empty and !mejora_encontrada
      }while evs_hechas <  tope_evs and mejora_encontrada

  return mascara
}
