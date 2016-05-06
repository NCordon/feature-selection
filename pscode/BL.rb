def BL(m){
    # Máscara inicial, por defecto aleatoria
    mascara = m
    do{
        no_seleccionados = {1,2,...n}
        do{
            i = random(no_seleccionados)
            no_seleccionados = no_seleccionados -{i}
            evs_hechas++

            if (tasa(flip(mascara, i) > mejor_tasa){
                mascara = flip(mascara,i)
                mejor_tasa = tasa(mascara)
                mejora_encontrada = true
            }
        }while !no_seleccionados.empty and !mejora_encontrada
    }while evs_hechas <  tope_evs and mejora_encontrada

  return mascara
}
