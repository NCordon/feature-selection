def BL(m){
    mascara = m
    no_seleccionados = {1,2,...n}
    do{
        i = random(no_seleccionados)
        no_seleccionados = no_seleccionados - {i}
        evs_hechas++

        if (tasa(flip(mascara, i) > mejor_tasa){
            mascara = flip(mascara,i)
            mejor_tasa = tasa(mascara)
            mejora_encontrada = true
        }
    }while !no_seleccionados.empty and !mejora_encontrado

  return (mascara, evs_hechas)
}
