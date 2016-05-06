def SFS(){
    non_selected = {1,...n}
    mascara = {0,0...0}
    mejora = true

    do{
        j = max arg{tasa(flip(mascara,x)): x$\in$ non_selected}

        if (tasa(flip(mascara,j)) > tasa(mascara)){
            non_selected = non_selected - {j}
            mascara [j] = 1
        }
        else
            mejora = false
    }while mejora

  return mascara
}
