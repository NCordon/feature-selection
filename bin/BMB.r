##########################################################################
### Funcion Busqueda Multiarranque
###     Lanza la Busqueda local un numero determinado de veces
###     para una solucion aleatoria generada, y se queda con la
###     mejor solucion de todas las encontradas
##########################################################################


BMB <- function(data){
  max.arranques <- BMB.num.sols.init
  
  masks <- lapply (1:max.arranques, function(x){ BL(data) })
  tasas <- sapply(masks, function(x){ tasa.clas(data, x) })
    
  j <- which.max(tasas)
  
  masks[[j]]
}