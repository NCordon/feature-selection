##########################################################################
### Función búsqueda tabú básica
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de características que se obtienen de aplicar la tabú con memoria
###     a corto plazo
##########################################################################

BT <- function(data){
  n <- ncol(data)
  n <- n-1
  mask <- sample(0:1, n, replace=TRUE)
  mask.best <- mask
  tasa.best <- tasa.clas(data, mask.best)
  n <- length(mask)
  
  max.vecinos <- BT.max.vecinos
  # Tamaño máximo de la lista tabú
  max.tabu <- n*BT.coef.max.tabu
  
  # Lista tabú
  tabu.list <- c()
  
  # Posición a escribir de la lista tabú
  tl.pos <- 1
  n.eval <- 0
  
  while(n.eval < max.eval){
    tasa.mejor.vecino <- 0
    pos.vecinos <- sample(1:n, min(c(max.vecinos, max.eval-n.eval)))
    
    evs <- sapply(pos.vecinos, function(j){
      m <- mask
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      
      if (j %in% tabu.list){
        # Si el criterio de aspiración no se cumple
        #   Asignamos un valor basura a la tasa para
        #   que no sea escogida como la mejor
        if (tasa.actual <= tasa.best){
          tasa.actual <- 0
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
  mask.best
}
