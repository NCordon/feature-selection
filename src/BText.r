##########################################################################
### Función búsqueda tabú extendida
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de características que se obtienen de aplicar la tabú con memoria
###     a corto y largo plazo
##########################################################################

BT.ext <- function(data){
  n <- ncol(data)
  n <- n-1
  mask <- sample(0:1, n, replace=TRUE)
  mask.best <- mask
  tasa.best <- tasa.clas(data, mask.best)
  n <- length(mask)
  
  max.vecinos <- BT.ext.max.vecinos
  # Tamaño máximo de la lista tabú
  tenencia.tabu <- n*BT.coef.tenencia.tabu 
  
  # Lista tabú
  tabu.list <- c()
  # Lista de frecuencias
  frec <- rep(0,n*BT.ext.coef.frec)
  tope.reinic <- BT.ext.tope.reinic
  n.sin.mejora <- 0
  
  # Posición a escribir de la lista tabú
  tl.pos <- 1
  n.eval <- 0
  
  # Probabilidades límite de intensificación y diversificación
  prob.diversif <- BT.ext.prob.diversif
  prob.intensif <- BT.ext.prob.intensif + BT.ext.prob.diversif
  
  while(n.eval < max.eval){
    # Reinicialización
    if (n.sin.mejora == tope.reinic){
      u <- runif(1, 0.0, 1.0)
      
      if (u < prob.diversif){
        mask <- sample(0:1, n, replace=TRUE)
      }
      else if (u < prob.intensif){
        mask <- mask.best
      }
      else{
        n.soluciones <- sum(frec)
        
        mask <- sapply(frec, function(f,n){
          u <- runif(1, 0.0, 1.0)
          x <- 0
          # Evita que falle cuando todas las frecuencias están a 0
          n <- max(n,1)
          
          if (u < 1 - f/n){
            x <- 1
          }
          x
        }, n = n.soluciones)
      }
      # Cambio de tamaño de la lista tabú
      u <- runif(1, 0.0, 1.0)
      if (u < 0.5){
        # Aumenta de tamaño en un 50%
        tenencia.tabu <- ceiling(tenencia.tabu*1.5)
      }
      else{
        # Disminuye en un 50%
        tenencia.tabu <- ceiling(tenencia.tabu*0.5)
      }
      
      tl.pos <- min(tl.pos, tenencia.tabu)
      n.sin.mejora <- 0
    }
    
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
      frec <- frec + mask
      n.sin.mejora <- 0
    }
    else{
      n.sin.mejora <- n.sin.mejora + 1
    }
    
    # Introducimos en la lista tabú el elemento que ha dado lugar
    # a la mejor solución del vecindario anterior
    tabu.list[tl.pos] <- tabu.elem
    tl.pos <- (tl.pos %% tenencia.tabu) + 1
    
    # Actualizamos el número de evaluaciones
    n.eval <- n.eval + length(pos.vecinos)
  }
  mask.best
}
