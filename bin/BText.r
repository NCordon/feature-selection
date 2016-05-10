##########################################################################
### Funcion busqueda tabu extendida
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar la tabu con memoria
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
  # Tamaño maximo de la lista tabu
  tenencia.tabu <- n*BT.coef.tenencia.tabu 
  
  # Lista tabu
  tabu.list <- c()
  # Lista de frecuencias
  frec <- rep(0,n)
  tope.reinic <- BT.ext.tope.reinic
  n.sin.mejora <- 0
  
  # Posicion a escribir de la lista tabu
  tl.pos <- 1
  n.eval <- 0
  
  # Probabilidades limite de intensificacion y diversificacion
  prob.diversif <- BT.ext.prob.diversif
  prob.intensif <- BT.ext.prob.intensif + BT.ext.prob.diversif
  
  while(n.eval < max.eval){
    # Reinicializacion
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
          # Evita que falle cuando todas las frecuencias estan a 0
          n <- max(n,1)
          
          if (u < 1 - f/n){
            x <- 1
          }
          x
        }, n = n.soluciones)
      }
      # Cambio de tamaño de la lista tabu
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
      
      m[j] <- !m[j]
      tasa.actual <- tasa.clas(data, m)
      
      if (j %in% tabu.list){
        # Si el criterio de aspiracion no se cumple
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
    # Esto asigna a la solucion actual el mejor vecino
    mask[tabu.elem] <- !mask[tabu.elem]
    
    if (tasa.mejor.vecino > tasa.best){
      mask.best <- mask
      tasa.best <- tasa.mejor.vecino
      frec <- frec + mask
      n.sin.mejora <- 0
    }
    else{
      n.sin.mejora <- n.sin.mejora + 1
    }
    
    # Introducimos en la lista tabu el elemento que ha dado lugar
    # a la mejor solucion del vecindario anterior
    tabu.list[tl.pos] <- tabu.elem
    tl.pos <- (tl.pos %% tenencia.tabu) + 1
    
    # Actualizamos el numero de evaluaciones
    n.eval <- n.eval + length(pos.vecinos)
  }
  mask.best
}
