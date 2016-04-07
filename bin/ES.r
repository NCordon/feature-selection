##########################################################################
### Funcion enfriamiento simulado
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar enfriamiento simulado
###
##########################################################################

ES <- function(data){
  n <- ncol(data)
  n <- n-1
  mask <- sample(0:1, n, replace=TRUE)
  mask.best <- mask
  tasa.best <- tasa.clas(data, mask.best)
  
  # Parametros del enfriamiento simulado
  #max.eval <- 15000
  max.vecinos <- ES.coef.max.vecinos*n
  max.exitos <- ES.coef.max.exitos*max.vecinos
  # Nota: los mejores valores probados para estos parametros han sido 0.05 y 0.05
  
  
  t.actual <- ES.mu*tasa.best/-log(ES.phi, base=exp(1))
  t.final <- 1e-3
  
  # Comprobamos que la temperatura final sea menor que la inicial
  # y la ajustamos en caso contrario
  while (t.final >= t.actual){
    t.final <- t.final * 1e-3
  }
  
  beta <- (t.actual - t.final)/((max.eval/max.vecinos)*t.actual*t.final)
  
  n.eval <- 0
  n.vecinos <- 0
  n.exitos <- 1
  
  # Nueva iteracion
  # print ("Nueva iteracion")
  
  while(n.eval < max.eval & n.exitos>0 & t.actual > t.final){
    n.vecinos <- 0
    n.exitos <- 0
    
    # Un enfriamiento
    while(n.vecinos < max.vecinos
          & n.exitos < max.exitos
          & n.eval < max.eval){
      
      m <- mask
      
      # Generamos un vecino
      j <- sample(1:n,1)
      m[j] <- (m[j]+1)%%2
      
      tasa.actual <- tasa.clas(data, m)
      delta <- tasa.actual - tasa.best
      u <- runif(1, 0.0, 1.0)
      
      if (delta > 0 || u <= exp(delta/t.actual)){
        mask <- m
        tasa.best <- tasa.actual
        n.exitos <- n.exitos + 1
        
        if (delta > 0){
          mask.best <- m
        }
        
      }
      
      n.vecinos <- n.vecinos + 1
      n.eval <- n.eval + 1
    }
    t.actual <- t.actual/(1 + beta*t.actual)
    # Depuracion
    #cat("\n Temperatura actual: ", t.actual)
    #cat("\n Numero de exitos: ", n.exitos)
    #cat("\n Numero de vecinos generados ", n.vecinos)
  }
  mask.best
}
