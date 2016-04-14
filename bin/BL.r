##########################################################################
### Generador de soluciones iniciales aleatorias en selec. caracteristicas
###     Para un tamaño determinado devuelve una seleccion de 
###     caracteristicas aleatoria
##########################################################################

random.init <- function(n){ sample(0:1, n, replace=TRUE) }


##########################################################################
### Funcion busqueda local del primer mejor
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar la busqueda local del
###     primer mejor
##########################################################################

BL <- function(data, gen.init = random.init){
  n <- ncol(data)
  n <- n-1
  mask <- gen.init(n)
  tasa.best <- tasa.clas(data, mask)
  mejora.found <- TRUE
  #max.eval <- 15000
  n.eval <- 0
  
  while(mejora.found && (n.eval < max.eval)){
    non.selected <- seq(1,n)
    vecinos.left <- TRUE
    mejora.found <- FALSE
    
    while(!mejora.found && vecinos.left && (n.eval < max.eval)){
      # Generamos un vecino hasta que mejoremos
      # o hasta que agotemos el vecindario
      
      m <- mask
      j <- sample(non.selected, 1)
      non.selected <- non.selected[non.selected!=j]
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      n.eval <- n.eval + 1
      
      if (tasa.actual > tasa.best){
        mask <- m
        tasa.best <- tasa.actual
        mejora.found <- TRUE
      }
      else{
        if (length(non.selected) == 0){
          vecinos.left <- FALSE
        }
      }
    }
  }
  mask
}


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

