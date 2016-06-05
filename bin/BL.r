##########################################################################
### Generador de soluciones iniciales aleatorias en selec. caracteristicas
###     Para un tamaño determinado devuelve una seleccion de 
###     caracteristicas aleatoria
##########################################################################

random.init <- function(data){ sample(0:1, ncol(data)-1, replace=TRUE) }

##########################################################################
### Funcion busqueda local del primer mejor
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar la busqueda local del
###     primer mejor
###
###
###     get.init      Es el generador de soluciones iniciales (por defecto
###                   aleatorias, pero puede pasarsele un GRASP p.e.)
##########################################################################

BL <- function(data, gen.init = random.init){
  n <- ncol(data)
  n <- n-1
  mask <- gen.init(data)
  tasa.best <- tasa.clas(data, mask)
  mejora.found <- TRUE
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
      
      m[j] <- !m[j]
      
      tasa.actual <- tasa.clas(data, m)
      n.eval <- n.eval + 1
      
      if (tasa.actual > tasa.best){
        mask <- m
        tasa.best <- tasa.actual
        mejora.found <- TRUE
        
      } else{
        if (length(non.selected) == 0){
          vecinos.left <- FALSE
        }
      }
    }
  }
  mask
}

##########################################################################
### Variante de la busqueda local del primer mejor por entornos
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar la busqueda local del
###     primer mejor. Devuelve ademas el numero de evaluaciones de la funcion
###     objetivo consumidas
###
###
###     mask          mascara a la que se desea aplicar busqueda local
###     max.entornos  numero maximo de entornos a explorar por la funcion
##########################################################################


BL.entornos <- function(data, mask, max.entornos){
  n <- ncol(data)
  n <- n-1
  tasa.mask <- tasa.clas(data, mask)
  mejora.found <- TRUE
  n.eval <- 0
  entornos <- 0
  
  while(entornos < max.entornos){
    non.selected <- seq(1,n)
    mejora.found <- FALSE
    
    while(!mejora.found && length(non.selected)>0){
      # Generamos un vecino hasta que mejoremos
      # o hasta que agotemos el vecindario
      
      m <- mask
      j <- sample(non.selected, 1)
      non.selected <- non.selected[non.selected!=j]
      
      m[j] <- !m[j]
      
      tasa.actual <- tasa.clas(data, m)
      n.eval <- n.eval + 1
      
      if (tasa.actual > tasa.mask){
        mask <- m
        tasa.mask <- tasa.actual
        mejora.found <- TRUE
      }
    }
    entornos <- entornos + 1
  }
  
  # Lista con solucion + evaluaciones consumidas
  list(mask=mask, eval = n.eval)
}

