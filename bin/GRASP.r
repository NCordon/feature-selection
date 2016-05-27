##########################################################################
### Funcion de generacion de soluciones iniciales GRASP
##########################################################################

random.greedy.init <- function(data){
  alpha <- GRASP.alpha
  n <- ncol(data)
  n <- n-1
  mask.best <- rep(0,n)
  non.selected <- seq(1,n)
  fin <- FALSE
  mask.fitness <- 0
  
  while(!fin){
    # lista restringida de candidatos
    masks <- lapply (non.selected, function(x){
      m <- mask.best
      m[x] <- 1
      m
    } )
    
    # Nos quedamos de las mascaras que verifican la condicion
    # del umbral, con una aleatoria
    tasas <- sapply(masks, function(x){ tasa.clas(data, x) })
    tasas.max <- max(tasas)
    tasas.min <- min(tasas)
    umbral <- alpha * (tasas.max - tasas.min)
    to.keep <- which(tasas.max - tasas <= umbral)
    masks <- masks[to.keep]
    sel <- sample(to.keep,1)
    
    # Actualizamos el mejor hasta el momento
    if (tasas [sel] > mask.fitness){
      mask.best[non.selected[sel]] <- 1
      mask.fitness <- tasas[sel]
      non.selected <- non.selected[-sel]
      
    } else{
      fin <- TRUE
    }
    if (length(non.selected)==0){
      fin <- TRUE
    }
  }
  mask.best
}


##########################################################################
### Funcion Busqueda Multiarranque GRASP
###     Lanza la Busqueda local un numero determinado de veces
###     para una solucion greedy-aleatoria generada, y se queda con la
###     mejor solucion de todas las encontradas
##########################################################################

GRASP <- function(data){
  max.arranques <- GRASP.num.sols.init
  
  # Generamos un numero determinado de soluciones aleatorias greedy y les
  # aplicamos busqueda local
  masks <- lapply (1:max.arranques, function(x){ BL(data, random.greedy.init) })
  tasas <- sapply(masks, function(x){ tasa.clas(data, x) })
  
  masks[[which.max(tasas)]]
}