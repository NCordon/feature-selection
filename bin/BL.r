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
### Funcion Busqueda Multiarranque Basica
###     Lanza la Busqueda local un numero determinado de veces
###     para una solucion aleatoria generada, y se queda con la
###     mejor solucion de todas las encontradas
##########################################################################


BMB <- function(data){
  max.arranques <- BMB.num.sols.init
  
  masks <- lapply (1:max.arranques, function(x){ BL(data) })
  tasas <- sapply(masks, function(x){ tasa.clas(data, x) })
  
  masks[[which.max(tasas)]]
}


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
    masks <- lapply (non.selected, function(x){
      m <- mask.best
      m[x] <- 1
      m
    } )
    
    tasas <- sapply(masks, function(x){ tasa.clas(data, x) })
    tasas.max <- max(tasas)
    tasas.min <- min(tasas)
    umbral <- alpha * (tasas.max - tasas.min)
    
    cur.selection <- which(tasas.max - tasas <= umbral)
    masks <- masks[cur.selection]
    
    sel <- sample(cur.selection,1)
    
    if (tasas [sel] > mask.fitness){
      mask.best[non.selected[sel]] <- 1
      mask.fitness <- tasas[sel]
      non.selected <- non.selected[-sel]
    }
    else{
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
  
  # Generamos un numero determinado de soluciones aleatorias y les aplicamos
  # busqueda local
  masks <- lapply (1:max.arranques, function(x){ BL(data, random.greedy.init) })
  tasas <- sapply(masks, function(x){ tasa.clas(data, x) })
  
  masks[[which.max(tasas)]]
}


##########################################################################
### Funcion Busqueda Local Reiterada (ILS)
###     Lanza la Busqueda local un numero determinado de veces
###     para una solucion aleatoria generada la primera vez, y las 
###     sucesivas veces sobre una solucion mutada
##########################################################################


ILS <- function(data){
  n <- ncol(data)
  n <- n-1
  n.a.mutar <- ILS.coef.mutacion*n
  max.arranques <- ILS.num.sols.init
  
  mask <- random.init(data)
  mask.best <- mask
  tasa.best <- tasa.clas(data, mask.best)
  
  for(n.eval in 1:max.arranques){
    # Aplicamos busqueda local sobre la solucion
    mask <- BL(data, function(x){ mask })
    tasa.mask <- tasa.clas(data, mask)
    
    if (tasa.mask > tasa.best){
      mask.best <- mask
      tasa.best <- tasa.mask
    }
    
    # Mutacion
    a.mutar <- sample(1:n, n.a.mutar)
    mask <- mask.best
    mask[a.mutar] <- (mask[a.mutar]+1) %% 2
  }
  
  mask.best
}