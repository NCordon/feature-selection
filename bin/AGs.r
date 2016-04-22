##########################################################################
### Generador de soluciones iniciales aleatorias en selec. caracteristicas
###     Para un tamaño determinado devuelve una seleccion de 
###     caracteristicas aleatoria
##########################################################################

random.init <- function(data){ sample(0:1, ncol(data)-1, replace=TRUE) }


##########################################################################
### Funcion AGG (algoritmo genetico generacional)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar el algoritmo genetico
###     generacional
###
##########################################################################

make.OX.cruce <- function(mum, dad){
  n <- length(mum)
  cortes <- sample(1:n,2)
  mum.gen <- seq(cortes[1],cortes[2])
  
  son <- dad$mask
  son[mum.gen] <- mum$mask[mum.gen]
  list(mask = son, fitness=tasa.clas(son))
}

AGG <- function(data){
  n <- ncol(data)
  n <- n-1
  n.eval <- 0
  n.crom <- AGG.n.crom
  prob.cruce <- AGG.prob.cruce
  n.cruces <- ceiling(n.crom*prob.cruce)

  population <- lapply(n.crom, function(i){
    mask <- gen.init(data)
    mask.tasa <- tasa.clas(mask)
    list(mask = mask, fitness = mask.tasa)
  })
  
  while(n.eval < max.eval){
    mother <- sample(1:n.crom, n.crom)
    father <- sample(1:n.crom, n.crom)
    parents <- Map(c,mother,father)     
    
    new.population <- lapply(parents, function(p){
      torneo <- population[p]  
      winner <- which.max(sapply(torneo, function(sol){ sol$fitness }))
      torneo[[winner]]
    })
    
    
    cruces <- lapply(1:n.cruces, function(i){
      make.OX.cruce(new.population[[i]], new.population[[i%%n.cruces + 1]]) 
    })
    
    new.population[1:n.cruces] <- cruces
  }
  
  # Devolver algo
}