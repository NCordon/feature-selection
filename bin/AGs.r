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

AGG <- function(data){
  n <- ncol(data)
  n <- n-1
  n.eval <- 0
  n.crom <- AGG.n.crom
  prob.cruce <- AGG.prob.cruce
  prob.mutation <- AGG.prob.mutation
  n.cruces <- ceiling(n.crom*prob.cruce)
  n.mutations <- ceiling(n*prob.mutation)

  # El mejor el ultimo. El peor el primero
  sorted <- function(population){
    order(sapply(population, function(x){ x$fitness }))
  }  
  
  make.crossover.OX <- function(mum, dad){
    n <- length(mum)
    cortes <- sample(1:n,2)
    mum.cromosomas <- seq(cortes[1],cortes[2])
    
    son <- dad$mask
    son[mum.cromosomas] <- mum$mask[mum.cromosomas]
    list(mask = son, fitness=tasa.clas(son))
  }  
  
  make.selection <- function(parents){
    lapply(parents, function(p){
      torneo <- population[p]  
      winner <- which.max(sapply(torneo, function(sol){ sol$fitness }))
      torneo[[winner]]
    })
  }
  
  make.crossover.OX <- function(new.population){
    cruces <- lapply(1:n.cruces, function(i){
      make.OX.cruce(new.population[[i]], new.population[[i%%n.cruces + 1]]) 
    })
    
    new.population[1:n.cruces] <- cruces
  }
  
  make.mutation <- function(new.population){
    crom.mutar <- sample(1:n.crom, n.mutations, replace=TRUE)
    gen.mutar <- sample(1:n, n.mutations, replace=TRUE) 
    
    mutations <- lapply(1:n.mutations, function(i){
      cromosoma <- new.population[[crom.mutar[i]]]
      gen <- cromosoma$mask[gen.mutar[i]]
      gen <- (gen+1) %% 2
      # Mutamos y recalculamos tasa
      cromosoma$mask[[gen.mutar[i]]] <- gen
      cromosoma$fitness <- tasa.clas(cromosoma$mask)
      
      cromosoma
    })
    
    new.population[crom.mutar] <- mutations
    new.population
  }
  
  keep.elitism <- function(new.population, old.best){
    # Si el antiguo mejor no está en la población
    if (!TRUE %in% (sapply (new.population, 
                            function(x) { TRUE %in% (x$mask == old.best$mask) }))){
      
      if(new.population[[1]]$fitness < old.best$fitness){
        new.population[[1]] <- old.best$mask
      }
    }
    new.population
  }
  
  
  ##########################################################
  ##### Comienzo del algoritmo
  ##########################################################
  # Generación de la poblacion inicial
  population <- lapply(n.crom, function(i){
    mask <- gen.init(data)
    mask.tasa <- tasa.clas(mask)
    list(mask = mask, fitness = mask.tasa)
  })
  
  population <- sorted (population)
  
  
  # Bucle principal
  while(n.eval < max.eval){
    mother <- sample(1:n.crom, n.crom)
    father <- sample(1:n.crom, n.crom)
    parents <- Map(c,mother,father)     
    
    # Seleccion
    new.population <- make.selection(population)
    
    # Cruce
    new.population <- make.crossover.OX(new.population)
    
    # Mutaciones
    new.population <- make.mutation(new.population)
    
    # Elitismo
    new.population <- sorted (new.population)
    new.population <- keep.elitism (new.population, population[[n.crom]])
  }
  
  # Como la poblacion esta ordenada por tasa de menor a mayor...
  population[[n.crom]]$mask
}