##########################################################################
### Generador de soluciones iniciales aleatorias en selec. caracteristicas
###     Para un tamaño determinado devuelve una seleccion de 
###     caracteristicas aleatoria
##########################################################################

random.init <- function(data){ sample(0:1, ncol(data)-1, replace=TRUE) }

##########################################################################
#### Operador de cruce OX para los AGs
####    Dada una madre y un padre se genera un nuevo cromosoma
####    con una subcadena de la madre y rellenamos el resto con
####    los genes del padre
##########################################################################
crossover.OX <- function(mum, dad){
  n <- length(mum)
  cortes <- sample(1:n,2)
  mum.cromosomas <- seq(cortes[1],cortes[2])
  
  son <- dad$mask
  son[mum.cromosomas] <- mum$mask[mum.cromosomas]
  list(mask = son, fitness=tasa.clas(son))
}  

##########################################################################
### Funcion AGG (algoritmo genetico generacional)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar el algoritmo genetico
###     generacional
###
##########################################################################
AGG <- function(data, crossover = crossover.OX){
  n <- ncol(data)
  n <- n-1
  n.eval <- 0
  n.crom <- AGG.n.crom
  prob.cruce <- AGG.prob.cruce
  prob.mutation <- AGG.prob.mutation
  n.cruces <- ceiling(n.crom*prob.cruce)
  n.mutations <- ceiling(n*prob.mutation)

  ##########################################################
  #### Ordena una pobacion de menor a mayor tasa
  ##########################################################
  sorted <- function(population){
    order(sapply(population, function(x){ x$fitness }))
  }  
  
  ##########################################################
  ### Operador de seleccion
  ##########################################################  
  make.selection <- function(parents.paired){
    # Hace una seleccion por torneo binario
    lapply(parents.paired, function(p){
      torneo <- population[p]  
      winner <- which.max(sapply(torneo, function(sol){ sol$fitness }))
      torneo[[winner]]
    })
  }
  
  ##########################################################
  #### Hace los cruces en la poblacion usando el operador
  #### de cruce
  ##########################################################  
  make.crossover <- function(new.population){
    cruces <- lapply(1:n.cruces, function(i){
      crossover(new.population[[i]], new.population[[i%%n.cruces + 1]]) 
    })
    
    new.population[1:n.cruces] <- cruces
    new.population
  }
  
  ##########################################################
  #### Operador de mutacion
  ##########################################################
  make.mutation <- function(new.population){
    crom.mutar <- sample(1:n.crom, n.mutations, replace=TRUE)
    gen.mutar <- sample(1:n, n.mutations, replace=TRUE) 
    
    # Hacemos las mutaciones en los genes de las codificaciones
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
  
  ##########################################################
  #### Mantiene el elitismo en la solucion
  ##########################################################  
  keep.elitism <- function(new.population, old.best){
    # Si el antiguo mejor no está en la poblacion,
    # lo cambiamos por el nuevo peor
    if (!TRUE %in% (sapply (new.population, 
                            function(x) { TRUE %in% (x$mask == old.best$mask) }))){
      
      if(new.population[[1]]$fitness < old.best$fitness){
        new.population[[1]] <- old.best$mask
      }
    }
    new.population
  }
  
  ##########################################################
  #### Comienzo del algoritmo
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
    pairs <- Map(c, sample(1:n.crom, n.crom), sample(1:n.crom, n.crom))
    
    # Seleccion
    new.population <- make.selection(pairs)
    # Cruce
    new.population <- make.crossover(new.population)
    # Mutaciones
    new.population <- make.mutation(new.population)
    # Elitismo
    new.population <- sorted (new.population)
    new.population <- keep.elitism (new.population, population[[n.crom]])
  }
  
  # Como la poblacion esta ordenada por tasa de menor a mayor...
  population[[n.crom]]$mask
}


##########################################################################
### Funcion AGE (algoritmo genetico estacionario)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar el algoritmo genetico
###     estacionario
###
##########################################################################
AGE <- function(data, crossover = crossover.OX){
  n <- ncol(data)
  n <- n-1
  n.eval <- 0
  n.crom <- AGE.n.crom
  prob.mutation <- AGE.prob.mutation
  
  
  ##########################################################
  ### Operador de seleccion
  ##########################################################  
  make.selection <- function(parents.paired){
    # Hace una seleccion por torneo binario
    lapply(parents.paired, function(p){
      torneo <- population[p]  
      winner <- which.max(sapply(torneo, function(sol){ sol$fitness }))
      torneo[[winner]]
    })
  }
  
  ##########################################################
  #### Hace los cruces en la poblacion usando el operador
  #### de cruce
  ##########################################################  
  make.crossover <- function(new.population){
    cruces <- lapply(1:n.cruces, function(i){
      crossover(new.population[[i]], new.population[[i%%n.cruces + 1]]) 
    })
    
    new.population[1:n.cruces] <- cruces
    new.population
  }
  
  ##########################################################
  #### Operador de mutacion
  ##########################################################
  make.mutation <- function(new.population){
    crom.mutar <- sample(1:n.crom, n.mutations, replace=TRUE)
    gen.mutar <- sample(1:n, n.mutations, replace=TRUE) 
    
    # Hacemos las mutaciones en los genes de las codificaciones
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
  
  
  ##########################################################
  #### Comienzo del algoritmo
  ##########################################################
  
  # Generación de la poblacion inicial
  population <- lapply(n.crom, function(i){
    mask <- gen.init(data)
    mask.tasa <- tasa.clas(mask)
    list(mask = mask, fitness = mask.tasa)
  })
  
  
  # Bucle principal
  while(n.eval < max.eval){
    
    
    # Seleccion
    new.population <- make.selection(pairs)
    # Cruce
    new.population <- make.crossover(new.population)
    # Mutaciones
    new.population <- make.mutation(new.population)
    # Elitismo
    new.population <- sorted (new.population)
    new.population <- keep.elitism (new.population, population[[n.crom]])
  }
  
  # Como la poblacion esta ordenada por tasa de menor a mayor...
  population[[n.crom]]$mask
}
