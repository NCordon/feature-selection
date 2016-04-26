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
crossover.OX <- function(xx, xy){
  n <- length(xx$mask)
  bounds <- sample(1:n,2)
  bounds <- bounds[1]:bounds[2]
  
  chromosome <- xx$mask
  chromosome[bounds] <- xy$mask[bounds]
  chromosome
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
  n.crom <- AGG.n.crom
  prob.cruce <- AGG.prob.cruce
  prob.mutation <- AGG.prob.mutation
  n.cruces <- ceiling(n.crom*prob.cruce)
  
  ##########################################################
  #### Ordena una pobacion de menor a mayor tasa
  ##########################################################
  sorted <- function(population){
    population[ order(sapply(population, function(x){ x$fitness })) ]
  }  
  
  ##########################################################
  ### Operador de seleccion
  ##########################################################  
  make.selection <- function(parents.paired){
    # Hace una seleccion por torneo binario
    lapply(parents.paired, function(p){
      sorted( population[p] ) [[length(p)]]  
    })
  }
  
  ##########################################################
  #### Hace los cruces en la poblacion usando el operador
  #### de cruce
  ##########################################################  
  make.crossover <- function(population){
    cruces <- lapply(1:n.cruces, function(i){
      mask <- crossover(population[[i]], population[[i%%n.cruces + 1]]) 
      list(mask = mask, fitness=tasa.clas(data, mask))
    })
    
    population[1:n.cruces] <- cruces
    population
  }
  
  ##########################################################
  #### Operador de mutacion
  ##########################################################
  make.mutation <- function(population){
    size.population <- length(population)
    n.mutations <- ceiling( n*size.population*prob.mutation)
    
    crom.mutate <- sample(1:size.population, n.mutations, replace=TRUE)
    gen.mutate <- sample(1:size.population, n.mutations, replace=TRUE) 
    
    # Hacemos las mutaciones en los genes de las codificaciones
    mutations <- lapply(1:n.mutations, function(i){
      chromosome <- population[[crom.mutate[i]]]
      
      gen <- chromosome$mask[gen.mutate[i]]
      gen <- (gen+1) %% 2
      
      # Mutamos y recalculamos tasa
      chromosome$mask [[ gen.mutate[i] ]] <- gen
      chromosome$fitness <- tasa.clas(data, chromosome$mask)
      
      chromosome
    })
    
    population[crom.mutate] <- mutations
    population
  }
  
  ##########################################################
  #### Mantiene el elitismo en la solucion
  ####    NOTA: Devuelve una poblacion ordenada
  ####          de menor a mayor en tasa
  ##########################################################  
  keep.elitism <- function(population){
    # Si el antiguo mejor no está en la poblacion,
    # lo cambiamos por el nuevo peor
    
    population <- sorted(population)
    
    if (!TRUE %in% (sapply (population, 
          function(x) { !FALSE %in% (x$mask == old.best$mask) }))){
      
      if(population[[1]]$fitness < old.best$fitness){
        population[[1]] <- old.best
      }
    }
    
    sorted(population)
  }
  
  ##########################################################
  #### Comienzo del algoritmo
  ##########################################################
  
  # Generación de la poblacion inicial
  population <- lapply(1:n.crom, function(i){
    mask <- random.init(data)
    mask.tasa <- tasa.clas(data, mask)
    list(mask = mask, fitness = mask.tasa)
  })
  
  population <- sorted (population)
  
  # Bucle principal
  for(n.eval in 1:max.eval){
    pairs <- Map(c, sample(1:n.crom, n.crom), sample(1:n.crom, n.crom))
    
    # Conservamos el antiguo mejor de la poblacion
    old.best <- population[[n.crom]]
    
    # Seleccion
    population <- make.selection(pairs)
    # Cruce
    population <- make.crossover(population)
    # Mutaciones
    population <- make.mutation(population)
    # Elitismo
    population <- keep.elitism (population)
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
    n <- length(new.population)
    n.mutations <- ceiling(n*prob.mutation)
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
  
  population <- sorted (population)
  
  # Bucle principal
  while(n.eval < max.eval){
    pairs <- Map(c, sample(1:n.crom, 2), sample(1:n.crom, 2))
    
    # Seleccion
    new.population <- make.selection(pairs)
    # Cruce
    new.population <- make.crossover(new.population)
    # Mutaciones
    new.population <- make.mutation(new.population)
    new.population <- sorted (new.population)
    
    
    # Falta hacer el reemplazamiento
    
    population <- new.population
  }
  
  # Como la poblacion esta ordenada por tasa de menor a mayor...
  population[[n.crom]]$mask
}