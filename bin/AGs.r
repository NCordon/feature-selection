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
### Funcion de encapsulacion genetica
###     Contiene la implementacion de los algoritmos geneticos generacional
###     y estacionario
###
##########################################################################
AG <- function(data, crossover = crossover.OX){
  n <- ncol(data)
  n <- n-1
  n.crom <- AG.n.crom
  
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
  make.crossover <- function(population, prob.cruce){
    size.population <- length(population)
    n.cruces <- ceiling( size.population*prob.cruce )
    
    cruces <- lapply(1:n.cruces, function(i){
      chromosome <- crossover(population[[i]], population[[i%%n.cruces + 1]]) 
      list(mask = chromosome, fitness=0, evaluated = FALSE)
    })
    
    population[1:n.cruces] <- cruces
    population
  }
  
  ##########################################################
  #### Operador de mutacion
  ##########################################################
  make.mutation <- function(population, prob.mutation){
    size.population <- length(population)
    n.mutations <- ceiling( n*size.population*prob.mutation )
    
    crom.mutate <- sample(1:size.population, n.mutations, replace=TRUE)
    gen.mutate <- sample(1:n, n.mutations, replace=TRUE) 
    
    # Hacemos las mutaciones en los genes de las codificaciones
    mutations <- lapply(1:n.mutations, function(i){
      chromosome <- population[[crom.mutate[i]]]$mask
      
      gen <- chromosome[gen.mutate[i]]
      gen <- (gen+1) %% 2
      
      # Mutamos y recalculamos tasa
      chromosome[gen.mutate[i]] <- gen
      
      list(mask = chromosome, fitness=0, evaluated = FALSE)
    })
    
    population[crom.mutate] <- mutations
    population
  }
  
  ##########################################################
  #### Reemplaza el peor de la poblacion por otro dado
  #### caso de ser mejor cromosoma que el
  ##########################################################  
  make.replacement <- function(population, old.best){
    # Si el antiguo mejor no está en la poblacion,
    # lo cambiamos por el nuevo peor
    
    population <- sorted(population)
    
    if (!TRUE %in% (sapply (population, 
          function(x) { !FALSE %in% (x$mask == old.best$mask) }))){
      
      if(population[[1]]$fitness < old.best$fitness){
        population[[1]] <- old.best
      }
    }
    
    population
  }
  
  ##########################################################  
  ### Cuenta las tasas que hay que reevaluar
  ##########################################################
  count.not.evaluated <- function(population){
    sum(sapply(population, function(x){ x$evaluated==FALSE }))
  }
  
  
  ##########################################################  
  ### Recalcula las tasas de la poblacion desactualizadas
  ##########################################################
  calc.fitness <- function(population){
    lapply(population, function(x){ 
      if(!x$evaluated){
        x$evaluated <- TRUE
        x$fitness <- tasa.clas(data, x$mask)
      }
      x
    })
  }
  
  
  ##########################################################
  #### Comienzo del algoritmo
  ##########################################################
  
  # Funcion de generacion de la poblacion inicial
  population <- lapply(1:n.crom, function(i){
    mask <- random.init(data)
    mask.tasa <- tasa.clas(data, mask)
    list(mask = mask, fitness = mask.tasa, evaluated = TRUE)
  })

  population <- sorted (population)  

  
  
  generational <- function(){
    prob.cruce <- AGG.prob.cruce
    prob.mutation <- AGG.prob.mutation
    n.eval <- 0
    
    # Bucle principal
    while(n.eval < max.eval){
      pairs <- Map(c, sample(1:n.crom, n.crom), sample(1:n.crom, n.crom))
      
      # Conservamos el antiguo mejor de la poblacion
      old.best <- population[[n.crom]]
      # Seleccion
      population <- make.selection(pairs)
      # Cruce
      population <- make.crossover(population, prob.cruce)
      # Mutaciones
      population <- make.mutation(population, prob.mutation)
      # Recalculamos las tasas de la poblacion
      n.eval <- n.eval + count.not.evaluated (population)
      population <- calc.fitness(population)
      # Elitismo
      population <- make.replacement (population, old.best)
    }  
    
    # Ordenando la poblacion por tasa de menor a mayor, delvolvemos el mejor...
    population <- sorted(population)
    population <- population[[n.crom]]$mask
  }
  
  stationary <- function(){
    prob.cruce <- AGE.prob.cruce
    prob.mutation <- AGE.prob.mutation
    n.eval <- 0
    
    while(n.eval < max.eval){
      # Sacamos dos padres al azar
      pairs <- sample(1:n.crom, 2)
      
      # Seleccion
      new.generation <- make.selection(pairs)
      # Cruce
      new.generation <- make.crossover(new.generation, prob.cruce)
      # Mutaciones
      new.generation <- make.mutation(new.generation, prob.mutation)
      
      n.eval <- n.eval + count.not.evaluated (new.generation)
      new.generation <- calc.fitness(new.generation)
      new.generation <- sorted(new.generation)
      
      # Introducimos en la nueva poblacion los dos hijos,
      # caso de ser mejores que las de las peores soluciones
      population <- make.replacement (population, new.generation[[2]])
      population <- make.replacement (population, new.generation[[1]])
    }  
    # Ordenando la poblacion por tasa de menor a mayor, delvolvemos el mejor...
    population <- sorted(population)
    population <- population[[n.crom]]$mask
  }
  
  list (generational = generational, stationary = stationary)
}

##########################################################################
### Funcion AGG (algoritmo genetico generacional)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar el algoritmo genetico
###     generacional
###
##########################################################################
AGG <- function(data){
  AG(data)$generational()
}

##########################################################################
### Funcion AGE (algoritmo genetico estacionario)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar el algoritmo genetico
###     estacionario
###
##########################################################################
AGE <- function(data){
  AG(data)$stationary()
}