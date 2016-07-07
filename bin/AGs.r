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
AG <- function(data, crossover = crossover.OX, function.BL, gen.memetic){
  n <- ncol(data)
  n <- n-1
  n.crom <- AG.n.crom
  memetic.algorithm <- !missing(function.BL) && !missing(gen.memetic)

  ##########################################################
  #### Ordena una pobacion de menor a mayor tasa
  ##########################################################
  sorted <- function(population){
    population[ order(sapply(population, function(x){ x$fitness })) ]
  }  
  
  ##########################################################
  ### Operador de seleccion
  ##########################################################  
  make.selection <- function(new.size){
    pairs <- Map(c, sample(1:n.crom, new.size), sample(1:n.crom, new.size))
    # Hace una seleccion por torneo binario
    lapply(pairs, function(p){
      sorted( population[p] ) [[length(p)]]  
    })
  }
  
  ##########################################################
  #### Hace los cruces en la poblacion usando el operador
  #### de cruce
  ##########################################################  
  make.crossover <- function(population, prob.cruce){
    size.population <- length(population)
    n.cruces <- ceiling( size.population*prob.cruce*0.5 )
    
    cruces <- lapply(seq(1, n.cruces, 2), function(i){
      chrom.one <- crossover(population[[i]], population[[i+1]]) 
      chrom.two <- crossover(population[[i+1]], population[[i]]) 
      
      c(list(mask = chrom.one, fitness=0, evaluated = FALSE),
        list(mask = chrom.two, fitness=0, evaluated = FALSE))
    })
    
    population[1:length(cruces)] <- cruces
    population
  }
  
  ##########################################################
  #### Operador de mutacion
  ##########################################################
  make.mutation <- function(population, prob.mutation){
    size.population <- length(population)
    n.mutations <- n*size.population*prob.mutation
    
    # Comprobación para no realizar demasiadas mutaciones
    if (runif(1, 0.0, 1.0) < n.mutations){
      n.mutations <- ceiling(n.mutations)
      crom.mutate <- sample(1:size.population, n.mutations, replace=TRUE)
      gen.mutate <- sample(1:n, n.mutations, replace=TRUE) 
      
      # Hacemos las mutaciones en los genes de las codificaciones
      mutations <- lapply(1:n.mutations, function(i){
        chromosome <- population[[crom.mutate[i]]]$mask
      
        # Mutamos y recalculamos tasa
        chromosome[gen.mutate[i]] <- !chromosome[gen.mutate[i]]
        
        list(mask = chromosome, fitness=0, evaluated = FALSE)
      })
      
      population[crom.mutate] <- mutations
    }
    
    population
  }
  
  ##########################################################
  #### Reemplaza el peor de la poblacion por otro dado
  #### caso de ser mejor cromosoma que el
  ##########################################################  
  keep.elitism <- function(population, old.best){
    # Si el antiguo mejor no está en la poblacion,
    # lo cambiamos por el nuevo peor
    
    population <- sorted(population)
    
    if (!TRUE %in% (sapply (population, 
          function(x) { !FALSE %in% (x$mask == old.best$mask) }))){
      
      if(population[[1]]$fitness < old.best$fitness){
        population[[1]] <- old.best
        population <- sorted(population)
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
  eval.fitness <- function(population){
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
    evs.bl <- 0
    n.generations <- 0
    
    # Bucle principal
    while(n.eval < max.eval){
      
      # Aplicamos busqueda local si el algoritmo es memetico
        
      if (memetic.algorithm && n.generations %% gen.memetic == 0 
          && n.generations !=0){
        result.bl <- function.BL(population)
        population <- result.bl[[1]]
        evs.bl <- result.bl[[2]]
      }
      
      # Conservamos el antiguo mejor de la poblacion
      old.best <- population[[n.crom]]
      
      # Seleccion
      population <- make.selection(n.crom)
      # Cruce
      population <- make.crossover(population, prob.cruce)
      # Mutaciones
      population <- make.mutation(population, prob.mutation)
      
      # Recalculamos las tasas de la poblacion
      n.eval <- n.eval + count.not.evaluated (population) + evs.bl
      population <- eval.fitness(population)
      # Elitismo
      population <- keep.elitism (population, old.best)
      
      # Aumentamos el número de generaciones
      n.generations <- n.generations + 1
    }  
    
    # Ordenando la poblacion por tasa de menor a mayor, delvolvemos el mejor...
    population <- sorted(population)
    
    if (AM.backward.selection){
      backward.selection(data, population[[n.crom]]$mask)
    } else{
      population[[n.crom]]$mask
    }
  }
  
  stationary <- function(){
    prob.cruce <- AGE.prob.cruce
    prob.mutation <- AGE.prob.mutation
    n.eval <- 0
    
    while(n.eval < max.eval){
      # Seleccion
      new.generation <- make.selection(2)
      # Cruce
      new.generation <- make.crossover(new.generation, prob.cruce)
      # Mutaciones
      new.generation <- make.mutation(new.generation, prob.mutation)
      
      n.eval <- n.eval + count.not.evaluated (new.generation)
      new.generation <- eval.fitness(new.generation)
      new.generation <- sorted(new.generation)
      
      # Introducimos en la nueva poblacion los dos hijos,
      # caso de ser mejores que las de las peores soluciones
      population <- keep.elitism (population, new.generation[[2]])
      population <- keep.elitism (population, new.generation[[1]])
    }  
    # Ordenando la poblacion por tasa de menor a mayor, delvolvemos el mejor...
    population <- sorted(population)
    
    
    if (AM.backward.selection){
      backward.selection(data, population[[n.crom]]$mask)
    } else{
      population[[n.crom]]$mask
    }
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


##########################################################################
### Funciones de búsqueda local para los algoritmos memeticos
###
### prob        probabilidad de aplicar busqueda local
### prof.bl     profundidad de la busqueda local que se aplica
### apply.best  si esta a TRUE, aplica busqueda local a los prob*N mejores
##########################################################################
memetic.BL <- function(data, prob, prof.bl, apply.best){
  function(population){
    
    size.population <- length(population)
    evs <- 0
    
    if (apply.best){
      # Se aplica a los prob*N mejores de la poblacion
      best.number <- ceiling(prob * size.population)
      apply.to <- c( rep(F, length(population) - best.number), 
                     rep(T, best.number) )
  
    } else {
      apply.to <- (runif(size.population) <= prob)
    }
    
    for (i in 1:length(population)){
      if (apply.to[i]){
        result <- BL.entornos(data, population[[i]]$mask, max.entornos=prof.bl)
        population[[i]] <- list (mask = result$mask, fitness = 0, evaluated = FALSE)
        evs <- evs + result$eval
      }
    }
    
    list(population, evs)
  }
}


##########################################################################
### Funciones AM (algoritmos memeticos)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar un algoritmo memetico
###     con distintos parametros en su busqueda local
###
##########################################################################

AM.10.1 <- function(data){
  AG(data = data, function.BL = memetic.BL(data, prob = 1, prof.bl = AM.prof.bl, apply.best = F), 
     gen.memetic=10)$generational()
}

AM.10.0.1 <- function(data){
  AG(data = data, function.BL = memetic.BL(data, prob = 0.1, prof.bl = AM.prof.bl, apply.best = F), 
     gen.memetic=10)$generational()
}
  
AM.10.0.1.mej <- function(data){
  AG(data = data, function.BL = memetic.BL(data, prob = 0.1, prof.bl = AM.prof.bl, apply.best = T), 
     gen.memetic=10)$generational()
}