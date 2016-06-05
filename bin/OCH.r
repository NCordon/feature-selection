##########################################################################
### Funcion de calculo de la entropia
###     Recibe un dataframe y devuelve el cálculo de la información
###     heuristica. La usan las metaheuristicas de colonia de hormigas.
###     Devuelve un vector con el valor de la entropia de cada variable
###     que interviene en el dataset
###
###     Recibe como parámetro h, número de valores discretos en caso de
###     que la variable sea continua. Por defecto, 10
##########################################################################

heuristic.info <- function(data, h = 10){
  sapply(1:(ncol(data)-1), function(i){
    
    if(is.factor(data[,i])){
      discretized.values <- split(data[,i], data[,i])
      
    } else{
      columna <- sort(data[,i])
      chunk.size <- ceiling( length(columna)/h )
      
      discretized.values <- split(columna, ceiling( seq_along(columna) / chunk.size) )
    }
    
    sum(
      sapply( unique(data$class), function(c){
        sum(
          sapply(discretized.values, function(f){
            dist.f <- data[data[,i] %in% f, ]
            prob.c.f <- sum(dist.f$class == c) / length(f)
            prob.c <- sum(data$class == c) / nrow(data)
            prob.f <- length(f) / nrow(data)
            
            if (prob.c.f > 0){
              prob.c.f * log(prob.c.f / (prob.c * prob.f), base=2)
            } else 0
          })
        )  
      })
    )
  })
}

##########################################################################
### Funcion de encapsulacion de hormigas
###     Contiene la implementacion de los algoritmos de optimización de
###     hormigas sistema de colonia de hormigas, y sistema de hormigas
###     max-min, ambos con búsqueda local
###
##########################################################################

OCH <- function(data){
  n <- ncol(data)
  n <- n-1
  n.eval <- 0
  num.ants <- OCH.num.ants
  alpha <- OCH.alpha
  beta <- OCH.beta
  global.evap <- OCH.evaporation
  
  # q0
  prob.trans <- OCH.BL.prob.trans
  # phi
  local.evap <- OCH.BL.evap
  # Profundidad de la busqueda local
  prof.bl <- OCH.BL.prof.bl
  mask.best <- random.init(data)
  tasa.best <- tasa.clas(data, mask.best)
  
  # Inicializacion de parametros
  trail.num.features <- rep(1.0/n, n)
  data.heuristic <- heuristic.info(data)
  
  ##########################################################
  #### Funcion de normalizacion de vectores que hace que la
  #### suma de sus componentes valga uno
  ##########################################################
  normalize.vector <- function(v){
    v/sum(v)  
  }
  
  ##########################################################
  #### Funcion de transicion del sistema de hormigas en base
  #### a la feromona, a la heuristica, y a los valores de
  #### alpha y beta
  ##########################################################
  make.transitions <- function(path, num.car, trail.features){
    if(num.car > 0){
      make.trans.prob <- runif(num.car)
      
      for (i in 1:num.car){
        values <- (as.numeric(!path) * trail.features**alpha * data.heuristic**beta)
        
        # Regla de la colonia de hormigas
        if (make.trans.prob[i] < prob.trans){
          selected <- which.max(values)
          
          # Regla del sistema de hormigas
        } else {
          non.selected <- (1:n)[values > 0]
          values <- values[values > 0]
          selected <- non.selected[ sample( 1:length(non.selected), size=1, prob=values ) ]
        }
        path[selected] <- 1
      }
    }
    path 
  }
  
  ##########################################################
  #### Funcion de actualizacion de la feromona
  ##########################################################
  update.trail <- function(trail, factor.evap, extra, mask){
    result <- trail * as.numeric(!mask) + 
              ((1 - factor.evap) * trail +  
              factor.evap * extra) * as.numeric(mask)
    # normalize.vector(result)
    result
  }
  
  
  OCH.algorithm <- function(init.trail, normalize.trails, 
                            local.update, trail.max, trail.min){
    
    trail.features <- rep(init.trail, n)
    
    while(n.eval < max.eval){
      # Inicializacion de los caminos seguidos por cada hormiga
      paths <- lapply(1:num.ants, function(x){ sample(c(1,rep(0,n-1))) })
      
      # Numero de caracteristicas que escogera cada hormiga
      num.features <- sample( 1:n, size=num.ants, 
                              replace=T, prob=trail.num.features) - 1
      
      for(k in 1:num.ants){
        # Transicion
        paths[[k]] <- make.transitions(paths[[k]], num.features[[k]], trail.features)
        
        if(local.update){
          # Actualizacion local de la feromona
          trail.features <- update.trail(trail.features, local.evap, 
                                         init.trail, paths[[k]])
        }
      }
    
      # Aplicamos busqueda local a los caminos encontrados por las hormigas
      paths <- lapply(paths, function(p){ BL.entornos(data, p, max.entornos=1) })
      n.eval <- n.eval + sum(sapply(paths, '[[', 2))
      paths <- lapply(paths, '[[', 1)
      
      # Buscamos la mejor solucion de todas las encontradas por las hormigas
      paths.score <- sapply(paths, function(p){tasa.clas(data, p)})
      n.eval <- n.eval + num.ants
      where.max <- which.max(paths.score)
      
      if(paths.score[where.max] > tasa.best){
        mask.best <- paths[[where.max]]
        
        if ( normalize.trails ){
          trail.max <- tasa.clas( data, mask.best )
          trail.min <- trail.max/500
        }
      }
      
      # Actualizacion global de feromona
      trail.features <- update.trail(trail.features, global.evap,
                                     paths.score[where.max], paths[[where.max]])
      
      trail.num.features <- update.trail(trail.num.features, 
                                         global.evap, paths.score[where.max], 
                                         c(rep(0,where.max-1), 1, rep(0, n-where.max)))
      
      if ( normalize.trails ){
        trail.features[ trail.features > trail.max ] <- trail.max
        trail.features[ trail.features < trail.min ] <- trail.min
      }
    }
    
    mask.best
  }
  
  
  SCH.BL <- function(){
    OCH.algorithm(init.trail = 1e-6, local.update = T,
                  normalize.trails = F)
  }
  
  SHMM.BL <- function(){
    init.trail <- tasa.clas( data, mask.best)/local.evap 
    
    OCH.algorithm(init.trail = init.trail , 
                  trail.max = init.trail,
                  trail.min = init.trail/500,
                  normalize.trails = T, local.update = F)
  }  
  list ( SCH.BL = SCH.BL, SHMM.BL = SHMM.BL)
}



##########################################################################
### Funcion sistema de colonia de hormigas
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar sistema de colonia de
###     hormigas reforzado con búsqueda local
###
##########################################################################
SCH.BL <- function(data){
  OCH(data)$SCH.BL()
}

##########################################################################
### Funcion sistema de hormigas max-min
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar sistema de hormigas
###     max-min reforzado con búsqueda local
###
##########################################################################
SHMM.BL <- function(data){
  OCH(data)$SHMM.BL()
}

