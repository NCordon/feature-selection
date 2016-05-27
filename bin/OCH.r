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
### Funcion sistema de colonia de hormigas
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar sistema de colonia de
###     hormigas reforzado con búsqueda local
###
##########################################################################

SCH.BL <- function(data){
  n <- ncol(data)
  n <- n-1
  num.ants <- OCH.num.ants
  alpha <- OCH.alpha
  beta <- OCH.beta
  global.evap <- OCH.evaporation
  
  # q0
  prob.trans <- SCH.BL.prob.trans
  # phi
  local.evap <- SCH.BL.evap
  # Profundidad de la busqueda local
  prof.bl <- SCH.BL.prof.bl * n
  mask.best <- rep(0,n)
  tasa.best <- 0
  
  # Inicializacion de parametros
  init.trail <- 1e-6
  trail.features <- rep(init.trail, n)
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
  make.transitions <- function(path, num.car){
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
    normalize.vector( trail + ((1 - factor.evap) * trail +  
                      factor.evap * extra) * mask )
  }
  
  
  for(i in 1:(max.eval/(num.ants + num.ants*prof.bl))){
    # Inicializacion de los caminos seguidos por cada hormiga
    paths <- lapply(1:num.ants, function(x){ sample(c(1,rep(0,n-1))) })
    
    # Numero de caracteristicas que escogera cada hormiga
    num.features <- sample( 1:n, size=num.ants, replace=T, prob=trail.num.features)
    
    for(k in 1:num.ants){
      # Transicion
      paths[[k]] <- make.transitions(paths[[k]], num.features[[k]] - 1 )

      # Actualizacion local de la feromona
      trail.features <- update.trail(trail.features, local.evap, 
                                    init.trail, paths[[k]])
    }
    
    # Aplicamos busqueda local a los caminos encontrados por las hormigas
    paths <- lapply(paths, function(p){ 
      BL(data, gen.init = function(data){ p }, max.eval=prof.bl) 
    })
    
    # Buscamos la mejor solucion de todas las encontradas por las hormigas
    paths.score <- sapply(paths, function(p){tasa.clas(data, p)})
    where.max <- which.max(paths.score)
    
    if(paths.score[where.max] > tasa.best){
      mask.best <- paths[[where.max]]
    }
    
    # Actualizacion global de feromona
    trail.features <- update.trail(trail.features, global.evap,
                      paths.score[where.max], paths[[where.max]])
    
    trail.num.features <- update.trail(trail.num.features, 
                          global.evap, paths.score[where.max], 
                          c(rep(0,where.max-1), 1, rep(0, n-where.max)))
  }
  
  mask.best
}