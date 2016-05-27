##########################################################################
### Funcion de calculo de la entropia
###     Recibe un dataframe y devuelve el cálculo de la información
###     heuristica. La usan las metaheuristicas de colonia de hormigas.
###     Devuelve un vector con el valor de la entropia de cada variable
###     que interviene en el dataset
###
##########################################################################

heuristic.info <- function(data){
  lapply(1:n, function(i){
    sum(
      sapply( unique(data$class), function(c){
        # Deben ser 0 o 1 en este caso
        values.c <- unique(data[,i])
        
        sum(
          sapply(values.c, function(f){
            dist.f <- data[data[,i] == f, ]
            prob.c.f <- sum(dist.f$class == c) / nrow(dist.f)
            prob.c <- sum(data$class == c) / nrow(data)
            prob.f <- sum(data[,i] == f) / nrow(data)
            
            prob.c.f * log(prob.c.f / (prob.c * prob.f), base=2)
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
  #### Construye una ruleta con las probabilidades dadas en
  #### prob.vector, y selecciona un indice segun prob
  ##########################################################
  roulette.selection <- function(prob.vector, prob){
    roll.probs <- sapply(1:length(prob.vector), fucntion(i){
      sum(prob.vector[1:i])
    })
    
    index <- which( roll.probs <= prob )
    
    if (any(index)){
      result <- max(index)
    }
    else{
      result <- n
    }
    
    result
  }
  
  ##########################################################
  #### Funcion de transicion del sistema de hormigas en base
  #### a la feromona, a la heuristica, y a los valores de
  #### alpha y beta
  ##########################################################
  make.transiction <- function(path){
    values <- (!path * trail.features**alpha * data.heuristic**beta)
    
    if (runif(1) < prob.trans){
      selected <- which.max(values)
    }
    else{
      values <- normalize.vector(values)
      selected <- roulette.selection(values)
    }
    
    selected
  }
  
  ##########################################################
  #### Funcion de actualizacion de la actualizacion de la 
  #### feromona local
  ##########################################################
  update.local.trail <- function(trail, s){
    trail[s] <- (1-local.evap) * trail[s] + local.evap * init.trail
    
    normalize.vector(trail)
  }
  
  
  ##########################################################
  #### Funcion de actualizacion de la actualizacion de la 
  #### feromona global
  ##########################################################
  update.global.trail <- function(trail, extra, mask){
    normalize.vector(trail + ((1 - global.evap) * trail +  
                    global.evap * extra) * mask)
    
  }
  
  
  for(i in 1:max.eval){
    # Inicializacion de los caminos seguidos por cada hormiga
    paths <- lapply(1:num.ants, sample(c(1,rep(0,n-1))))
    
    # Numero de caracteristicas que escogera cada hormiga
    num.features <- sapply(runif(num.ants), function(prob){
      roulette.selection(trail.num.features, prob) - 1
    })
    
    for(k in 1:num.ants){
      if (num.features[k] > 0){  
        # Transicion
        selected <- make.transiction(paths[[i]])
        paths[[i]][selected] <- 1

        # Actualizacion local de la feromona
        trail.features <- update.local.trail(trail.features, selected)
        
        num.features[k] <- num.features[k] - 1
      }
    }
    
    # Aplicamos busqueda local a los caminos encontrados por las hormigas
    paths <- lapply(paths, function(p){ BL(p,1) })
    
    # Buscamos la mejor solucion de todas las encontradas por las hormigas
    paths.score <- lapply(paths, function(p){tasa.clas(data, p)})
    where.max <- which.max(paths.score)
    
    if(paths.score[where.max] > tasa.best){
      mask.best <- paths[where.max]
    }
    
    # Actualizacion global de feromona
    trail.features <- update.global.trail(trail.features, 
                      paths.score[where.max], paths[where.max])
    trail.num.features <- update.global.trail(trail.num.features, 
                          paths.score[where.max], 
                          c(rep(0,where.max-1), 1, rep(0, n-where.max)))
  }
  
  mask.best
}