##########################################################################
### Generador de soluciones iniciales aleatorias en selec. caracteristicas
###     Para un tamaño determinado devuelve una seleccion de 
###     caracteristicas aleatoria
##########################################################################

random.init <- function(data){ sample(0:1, ncol(data)-1, replace=TRUE) }


##########################################################################
# Funcion de normalizacion de datasets
##########################################################################

normalize.dataset <- function(data){
  colnames(data) <- tolower(colnames(data))
  names <- colnames(data)[ colnames(data) != "class" ]
  
  # La ultima columna es la de clase, y quitamos columnas con un unico valor
  data <- data[,c(names, "class")]
  data <- Filter(function(x){ length(unique(x))>1 }, data)
  
  # Columnas con valores entre 0 y 1
  data.frame(lapply(data, function(x){
    if(is.numeric(x)){
      (x-min(x))/(max(x)-min(x))
    } else x
  }))
}

##########################################################################
# Funcion de generacion de particiones
##########################################################################

make.partition <- function(data,per){
  rows <- sample(1:nrow(data), nrow(data)*per)
  
  list(train = data[rows,], test = data[-rows,])
}


##########################################################################
### Funcion tasa clasificacion
##########################################################################

tasa.clas <- function (train, mask){
  # Si la mascara es no nula...
  
  if (1 %in% mask){
    # Aplicamos la mascara
    cl <- train$class
    #print(mask)
    train <- subset(train, select = which(mask==1))
    #print('hola')
    # Obtenemos el fit que se haria del conjunto test para el 3-knn
    fit <- knn.cv(train, cl, k=3, use.all = TRUE)
    
    # Tasa de clasificacion
    result <- (100 * length(which(cl == fit)) / length(cl))
    
  } else {
    result <- 0
  }
  result
}


#########################################################################
### Funcion de validacion cruzada 
###     Para un algoritmo devuelve valores que permiten medir su calidad
###     como algoritmo, haciendo un 5x2 cross validation
###     Tasa clasificacion en media para el clasificador 3-knn
###     Tasa reduccion de caracteristicas en media
###     Media del tiempo de ejecucion
##########################################################################

cross.eval <- function(algorithm){
  n.eval <- length(semilla)
  all.results <- list()
  mean.results <- list()
  
  with.decimals <- function(v){ format(v, nsmall=5) }
  
  
  gather.results <- function(train, test, result){
    n.var <- length(colnames(train))-1
    
    t.ini <- proc.time()[3]
    mask <- algorithm(train)
    t.fin <- proc.time()[3]
    result$tasa.test <- c( result$tasa.test, tasa.clas(test,mask) )
    result$tasa.train <- c( result$tasa.train, tasa.clas(train,mask) )
    result$tasa.red <- c( result$tasa.red, (n.var - sum(mask==1))/n.var )
    result$t.exec <- c( result$t.exec, unname(t.fin - t.ini) )
    
    result
  }
  
  for (d in 1:length(datasets)){
    x <- datasets[[d]]
    result <- list(tasa.test = NULL, tasa.train = NULL, tasa.red = NULL, t.exec = NULL)
    
    cat("Procesando dataset", datasets.names[d], "\n")
    
    class.split <- split(x, x$class)
    
    for (i in 1:n.eval){
      set.seed(semilla[i])
      
      # Hacemos las particiones de entrenamiento y prueba
      partition <- lapply(class.split, make.partition, per=0.5 )
      train <- lapply (partition, function(x){ x$train } )
      train <- rbindlist(train)
      test <- lapply (partition, function(x){ x$test } )
      test <- rbindlist(test)
      
      # Primero usando la mascara dada por el train
      result <- gather.results(train, test, result)
      # Despues usando la mascara dada por el test
      result <- gather.results(test, train, result)
    }
    
    result <- data.frame(result)
    result.mean <- apply(result, 2, mean) 
    
    all.results[[d]] <- with.decimals(result)
    mean.results[[d]] <- result.mean
  }
  
  names(all.results) <- datasets.names
  names(mean.results) <- paste(datasets.names, ".media", sep="")
  # Salida
  append(all.results, mean.results)
} 

#########################################################################
### Funcion de backward selection 
###     Para un conjunto de datos y una mascara, va quitando componentes
###     seleccionadas en la mascara mientras no se pierda calidad de
###     scoring del algoritmo
##########################################################################

backward.selection <- function(data, mask){
  selected <- which(mask == 1)
  actual <- tasa.clas(data, mask)
  mejora <- TRUE
  
  while(mejora){
    evs <- sapply (selected, function(x){
      m <- mask
      m[x] <- 0
      tasa.clas(data, m)
    } )
    
    if (max(evs) < actual || length(evs)==0){
      mejora <- FALSE
    }
    else{
      sel <- selected[which.max (evs)]
      mask [sel] <- 0
      selected <- selected[selected != sel]
      actual <- max(evs)
    }
  }
  
  mask
}
