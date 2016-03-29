# Limpieza del entorno
rm(list = ls())


##########################################################################
# Lista de paquetes a cargar

  # foreign: leer ARFF 
  # data.table: rbindinglist
  # class: kknn
  # base: logaritmo
##########################################################################
# Cargamos pkgs

pkgs = c("foreign", "data.table", "class", "base")
to.install <- pkgs[ ! pkgs %in% installed.packages() ]

if ( length(to.install) > 0 )
  install.packages( to.install, dependencies = TRUE )
  
lapply(pkgs, require, character.only=TRUE)


##########################################################################
# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


# Función de normalización de datasets
normalize <- function(data){
  colnames(data) <- tolower(colnames(data))
  names <- colnames(data)[ colnames(data) != "class" ]
  
  # La última columna es la de clase, y quitamos columnas con un único valor
  data <- data[,c(names, "class")]
  data <- Filter(function(x){ length(unique(x))>1 }, data)
  
  # Columnas con valores entre 0 y 1
  data.frame(lapply(data, function(x){ 
    if(is.numeric(x)){
      x/(max(x)-min(x))
    }
    else x
  }))
}

# Normalizamos los datasets
mlibras <- normalize(mlibras)
arrhythmia <- normalize(arrhythmia)
wdbc <- normalize(wdbc)


##########################################################################
### Función tasa clasificación
##########################################################################
tasa.clas <- function (train, mask){
  # Aplicamos la máscara
  cl <- train$class
  train <- subset(train, select = which(mask==1))
  
  # Obtenemos el fit que se haría del conjunto test para el 3-knn
  fit <- knn.cv(train, cl, k = 3, l = 0, prob = FALSE, use.all = TRUE)
  
  # Tasa de clasificación
  return (100 * length(which(cl == fit)) / length(cl))
}


##########################################################################
# Función de generación de particiones
make.partition <- function(data,per){
  rows <- sample(1:nrow(data), nrow(data)*per) 
  
  list(train = data[rows,], test = data[-rows,])
}

##########################################################################
### Función SFS
###     Para un data frame devuelve para el clasificador 3-knn el conjunto 
###     de características que maximizan la tasa de clasificación de ese 
###     conjunto usando leaving one out en el knn euclídeo
##########################################################################

SFS <- function(data){
  n <- ncol(data)
  n <- n-1
  mask <- rep(0,n)
  non.selected <- seq(1,n)
  max <- 0
  
  repeat{
    evs <- sapply (non.selected, function(x){
      m <- mask
      m[x] <- 1
      tasa.clas(data, m)
    } )
    
    if (max(evs) <= max){
      break
    }
    
    mask [which.max (evs)] <- 1
    non.selected <- non.selected[non.selected != which.max(evs)]
    max <- max(evs)
  }
  
  mask
}


##########################################################################
### Función búsqueda local del primer mejor
###     Para un data frame devuelve para el clasificador 3-knn el conjunto 
###     de características que se obtienen de aplicar la búsqueda local del
###     primer mejor
##########################################################################

BL <- function(data){
  mask <- SFS(data)
  n <- length(mask)
  fin <- FALSE
  tasa.best <- tasa.clas(data, mask)
  max.eval <- 15000
  n.eval <- 0
  
  while(!fin && (n.eval < max.eval)){
    non.selected <- seq(1,n)
    
    while(!fin && (n.eval < max.eval)){
      # Generamos un vecino hasta que mejoremos
      # o hasta que agotemos el vecindario
      
      m <- mask
      
      repeat{
        j <- sample(1:n,1)
        
        if (j %in% non.selected){
          non.selected <- non.selected[non.selected!=j]
          break
        }
      }
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      n.eval <- n.eval + 1
      
      if (tasa.actual > tasa.best){
        mask <- m
        tasa.best <- tasa.actual
        fin <- TRUE
      }
      else{
        if (length(non.selected) == 0){         
          fin <- TRUE
        }
      }
    }
    # Si no hemos encontrado un vecino que mejore a la solución actual, fin del algoritmo
    fin <- (length(non.selected) == 0)
  }
  mask
}

##########################################################################
### Función búsqueda local del primer mejor
###     Para un data frame devuelve para el clasificador 3-knn el conjunto 
###     de características que se obtienen de aplicar la búsqueda local del
###     primer mejor
##########################################################################

SA <- function(data){
  # Solución greedy inicial
  mask <- SFS(data)
  n <- length(mask)
  tasa.best <- tasa.clas(data, mask)
  mask.best <- mask
  
  # Parámetros del enfriamiento simulado
  max.eval <- 15000
  max.vecinos <- n
  max.exitos <- 0.1*max.vecinos
  mu <- 0.3
  phi <- 0.3
  # Nota: los mejores valores probados para estos parámetros han sido 0.05 y 0.05
  
  
  t.actual <- mu*tasa.best/-log(phi, base=exp(1))
  t.final <- 1e-3
  beta <- (t.actual - t.final)/((max.eval/max.vecinos)*t.actual*t.final)
  
  n.eval <- 0
  n.vecinos <- 0
  n.exitos <- 0
  fin <- FALSE

  while(n.eval < max.eval & !fin & t.actual > t.final){
    n.vecinos <- 0
    n.exitos <- 0
    
    # Un enfriamiento
    while(n.vecinos < max.vecinos 
          & n.exitos < max.exitos 
          & n.eval < max.eval){
      # Generamos un vecino
      m <- mask
      fin <- TRUE
      
      i <- sample(1:n,1)
      m[i] <- (m[i]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      u <- runif(1, 0.0, 1.0)
      delta <- tasa.actual - tasa.best
      
      if (delta >= 0 || u <= exp(delta/t.actual)){
        mask <- m
        tasa.best <- tasa.actual
        n.exitos <- n.exitos + 1
        fin <- FALSE
        
        if (delta >=0){
          mask.best <- m
        }
        
      }
      
      n.vecinos <- n.vecinos + 1
      n.eval <- n.eval + 1
    }
  t.actual <- t.actual/(1 + beta*t.actual)  
  }
  mask.best
}


##########################################################################
### Función búsqueda tabú básica
###     Para un data frame devuelve para el clasificador 3-knn el conjunto 
###     de características que se obtienen de aplicar la tabú con memoria
###     a corto plazo
##########################################################################

BT <- function(data){
  mask <- SFS(data)
  mask.best <- mask
  tasa.best <- tasa.clas(data, mask)
  n <- length(mask)

  max.eval <- 15000
  max.vecinos <- 30
  # Tamaño máximo de la lista tabú
  max.tabu <- n/3
  
  # Lista tabú
  tabu.list <- c()
  
  # Posición a escribir de la lista tabú
  tl.pos <- 1
  n.eval <- 0
  
  while(n.eval < max.eval){
    non.selected <- seq(1,n)
    n.vecinos <- 0
    tasa.mejor.vecino <- 0
    
    while((n.vecinos < max.vecinos) && (n.eval < max.eval)){
      m <- mask
      n.vecinos <- n.vecinos + 1
      n.eval <- n.eval + 1
      
      repeat{
        j <- sample(1:n,1)
        
        if (j %in% non.selected){
          non.selected <- non.selected[non.selected!=j]
          break
        }
      }
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      tabu.act <- (j %in% tabu.list)
      
      if (tabu.act){
        # El criterio de aspiración
        if (tasa.actual > tasa.best){
          if (tasa.actual > tasa.mejor.vecino){
            tasa.mejor.vecino <- tasa.actual
            tabu.elem <- j
            mejor.vecino <- m
          }
        }
      }
      else{
        if (tasa.actual > tasa.mejor.vecino){
          tasa.mejor.vecino <- tasa.actual
          tabu.elem <- j
          mejor.vecino <- m
        }
      }
    }
    
    if (tasa.mejor.vecino > tasa.best){
      mask.best <- mejor.vecino
      tasa.best <- tasa.mejor.vecino
    }
    
    # Introducimos en la lista tabú el elemento que ha dado lugar
    # a la mejor solución del vecindario anterior
    tabu.list[tl.pos] <- tabu.elem
    tl.pos <- (tl.pos %% max.tabu) + 1
    mask <- mejor.vecino
  }
  mask.best
}

##########################################################################
### Función evaluación calidad algoritmo
###     Para un algoritmo devuelve valores que permiten medir su calidad
###     como algoritmo, haciendo un 5x2 cross validation
###     Tasa clasificación en media para el clasificador 3-knn
###     Tasa reducción de características en media
###     Media del tiempo de ejecución
##########################################################################

cross.eval <- function(algorithm){
  # Semillas aleatorias
  semilla = c(
    12345678,
    23456781,
    34567812,
    45678123,
    56781234
  )
  
  for (j in 1:length(datasets)){
    x <- datasets[[j]]
    num.variables <- length(colnames(x))
    tiempo.exec = c()
    train.tasas = c()
    test.tasas = c()
    tasa.red = c()
    
    cat("Dataset", datasets.names[j])
    
    class.split <- split(x, x$class)
    i <- 1
    
    while (i<=1){
      set.seed(semilla[i])
      partitioned <- lapply(class.split, make.partition, per=0.5 )
      train <- lapply (partitioned, function(x){ x$train } )
      train <- rbindlist(train)
      test <- lapply (partitioned, function(x){ x$test } )
      test <- rbindlist(test)
      
      
      # Primero usando la máscara dada por el train
      tmp <- proc.time()[3]
      mask <- algorithm(train)
      tiempo.exec <- c(tiempo.exec, proc.time()[3] - tmp)
      
      test.tasas <- c(test.tasas, tasa.clas(test,mask)) 
      train.tasas <- c(train.tasas, tasa.clas(train,mask))
      tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
      
      
      # Usando ahora la máscara dada por el test
      tmp <- proc.time()[3]
      mask <- algorithm(test)
      tiempo.exec <- c(tiempo.exec, proc.time()[3] - tmp)
      test.tasas <- c(test.tasas, tasa.clas(train,mask)) 
      train.tasas <- c(train.tasas, tasa.clas(test,mask)) 
      tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
      
      i <- i+1
    }
    cat("\n\tTasas de clasificación:\n")
    cat("\t\t Test: ", test.tasas, "\n\t\t","Train:", train.tasas)
    cat("\n\tTasa de reducción:", tasa.red)
    cat("\n\tTiempo de ejecución(s):", tiempo.exec)
    cat("\n")
  }
}


##########################################################################
### Comparación

# Lista de datasets
datasets <- list(mlibras, arrhythmia, wdbc)
datasets.names <- c("mlibras","arrhythmia","wdbc")
##########################################################################

cross.eval(SFS)
cross.eval(BL)
cross.eval(SA)
cross.eval(BT)