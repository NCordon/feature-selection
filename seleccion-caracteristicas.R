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

sapply(pkgs, require, character.only=TRUE)


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
      (x-min(x))/(max(x)-min(x))
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
  fit <- knn.cv(train, cl, k = 3, prob = FALSE, use.all = TRUE)
  
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
  fin <- FALSE
  
  while(!fin){
    evs <- sapply (non.selected, function(x){
      m <- mask
      m[x] <- 1
      tasa.clas(data, m)
    } )
    
    if (max(evs) < max || length(evs)==0){
      fin <- TRUE
    }
    
    sel <- non.selected[which.max (evs)]
    mask [sel] <- 1
    non.selected <- non.selected[non.selected != sel]
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
  tasa.best <- tasa.clas(data, mask)
  mejora.found <- TRUE
  max.eval <- 15000
  n.eval <- 0
  
  while(mejora.found && (n.eval < max.eval)){
    non.selected <- seq(1,n)
    vecinos.left <- TRUE
    mejora.found <- FALSE
    
    while(!mejora.found && vecinos.left && (n.eval < max.eval)){
      # Generamos un vecino hasta que mejoremos
      # o hasta que agotemos el vecindario
      
      m <- mask
      j <- sample(non.selected, 1)
      non.selected <- non.selected[non.selected!=j]
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      n.eval <- n.eval + 1
      
      if (tasa.actual >= tasa.best){
        mask <- m
        tasa.best <- tasa.actual
        mejora.found <- TRUE
      }
      else{
        if (length(non.selected) == 0){
          vecinos.left <- FALSE
        }
      }
    }
  }
  mask
}

##########################################################################
### Función enfriamiento simulado
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de características que se obtienen de aplicar enfriamiento simulado
###
##########################################################################

ES <- function(data){
  # Solución greedy inicial
  mask <- SFS(data)
  n <- length(mask)
  tasa.best <- tasa.clas(data, mask)
  mask.best <- mask
  
  # Parámetros del enfriamiento simulado
  max.eval <- 15000
  max.vecinos <- 10*n
  max.exitos <- 0.1*max.vecinos
  mu <- 0.3
  phi <- 0.3
  # Nota: los mejores valores probados para estos parámetros han sido 0.05 y 0.05
  
  
  t.actual <- mu*tasa.best/-log(phi, base=exp(1))
  t.final <- 1e-3
  
  # Comprobamos que la temperatura final sea menor que la inicial
  # y la ajustamos en caso contrario
  while (t.final >= t.actual){
    t.final <- t.final * 1e-3
  }
  
  beta <- (t.actual - t.final)/((max.eval/max.vecinos)*t.actual*t.final)
  
  n.eval <- 0
  n.vecinos <- 0
  n.exitos <- 1
  
  # Nueva iteración
  # print ("Nueva iteración")
  
  while(n.eval < max.eval & n.exitos>0 & t.actual > t.final){
    n.vecinos <- 0
    n.exitos <- 0
    
    # Un enfriamiento
    while(n.vecinos < max.vecinos
          & n.exitos < max.exitos
          & n.eval < max.eval){
      
      m <- mask
      
      # Generamos un vecino
      j <- sample(1:n,1)
      m[j] <- (m[j]+1)%%2
      
      tasa.actual <- tasa.clas(data, m)
      delta <- tasa.actual - tasa.best
      u <- runif(1, 0.0, 1.0)
      
      if (delta >= 0 || u <= exp(delta/t.actual)){
        mask <- m
        tasa.best <- tasa.actual
        n.exitos <- n.exitos + 1
        
        if (delta >=0){
          mask.best <- m
        }
        
      }
      
      n.vecinos <- n.vecinos + 1
      n.eval <- n.eval + 1
    }
    t.actual <- t.actual/(1 + beta*t.actual)
    # Depuración
    #cat("\n Temperatura actual: ", t.actual)
    #cat("\n Número de éxitos: ", n.exitos)
    #cat("\n Número de vecinos generados ", n.vecinos)
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
    tasa.mejor.vecino <- 0
    pos.vecinos <- sample(1:n, min(c(max.vecinos, max.eval-n.eval)))
    
    evs <- sapply(pos.vecinos, function(j){
      m <- mask
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      
      if (j %in% tabu.list){
        # Si el criterio de aspiración no se cumple
        #   Asignamos un valor basura a la tasa para
        #   que no sea escogida como la mejor
        if (tasa.actual <= tasa.best){
          tasa.actual <- 0
        }
      }
      
      tasa.actual
    })
    
    j <- which.max(evs)
    tasa.mejor.vecino <- evs[j]
    tabu.elem <- pos.vecinos[j]
    # Esto asigna a la solución actual el mejor vecino
    mask[tabu.elem] <- (mask[tabu.elem]+1)%%2
    
    if (tasa.mejor.vecino > tasa.best){
      mask.best <- mask
      tasa.best <- tasa.mejor.vecino
    }
    
    # Introducimos en la lista tabú el elemento que ha dado lugar
    # a la mejor solución del vecindario anterior
    tabu.list[tl.pos] <- tabu.elem
    tl.pos <- (tl.pos %% max.tabu) + 1
    
    # Actualizamos el número de evaluaciones
    n.eval <- n.eval + length(pos.vecinos)
  }
  mask.best
}

BT.ext <- function(data){
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
  # Lista de frecuencias
  frec <- rep(0,n)
  n.reinic <- 20
  n.sin.mejora <- 0
  
  # Posición a escribir de la lista tabú
  tl.pos <- 1
  n.eval <- 0
  
  while(n.eval < max.eval){
    # Reinicialización
    if ((n.sin.mejora %% n.reinic == 0)){
      u <- runif(1, 0.0, 1.0)
      
      if (u < 0.25){
        mask <- sample(0:1, n, replace=TRUE)
      }
      else if (u < 0.5){
        mask <- mask.best
      }
      else{
        n.soluciones <- sum(frec)
        
        mask <- sapply(frec, function(f,n){
          u <- runif(1, 0.0, 1.0)
          x <- 0
          # Evita que falle en las primeras iteraciones
          n <- max(n,1)
          
          if (u < 1 - f/n){
            x <- 1
          }
          x
        }, n = n.soluciones)
      }
      # Cambio de tamaño de la lista tabú
      u <- runif(1, 0.0, 1.0)
      if (u < 0.5){
        # Aumenta de tamaño en un 50%
        max.tabu <- ceiling(max.tabu*1.5)
      }
      else{
        # Disminuye en un 50%
        max.tabu <- ceiling(max.tabu*0.5)
      }
      
      tl.pos <- min(tl.pos, max.tabu)
      n.sin.mejora <- 0
    }
    
    tasa.mejor.vecino <- 0
    pos.vecinos <- sample(1:n, min(c(max.vecinos, max.eval-n.eval)))
    
    evs <- sapply(pos.vecinos, function(j){
      m <- mask
      
      m[j] <- (m[j]+1)%%2
      tasa.actual <- tasa.clas(data, m)
      
      if (j %in% tabu.list){
        # Si el criterio de aspiración no se cumple
        #   Asignamos un valor basura a la tasa para
        #   que no sea escogida como la mejor
        if (tasa.actual <= tasa.best){
          tasa.actual <- 0
        }
      }
      
      tasa.actual
    })
    
    j <- which.max(evs)
    tasa.mejor.vecino <- evs[j]
    tabu.elem <- pos.vecinos[j]
    # Esto asigna a la solución actual el mejor vecino
    mask[tabu.elem] <- (mask[tabu.elem]+1)%%2
    
    if (tasa.mejor.vecino > tasa.best){
      mask.best <- mask
      tasa.best <- tasa.mejor.vecino
      frec <- frec + mask
      n.sin.mejora <- 0
    }
    else{
      n.sin.mejora <- n.sin.mejora + 1
    }
    
    # Introducimos en la lista tabú el elemento que ha dado lugar
    # a la mejor solución del vecindario anterior
    tabu.list[tl.pos] <- tabu.elem
    tl.pos <- (tl.pos %% max.tabu) + 1
    
    # Actualizamos el número de evaluaciones
    n.eval <- n.eval + length(pos.vecinos)
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
  semilla <- c(
     12345678
    ,23456781
    ,34567812
    ,45678123
    ,56781234
  )

  n.eval <- length(semilla)
  
  for (j in 1:length(datasets)){
    x <- datasets[[j]]
    num.variables <- length(colnames(x))
    tiempo.exec = c()
    train.tasas = c()
    test.tasas = c()
    tasa.red = c()
    
    cat("\nDataset", datasets.names[j], "\n")
    
    class.split <- split(x, x$class)
    i <- 1
    
    while (i <= n.eval){
      set.seed(semilla[i])
      partitioned <- lapply(class.split, make.partition, per=0.5 )
      train <- lapply (partitioned, function(x){ x$train } )
      train <- rbindlist(train)
      test <- lapply (partitioned, function(x){ x$test } )
      test <- rbindlist(test)
      
      
      # Primero usando la máscara dada por el train
      t.inicial <- proc.time()[3]
      mask <- algorithm(train)
      t.final <- proc.time()[3]
      
      tiempo.exec <- c(tiempo.exec, t.final - t.inicial)
      test.tasas <- c(test.tasas, tasa.clas(test,mask))
      train.tasas <- c(train.tasas, tasa.clas(train,mask))
      tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
      
      
      # Usando ahora la máscara dada por el test
      t.inicial <- proc.time()[3]
      mask <- algorithm(test)
      t.final <- proc.time()[3]
      
      tiempo.exec <- c(tiempo.exec, t.final - t.inicial)
      test.tasas <- c(test.tasas, tasa.clas(train,mask))
      train.tasas <- c(train.tasas, tasa.clas(test,mask))
      tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
      
      i <- i+1
    }
    with.decimals <- function(v){ format(v, nsmall=5) }
    result <- data.frame(with.decimals(test.tasas),
                         with.decimals(tasa.red),
                         with.decimals(tiempo.exec),
                         with.decimals(train.tasas))
    
    result.medias <- data.frame(with.decimals(mean(test.tasas)),
                                with.decimals(mean(tasa.red)),
                                with.decimals(mean(tiempo.exec)),
                                with.decimals(mean(train.tasas)))
    
    names.result <- c("Tasa.test", "Tasa.red", "T.exec", "Tasa.train")
    colnames(result) <- names.result
    colnames(result.medias) <- names.result    
    
    print(result)
    cat("\tMedias resultados\n")
    print(result.medias)
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
cross.eval(ES)
#cross.eval(BT)
#cross.eval(BT.ext)