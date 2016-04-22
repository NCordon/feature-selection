##########################################################################
# Funcion de normalizacion de datasets
##########################################################################

normalize <- function(data){
  colnames(data) <- tolower(colnames(data))
  names <- colnames(data)[ colnames(data) != "class" ]
  
  # La ultima columna es la de clase, y quitamos columnas con un unico valor
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
    train <- subset(train, select = which(mask==1))
    
    # Obtenemos el fit que se haria del conjunto test para el 3-knn
    fit <- knn.cv(train, cl, k=3, use.all = TRUE)
    
    # Tasa de clasificacion
    result <- (100 * length(which(cl == fit)) / length(cl))
  }
  else{
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
  
  for (j in 1:length(datasets)){
    x <- datasets[[j]]
    num.variables <- length(colnames(x))-1
    tiempo.exec = c()
    train.tasas = c()
    test.tasas = c()
    tasa.red = c()
    
    cat("Procesando dataset", datasets.names[j], "\n")
    
    class.split <- split(x, x$class)
    i <- 1
    
    while (i <= n.eval){
      set.seed(semilla[i])
      partitioned <- lapply(class.split, make.partition, per=0.5 )
      train <- lapply (partitioned, function(x){ x$train } )
      train <- rbindlist(train)
      test <- lapply (partitioned, function(x){ x$test } )
      test <- rbindlist(test)
      
      
      # Primero usando la mascara dada por el train
      t.inicial <- proc.time()[3]
      mask <- algorithm(train)
      t.final <- proc.time()[3]
      
      tiempo.exec <- c(tiempo.exec, t.final - t.inicial)
      test.tasas <- c(test.tasas, tasa.clas(test,mask))
      train.tasas <- c(train.tasas, tasa.clas(train,mask))
      tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
      
      
      # Usando ahora la mascara dada por el test
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
    result <- data.frame(
      with.decimals(test.tasas),
      with.decimals(train.tasas),
      with.decimals(tasa.red),
      with.decimals(tiempo.exec)
    )
    
    result.medias <- data.frame(
      with.decimals(mean(test.tasas)),
      with.decimals(mean(train.tasas)),
      with.decimals(mean(tasa.red)),
      with.decimals(mean(tiempo.exec))
    )
    
    names.result <- c("Tasa.test", "Tasa.train", "Tasa.red", "T.exec")
    colnames(result) <- names.result
    colnames(result.medias) <- names.result    
    
    all.results[[j]] <- result
    mean.results[[j]] <- result.medias
  }
  names(all.results) <- datasets.names
  names(mean.results) <- paste(datasets.names, ".media", sep="")
  append(all.results, mean.results)
} 
