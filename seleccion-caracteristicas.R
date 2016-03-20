# Limpieza del entorno
rm(list = ls())


##########################################################################
# Lista de paquetes a cargar

  # foreign: leer ARFF 
  # data.table: rbindinglist
  # class: kknn
##########################################################################
# Cargamos pkgs

pkgs = c("foreign", "data.table", "class")
to.install <- pkgs[ ! pkgs %in% installed.packages() ]

if ( length(to.install) > 0 )
  install.packages( to.install, dependencies = TRUE )
  
lapply(pkgs, require, character.only=TRUE)


##########################################################################
# Semillas aleatorias
semilla = c(
  12345678,
  23456781,
  34567812,
  45678123,
  56781234
)


##########################################################################
# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


# Función de normalización de datasets
normalize <- function(data){
  colnames(data) <- tolower(colnames(data))
  names <- colnames(data)[ colnames(data) != "class" ]
  
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

# Normalizamos los nombres de los atributos
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
### Bucle principal
##########################################################################
# Lista de datasets
datasets = list(mlibras, arrhythmia, wdbc)
datasets.names = c("mlibras","arrhythmia","wdbc")
##########################################################################


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
  
  while (i<=5){
    set.seed(semilla[i])
    partitioned <- lapply(class.split, make.partition, per=0.5 )
    train <- lapply (partitioned, function(x){ x$train } )
    train <- rbindlist(train)
    test <- lapply (partitioned, function(x){ x$test } )
    test <- rbindlist(test)
    
    
    # Primero usando la máscara dada por el train
    tmp <- proc.time()[3]
    mask <- SFS(train)
    tiempo.exec <- c(tiempo.exec, proc.time()[3] - tmp)
    
    test.tasas <- c(test.tasas, tasa.clas(test,mask)) 
    train.tasas <- c(train.tasas, tasa.clas(train,mask))
    tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
    
    
    # Usando ahora la máscara dada por el test
    tmp <- proc.time()[3]
    mask <- SFS(test)
    tiempo.exec <- c(tiempo.exec, proc.time()[3] - tmp)
    test.tasas <- c(test.tasas, tasa.clas(train,mask)) 
    train.tasas <- c(train.tasas, tasa.clas(test,mask)) 
    tasa.red <- c(tasa.red, (num.variables - sum(mask==1))/num.variables)
    
    i <- i+1
  }
  cat("\n\tTasas de clasificación:\n")
  cat("\t\tTest:", mean(test.tasas), "\t","Train:", mean(train.tasas))
  cat("\n\tTasa de reducción:", mean(tasa.red))
  cat("\n\tTiempo de ejecución:", mean(tiempo.exec))
  cat("\n")
}
