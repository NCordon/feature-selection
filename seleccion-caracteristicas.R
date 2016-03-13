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

Filter(function(x){ length(unique(x))>1 }, data)

# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


normalize <- function(data){
  colnames(data) <- tolower(colnames(data))
  data <- data[,c(colnames(data) [colnames(data) != "class"], "class")]
  #Filter(function(x){ length(unique(x))>1 }, data)
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
# Lista de datasets
datasets = list(mlibras, arrhythmia, wdbc)
##########################################################################

##########################################################################
# Función de generación de particiones
make.partition <- function(data,per){
  rows <- sample(1:nrow(data), nrow(data)*per) 
  
  list(train = data[rows,], test = data[-rows,])
}

##########################################################################


# Semilla aleatoria
set.seed(1)

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
    
    if (max(evs) < max){
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


data.clases <- split(mlibras, mlibras$class)
i <- 1

while (i<=5){
  partitioned <- lapply(data.clases, make.partition, per=0.5 )
  train <- lapply (partitioned, function(x){ x$train } )
  train <- rbindlist(train)
  test <- lapply (partitioned, function(x){ x$test } )
  test <- rbindlist(test)
  
  # Primero usando la máscara dada por el train
  mask <- SFS(train)
  print(c( tasa.clas(test,mask), tasa.clas(train,mask)) )
  
  # Usando ahora la máscara dada por el test
  mask <- SFS(test)
  print(c( tasa.clas(test,mask), tasa.clas(train,mask)) )
  
  i <- i+1
}