# Limpieza del entorno
rm(list = ls())


###############################################
# Lista de paquetes a cargar

  # foreign: leer ARFF 
  # caret: particionado
################################################

pkgs = c("foreign","data.table")
to.install <- pkgs[ ! pkgs %in% installed.packages() ]
install.packages( to.install, dependencies = TRUE )

# Cargamos pkgs
lapply(pkgs,require,character.only=TRUE)


is.element("data.table", installed.packages())
library(data.table)
# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


# Normalizamos los nombres de los atributos
colnames(mlibras) <- tolower(colnames(mlibras))
colnames(arrhythmia) <- tolower(colnames(arrhythmia))
colnames(wdbc) <- tolower(colnames(wdbc))




datasets = list(mlibras, arrhythmia, wdbc)


# Semilla aleatoria
set.seed(1)

n <- nrow(mlibras)
mclases <- split(mlibras, mlibras$class)

make.partition <- function(data,per){
  rows <- sample(1:nrow(data), nrow(data)*per) 
  
  list(training = data[rows,], test = data[-rows,])
}

mclases.partitioned <- lapply(mclases, make.partition, per=0.5 )
mclases.training <- lapply (mclases.partitioned, function(x){ x$training } )
mclases.training <- rbindlist(mclases.training)
mclases.test <- lapply (mclases.partitioned, function(x){ x$test } )
mclases.test <- rbindlist(mclases.test)

SFS <- function(data){
  
   #Por implementar  
  
  
  
}