# Limpieza del entorno
rm(list = ls())


###############################################
# Lista de paquetes a cargar

  # foreign: leer ARFF 
  # caret: particionado
################################################

pkgs = list("foreign")
to.install <- pkgs[ ! pkgs %in% installed.packages() ]
install.packages( to.install, dependencies = TRUE )

# Cargamos pkgs
lapply(pkgs,require,character.only=TRUE)





# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


# Normalizamos los nombres de los atributos
colnames(mlibras) <- tolower(colnames(mlibras))
colnames(arrhythmia) <- tolower(colnames(arrhythmia))
colnames(wdbc) <- tolower(colnames(wdbc))


# d = list(mlibras, arrhythmia, wdbc)


# Para cada dataset
# Desde 1 hasta 5
#   Hacemos una partición estratificada
#
#   Para {train, prueba}
#     Hacemos greedy de selección de características
#     Evaluamos tasa de acierto en el contrario
#
#   Hacemos media

n <- nrow(mlibras)
mclases <- split(mlibras, mlibras$class)



set.seed(1)

make.partition <- function(data,per){
  rows <- sample(1:nrow(data), nrow(data)*per) 
  
  list(training = data[rows,], test = data[-rows,])
}

mclases.partitioned <- lapply(mclases, make.partition, per=0.5 )





SFS <- function(data){
  
   #Por implementar  
  
  
  
}