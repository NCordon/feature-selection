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
load.my.packages <- function(){
  pkgs = c("foreign", "data.table", "class", "base")
  to.install <- pkgs[ ! pkgs %in% installed.packages() ]
  
  if ( length(to.install) > 0 )
    install.packages( to.install, dependencies = TRUE )
  
  sapply(pkgs, require, character.only=TRUE)
}

load.my.packages()


##########################################################################
### Algoritmos
##########################################################################

source(file="./NN3.r")
source(file="./SFS.r")
source(file="./BL.r")
source(file="./ES.r")
source(file="./BT.r")
source(file="./BText.r")


##########################################################################
### Lectura de datos
##########################################################################

# Semillas aleatorias y parámetros de los algoritmos
source(file="./params.r")


# Fichero con la funcion objetivo, de particionado, de normalizacion...
source(file="./aux.r")


# Lectura de ficheros y normalizacion
mlibras <- normalize (read.arff("../data/movement_libras.arff"))
arrhythmia <- normalize (read.arff("../data/arrhythmia.arff"))
wdbc <- normalize (read.arff("../data/wdbc.arff"))

datasets <- list(wdbc, mlibras, arrhythmia)
datasets.names <- c("wdbc", "mlibras", "arrhythmia")


##########################################################################
### Obtencion de resultados
##########################################################################
data.file <- "seleccion-caracteristicas.RData"
#load(file = data.file)
save.image(file = data.file, safe=TRUE)  

NN3.results <- cross.eval(NN3)
save.image(file = data.file, safe=TRUE)  
SFS.results <- cross.eval(SFS)
save.image(file = data.file, safe=TRUE)  
BL.results <- cross.eval(BL)
save.image(file = data.file, safe=TRUE)  
ES.results <- cross.eval(ES)
save.image(file = data.file, safe=TRUE)  
BT.results <- cross.eval(BT)
save.image(file = data.file, safe=TRUE)  
BT.ext.results <-cross.eval(BT.ext)


#write.csv2(NN3.results,file="NN3.csv", quote=FALSE, row.names = FALSE)