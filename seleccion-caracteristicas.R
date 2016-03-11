# Limpieza del entorno
rm(list = ls())


###############################################
# Lista de paquetes a cargar

  # foreign: leer ARFF 
  # caret: particionado
################################################

pkgs = list("foreign","ggplot2")
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



SFS <- function(data){
  
   #Por implementar  
  
  
  
}