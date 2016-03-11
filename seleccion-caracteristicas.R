# Limpieza del entorno
rm(list = ls())

# Paquete para leer los ARFF
library(foreign)


# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


# Normalizamos los nombres de los atributos
colnames(mlibras) <- tolower(colnames(mlibras))
colnames(arrhythmia) <- tolower(colnames(arrhythmia))
colnames(wdbc) <- tolower(colnames(wdbc))
