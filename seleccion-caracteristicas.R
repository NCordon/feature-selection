# Limpieza del entorno
rm(list = ls())

library(foreign)

mov_libras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")

