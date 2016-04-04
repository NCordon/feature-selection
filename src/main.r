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
# Lectura de datos
##########################################################################

mlibras <- read.arff("../data/movement_libras.arff")
arrhythmia <- read.arff("../data/arrhythmia.arff")
wdbc <- read.arff("../data/wdbc.arff")


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
# Función de generación de particiones
##########################################################################
make.partition <- function(data,per){
  rows <- sample(1:nrow(data), nrow(data)*per)
  
  list(train = data[rows,], test = data[-rows,])
}


##########################################################################
### Función tasa clasificación
##########################################################################

tasa.clas <- function (train, mask){
  # Aplicamos la máscara
  cl <- train$class
  train <- subset(train, select = which(mask==1))
  
  # Obtenemos el fit que se haría del conjunto test para el 3-knn
  fit <- knn.cv(train, cl, k=3, use.all = TRUE)
  
  # Tasa de clasificación
  return (100 * length(which(cl == fit)) / length(cl))
}


#########################################################################
### Función evaluación calidad algoritmo
###     Para un algoritmo devuelve valores que permiten medir su calidad
###     como algoritmo, haciendo un 5x2 cross validation
###     Tasa clasificación en media para el clasificador 3-knn
###     Tasa reducción de características en media
###     Media del tiempo de ejecución
##########################################################################

cross.eval <- function(algorithm){
  n.eval <- length(semilla)
  all.results <- list()
  mean.results <- list()
  
  for (j in 1:length(datasets)){
    x <- datasets[[j]]
    num.variables <- length(colnames(x))
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
### Obtención de resultados
##########################################################################
# Semillas aleatorias

source(file="./params.r")


# Lista de datasets

datasets <- list(wdbc, mlibras, arrhythmia)
datasets.names <- c("wdbc", "mlibras", "arrhythmia")

##########################################################################
# Archivo donde guardamos el entorno guardado hasta el momento
data.file <- "seleccion-caracteristicas.RData"

save.my.image <- function(){
  save.image(file = data.file, safe=TRUE)  
}
#load(file = data.file)

# Después de ejecutar cada algoritmo, guardamos la imagen para poderla recuperar

NN3.results <- cross.eval(NN3)
save.my.image()

SFS.results <- cross.eval(SFS)
save.my.image()

BL.results <- cross.eval(BL)
save.my.image()

ES.results <- cross.eval(ES)
save.my.image()

BT.results <- cross.eval(BT)
save.my.image()

BT.ext.results <-cross.eval(BT.ext)
save.my.image()

#write.csv2(NN3.results$arrhythmia,file="arrhythmia.csv", quote=FALSE, row.names = FALSE)