# Limpieza del entorno
rm(list = ls())


###############################################
# Lista de paquetes a cargar

  # foreign: leer ARFF 
  # data.table: rbindinglist
  # class: kknn
################################################
# Cargamos pkgs

pkgs = c("foreign", "data.table", "kknn")
to.install <- pkgs[ ! pkgs %in% installed.packages() ]

install.packages( to.install, dependencies = TRUE )
lapply(pkgs,require,character.only=TRUE)
################################################




# Lectura de datos
mlibras <- read.arff("movement_libras.arff")
arrhythmia <- read.arff("arrhythmia.arff")
wdbc <- read.arff("wdbc.arff")


# Normalizamos los nombres de los atributos
colnames(mlibras) <- tolower(colnames(mlibras))
colnames(arrhythmia) <- tolower(colnames(arrhythmia))
colnames(wdbc) <- tolower(colnames(wdbc))




datasets = list(mlibras, arrhythmia, wdbc)
##########################################################################
### Ejemplo hecho para un dataset, falta hacerlo para la lista de datasets
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

##########################################################################

          
tasa.clas <- function (train, test, mask){
  # Aplicamos la máscara
  test <- subset(test, select = c(which(mask==1), colnum(test)))
  train <- subset(train, select = c(which(mask==1), colnum(train)))
  
  knn.clas <- kknn (class~., train, test, distance=2, class, k=3, use.all = FALSE)
  fit <- fitted(knn.clas)
  
  length(which(test$class != fit))
  
  # Por terminar...
}

SFS <- function(data){
  
   #Por implementar  
  
  
  
}