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

if ( length(to.install) > 0 )
  install.packages( to.install, dependencies = TRUE )
  
lapply(pkgs, require, character.only=TRUE)
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
            
            list(train = data[rows,], test = data[-rows,])
          }
          
          mclases.partitioned <- lapply(mclases, make.partition, per=0.5 )
          mclases.train <- lapply (mclases.partitioned, function(x){ x$train } )
          mclases.train <- rbindlist(mclases.train)
          mclases.test <- lapply (mclases.partitioned, function(x){ x$test } )
          mclases.test <- rbindlist(mclases.test)

          tasa.clas(mclases.train, mclases.test, c(1,0,1,0,1))
##########################################################################

          
tasa.clas <- function (train, test, mask){
  # Aplicamos la máscara
  test <- subset(test, select = c(which(mask==1), ncol(test)))
  train <- subset(train, select = c(which(mask==1), ncol(train)))
  
  # Obtenemos el fit que se haría del conjunto test para el 3-knn
  knn.clas <- kknn (class~., train, test, distance=2, class, k=3)
  fit <- fitted(knn.clas)
  
  # Tasa de clasificación
  return (100 * length(which(test$class == fit)) / length(test$class))
}


SFS <- function(col.number, train, test){
  n <- col.number
  mask <- rep(0,n)
  non.selected <- seq(1,n)
  max <- 0
  
  repeat{
    evs <- lapply (non.selected, function(x){
      m <- mask
      m[x] <- 1
      tasa.clas(train, test, m)
    } )
    
    if (max(evs) < max){
      break
    }
    
    mask [which.max (evs)] <- 1
    non.selected <- non.selected[non.selected != which.max(evs)]
    max <- max(evs)
  }
  
  return mask
}