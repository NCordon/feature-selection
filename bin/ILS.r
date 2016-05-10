##########################################################################
### Funcion Busqueda Local Reiterada (ILS)
###     Lanza la Busqueda local un numero determinado de veces
###     para una solucion aleatoria generada la primera vez, y las 
###     sucesivas veces sobre una solucion mutada
##########################################################################

ILS <- function(data){
  n <- ncol(data)
  n <- n-1
  n.a.mutar <- ILS.coef.mutacion*n
  max.arranques <- ILS.num.sols.init
  
  mask <- random.init(data)
  mask.best <- mask
  tasa.best <- tasa.clas(data, mask.best)
  
  for(n.eval in 1:max.arranques){
    # Aplicamos busqueda local sobre la solucion
    mask <- BL(data, function(x){ mask })
    tasa.mask <- tasa.clas(data, mask)
    
    if (tasa.mask > tasa.best){
      mask.best <- mask
      tasa.best <- tasa.mask
    }
    
    # Mutacion sobre el mejor de entre la mascara de
    # la iteracion actual y el mejor hasta el momento
    a.mutar <- sample(1:n, n.a.mutar)
    mask <- mask.best
    mask[a.mutar] <- !mask[a.mutar]
  }
  
  mask.best
}