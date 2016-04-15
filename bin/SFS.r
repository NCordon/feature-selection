##########################################################################
### Funcion SFS
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que maximizan la tasa de clasificacion de ese
###     conjunto usando leaving one out en el knn euclideo
##########################################################################

SFS <- function(data){
  n <- ncol(data)
  n <- n-1
  mask <- rep(0,n)
  non.selected <- seq(1,n)
  max <- 0
  fin <- FALSE
  
  while(!fin){
    evs <- sapply (non.selected, function(x){
      m <- mask
      m[x] <- 1
      tasa.clas(data, m)
    } )
    
    if (max(evs) <= max || length(evs)==0){
      fin <- TRUE
    }
    else{
      sel <- non.selected[which.max (evs)]
      mask [sel] <- 1
      non.selected <- non.selected[non.selected != sel]
      max <- max(evs)
    }
  }
  
  mask
}
