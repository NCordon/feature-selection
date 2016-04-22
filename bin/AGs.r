##########################################################################
### Generador de soluciones iniciales aleatorias en selec. caracteristicas
###     Para un tamaño determinado devuelve una seleccion de 
###     caracteristicas aleatoria
##########################################################################

random.init <- function(data){ sample(0:1, ncol(data)-1, replace=TRUE) }


##########################################################################
### Funcion AGG (algoritmo genetico generacional)
###     Para un data frame devuelve para el clasificador 3-knn el conjunto
###     de caracteristicas que se obtienen de aplicar el algoritmo genetico
###     generacional
###
##########################################################################
population <- lapply(1:5, function(i){
  mask <- c(1,1)
  
  list(mask = mask, tasa = 38)
})
population[c(1,3)]$tasa

AGG <- function(data){
  n <- ncol(data)
  n <- n-1
  n.eval <- 0
  n.crom <- AGG.n.crom
  prob.cruce <- AGG.prob.cruce
  
  population <- lapply(n.crom, function(i){
    mask <- gen.init(data)
    mask.tasa <- tasa.clas(mask)
    list(mask = mask, tasa = mask.tasa)
  })
  
  while(n.eval < max.eval){
    # Tomamos los indices de los padres a los que se le aplicara torneo
    mother <- sample(1:n.crom, ncrom)
    father <- sample(1:n.crom, ncrom)
    parents <- Map(c,mother,father)     
    
    population.new <- lapply(parents, function(p){
      
    })
    
  }
  mask
}