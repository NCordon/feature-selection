# Parametros para seleccion-caracteristicas.R

semilla <- c(
  12345678
  ,23456781
  ,34567812
  ,45678123
  ,56781234
)

# Maximo de iteraciones de las metaheurísticas
max.eval <- 15000

# Parametros del Enfriamiento Simulado
ES.coef.max.vecinos <- 10
ES.coef.max.exitos <- 0.1
ES.mu <- 0.3
ES.phi <- 0.3


# Parametros de la Busqueda Tabu
BT.max.vecinos <- 30
BT.coef.tenencia.tabu <- 1/3

# Parametros de la Busqueda Tabu extendida
#### coef.tenencia.tabu:  multiplica a n para dar la tenencia
#### tope.reinic:         numero maximo de iteraciones sin mejorar
#### prob.diversif:       probabilidad de diversificacion
#### prob.intensif:       probabilidad de intensificacion
BT.ext.coef.tenencia.tabu <- 1/3
BT.ext.tope.reinic <- 10
BT.ext.max.vecinos <- 30
BT.ext.prob.diversif <- 0.25
BT.ext.prob.intensif <- 0.25