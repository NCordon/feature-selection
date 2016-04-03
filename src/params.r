# Parámetros para seleccion-caracteristicas.R

semilla <- c(
  12345678
  ,23456781
  ,34567812
  ,45678123
  ,56781234
)

# Máximo de iteraciones de las metaheurísticas
max.eval <- 15000

# Parámetros del Enfriamiento Simulado
ES.coef.max.vecinos <- 10
ES.coef.max.exitos <- 0.1
ES.mu <- 0.3
ES.phi <- 0.3


# Parámetros de la Búsqueda Tabú
BT.max.vecinos <- 30
BT.coef.max.tabu <- 1/3

# Parámetros de la Búsqueda Tabú extendida
BT.ext.tope.reinic <- 10
BT.ext.coef.frec <- 1
BT.ext.max.vecinos <- 30
BT.ext.prob.diversif <- 0.25
BT.ext.prob.intensif <- 0.25