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
#### max.vecinos:   *n, numero maximo de vecinos en cada vecindario
#### max.exitos:    *max.vecinos, numero maximo de exitos por vecindario
#### mu y phi       parametros de la temperatura inicial
####                phi es la probabilidad de coger una solucion un mu
####                por ciento peor que la inicial
ES.coef.max.vecinos <- 10
ES.coef.max.exitos <- 0.1
ES.mu <- 0.3
ES.phi <- 0.3


# Parametros de la Busqueda Tabu
#### max.vecinos:         numero maximo de vecinos en cada vecindario
#### coef.tenencia.tabu:  multiplica a n para dar la tenencia
BT.max.vecinos <- 30
BT.coef.tenencia.tabu <- 1/3


# Parametros de la Busqueda Tabu extendida
#### coef.tenencia.tabu:  multiplica a n para dar la tenencia
#### tope.reinic:         numero maximo de iteraciones sin mejorar
#### max.vecinos:         numero maximo de vecinos en cada vecindario
#### prob.diversif:       probabilidad de diversificacion
#### prob.intensif:       probabilidad de intensificacion
BT.ext.coef.tenencia.tabu <- 1/3
BT.ext.tope.reinic <- 10
BT.ext.max.vecinos <- 30
BT.ext.prob.diversif <- 0.25
BT.ext.prob.intensif <- 0.25


# Parametros de Busqueda Multiarranque Basica
#### num.sols.init        numero de soluciones aleatorias desde las que
####                      partira la busqueda local
BMB.num.sols.init <- 25


# Parametros de GRASP
#### alpha                tolerancia de la lista de candidatos frente al mejor
#### num.sols.init        numero de soluciones aleatorias desde las que
####                      partira la busqueda local
GRASP.alpha <- 0.3
GRASP.num.sols.init <- 25


# Parametros de ILS
#### coef.mutacion        *n, numero de caracteristicas a mutar aleatoriamente
#### num.sols.init        numero de soluciones aleatorias desde las que
####                      partira la busqueda local
ILS.coef.mutacion <- 0.1 
ILS.num.sols.init <- 25



# Parametros de AGs (AGG: generacional, AGE: estacionario)
#### n.crom               numero de cromososmas en cada iteracion
#### prob.cruce           probabilidad de que dos padres crucen
#### prob.mutacion        probabilidad de mutacion de un gen
####
AGG.n.crom <- 30
AGG.prob.cruce <- 0.7
AGG.prob.mutation <- 0.001
AGE.n.crom <- 2
AGE.prob.cruce <- 1
AGE.prob.mutation <- 0.001