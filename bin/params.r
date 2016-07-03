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



# Parametros de AGs (AGG: generacional, AGE: estacionario) y meméticos
#### n.crom               numero de cromososmas en cada iteracion
#### prob.cruce           probabilidad de que dos padres crucen
#### prob.mutacion        probabilidad de mutacion de un gen
#### prob.bl              profundidad a aplicar en la búsqueda local
AG.n.crom <- 30
AGG.prob.cruce <- 0.7
AGG.prob.mutation <- 0.001
AGE.prob.cruce <- 1
AM.prof.bl <- 1

# Parametros de las OCH  
#### num.ants             numero de hormigas
#### alpha                parametro de la regla de transicion
#### beta                 parametro de la regla de transicion
#### evaporation          parametro de evaporacion global, en tanto por uno
#### prob.trans           probabilidad de efectuar transicion
#### local.evap           parametro de evaporacion local
#### prof.bl              profundidad a aplicar en la busqueda local
OCH.num.ants <- 10
OCH.alpha <- 1
OCH.beta <- 2
OCH.global.evap <- 0.2
OCH.prob.trans <- 0.8
OCH.local.evap <- 0.2
OCH.prob.bl <- 1