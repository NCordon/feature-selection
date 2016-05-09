def OX(xx, xy){
    limites = [random():random()]
    chromosome = yy
    chromosome[limites] <- xy[limites]

    return(chromosome)
}


def selection(pairs){
v =[for p in pairs{
    if(pair.first().fitness < pair.second().fitness)
        yield pair.first()
    else
        yield pair.second()
    ]
}


def crossover(population, prob){
    n_cruces = ceiling( length(population)*prob*0.5 )
    cruces = []
    i=0

    for(j=0; j<n_cruces; j+=1){
        uno = crossover(population[i], population[i+1])
        otro = crossover(population[i+1], population[i])

        cruces.add( [list(mask = uno, fitness=0, evaluated = False])
        cruces.add( [list(mask = otro, fitness=0, evaluated = False])
        j+=2
    }

    population[1:length(cruces)] = cruces
    return(population)
}

def mutate(population, prob){
    # n es el número de predictores del dataset
    M <- length(population)
    mutations <- n*M*prob


    if (random(0.0, 1.0) < mutations){
        mutations <- ceiling(n.mutations)
    }

    # Se escogen los genes que se mutarán junto a sus cromosomas
    crom = [ for i < mutations random[{0,...,M}]
    gen  = [ for i < mutations random[{0,...,n}]
})
