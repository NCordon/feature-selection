def OX(xx, xy){
    k = length(xx)
    limites = [random(1,k):random(1,k)]
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
    i=0

    for(j=0; j<n_cruces; j+=1){
        population[i].mask = crossover(population[i], population[i+1])
        population[i+1].mask = crossover(population[i+1], population[i])

        population[i].evaluated = False
        population[i+1].evaluated = False
    }

    return(population)
}

def mutate(population, prob){
    # n es el número de predictores del dataset
    M <- length(population)
    mutations <- n*M*prob

    # Esto impide que mutemos todas las soluciones en el estacionario
    if (random(0.0, 1.0) < mutations){
        mutations <- ceil(n.mutations)
    }

    # Se escogen los genes que se mutarán junto a sus cromosomas
    crom = [ for i < mutations random[{0,...,M}]
    gen  = [ for i < mutations random[{0,...,n}]

    for i in {1...len(crom)}{
        flip(population[crom[i]].mask, gen[i])
        population[crom[i]].evaluated = False
    }

    return(population)
})

def keep_elitism(population,old_best){
    if old_best not in population{
        k = arg min{population[0].fitness,...population[M-1].fitness}

        if population[k].fitness < old_best.fitness{
            population[k] = old_best
        }
    }

    return(population)
}

def reeval(population){
    for c in population{
        if c.evaluated == False{
            c.fitness = tasa(c.mask)
        }
    }
    return(population)
}
