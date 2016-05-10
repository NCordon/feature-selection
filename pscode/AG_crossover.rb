def OX(xx, xy){
    k = length(xx)
    limites = [random(1,k):random(1,k)]
    chromosome = yy
    chromosome[limites] <- xy[limites]

    return(chromosome)
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
