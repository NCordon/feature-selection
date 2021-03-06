def mutate(population, prob){
    # n es el número de predictores del dataset
    M = length(population)
    mutations = n*M*prob

    # Esto impide que mutemos siempre en el estacionario
    if (random(0.0, 1.0) < mutations){
        mutations = ceil(mutations)

        # Se escogen los genes que se mutarán junto a sus cromosomas
        crom = [ for i < mutations random[{0,...,M}]
        gen  = [ for i < mutations random[{0,...,n}]

        for i in {0...len(crom)}{
            flip(population[crom[i]].mask, gen[i])
            population[crom[i]].evaluated = False
        }
    }

    return(population)
})
