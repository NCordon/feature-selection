def selection(num_crom)
    pairs = [ for i in {1,..,size_generation}
            (random({0,...,num_crom}), random({0,...,num_crom})) ]

    v =[ for p in pairs{
        if(population[p.first()].fitness < population[p.second()].fitness)
            yield population[p.first()]
        else
            yield population[p.second()] ]
}
