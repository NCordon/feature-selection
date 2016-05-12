def selection(num_crom)
    pairs = [(random({0,...,num_crom}), random({0,...,num_crom}))
             for i in {1,..,size_generation}]

    v =[ for p in pairs{
        if(population[p.first()].fitness > population[p.second()].fitness)
            yield population[p.first()]
        else
            yield population[p.second()] ]
}
