def memetic(data, crossover, bl, gen_memetic){
    n = num_variables(data)
    n_eval = 0
    n_generations = 0

    while(n_eval < max_eval){
        if(n_generations % gen_memetic == 0 and num_generations > 0){
            result = bl(population)
            population = result.population
            evs_bl = result.evs
        }

        old_best = population[ arg max population.fitness ]
        population = selection(10)
        population = crossover(population, p_cruce)
        population = mutate(population, p_mutation)

        n_eval += count (which(not population.evaluated)) + evs_bl
        population = reeval(population)

        population = keep_elitism (population, old.best)
        n_generations++
    }
    return (population[ arg max population.fitness ].mask)
}
