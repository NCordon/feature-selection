def AGG(data){
    n = num_col(data)
    n_eval = 0

    while(n_eval < max_eval){
        old_best = population[ arg max population.fitness ]

        population = selection(30)
        population = crossover(population, p_cruce)
        population = mutate(population, p_mutation)

        n_eval += count (which(not population.evaluated))
        population = reeval(population)

        population = keep_elitism (population, old.best)
    }
    return (population[ arg max population.fitness ].mask)
}
