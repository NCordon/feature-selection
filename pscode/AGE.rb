def AGE(data){
    n = num_col(data)
    n_eval = 0

    while(n_eval < max_eval){
        new_generation = selection(2)
        new_generation = crossover(new_generation, p_cruce)
        new_generation = mutate(new_generation, p_mutacion)

        n_eval += count (which(not population.evaluated))
        new_generation = reeval(new_generation)


        population = keep_elitism (population, best(new_generation))
        population = keep_elitism (population, worst(new_generation))
    }
    return (population[ arg max population.fitness ].mask)
}
