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
