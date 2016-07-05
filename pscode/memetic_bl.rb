def memetic_bl (data, prob, flag_best){
    return(
        function(population){
          evs = 0
          # La poblacion está ordenada
          if (flag_best){
              to_apply = {1...prob*size(population)}
              to_apply += (size(population) - size(to_apply))
          }
          else to_apply = which(runif(0, 1, size(population)) <= prob)

          for (i in to_apply){
              result = BL(population[[i]].mask)
              population[[i]].mask = result.mask
              population[[i]].evaluated = False
              evs = evs + result.eval
          }
          return [population, evs]
        }
    )
}
