def make_transitions (path, num_car, trail_features){
    if(num_car > 0){
        probabilidades = [for i in {1...num_car} runif()]

        for i in {1...num.car}{
            values = [ for j in 1...length(path)
                        if path[j]==0:
                            (trail_features[i]^alpha) * (data.heuristic^beta)
                        else 0
                     ]

            # Regla de la colonia de hormigas
            if (probabilidades[i] < probabilidad.transition)
                selected = arg_max (values)
            # Regla del sistema de hormigas
            else{
                non_selected = which(values > 0)
                values = values[ non_selected ]
                selected = random(non_selected, prob = values)
            }
                path[selected] = 1
        }
    }
    return path
 }
