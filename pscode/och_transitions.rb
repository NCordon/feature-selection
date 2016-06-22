def make_transitions (path, num_car, trail_features){
    if(num_car > 0){
        probabilidades = [for i in {1...num_car} random_unif(0,1)]

        for i in {1...num.car}{
            values = [ for j in 1...length(path)
                        if path[j]==0:
                            # data_heuristic = entropy(data)
                            (trail_features[j]^alpha) * (data_heuristic[j]^beta)
                        else 0
                     ]

            # Regla de la colonia de hormigas
            if (probabilidades[i] < probabilidad_transicion)
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
