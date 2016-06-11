
def SCH_BL(data){
    data_heuristic = entropy(data)

    while(n_eval < max_eval){
        init_iteration(data)

        for k in {1...num.ants}{
            paths[k] = make.transitions(paths[[k]], num_features[k], trail_features)

            # Actualizacion local de la feromona
            update_trail(  trail_features, local.evap, 10^(-6), paths[k] )
        }

        # Aplicamos busqueda local
        paths, evs_BL = map(BL, paths)

        scores = map(tasa_clasificacion, paths)
        n_eval = n_eval + sum(evs_BL) + num_ants

        if(max(scores) > tasa_best)
            mask_best = paths[ arg_max(scores) ]

        # Actualizacion global de feromona
        update_trail( trail_features, global_evap,
                      scores[arg_max(scores)], paths[arg_max(scores)] )
        # El único 1 del vector que pasamos a update_trail está en arg_max(scores)
        update.trail( trail_num_features, global_evap, max(score), [ 0,0,...1,...0] )
    }

    return mask_best
}
