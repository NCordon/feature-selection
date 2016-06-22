def SHMM_BL(data){
    data_heuristic = entropy(data)

    while(n_eval < max_eval){
        init_iteration(data)

        for k in {1...num.ants}{
            paths[k] = make_transitions(paths[[k]], num_features[k], trail_features)
        }

        # Aplicamos busqueda local
        paths, evs_BL = map(BL, paths)

        scores = map(tasa, paths)
        n_eval = n_eval + sum(evs_BL) + num_ants

        if(max(scores) > tasa(mask_best)){
            mask_best = paths[ arg_max(scores) ]
            trail_max = tasa( mask_best )
            trail_min = trail_max/500
        }
        # Actualizacion global de feromona
        update_trail( trail_features, global_evap,
                      scores[arg_max(scores)], paths[arg_max(scores)] )
        # El único 1 del vector que pasamos a update_trail está en arg_max(scores)
        update_trail(trail_num_features, global_evap, max(score), [0,0,...1,...0])


        trail_features[ which(trail_features > trail_max) ] = trail_max
        trail_features[ which(trail_features < trail_min) ] = trail_min
    }
    return mask_best
}
