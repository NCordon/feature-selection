def entropy(data){
    heuristic_info = [0,0...0]

    for i in {1...n}{

        if( not.numeric(data[,i]) ){
            discretized_values = split(data[,i], data.class)
        }
        else{
            columna = sort(data[,i])
            chunk = ceil( length(columna)/10 )
            discretized_values = split(columna, size_split = chunk )
        }

        Nc <- length(unique(data.class))
        value = 0

        for c in {1...Nc}{

            for f in discretized_values{
                dist_f = data[data[,i %in% f, ]

                prob_c_f <- sum(which(dist_f.class == c)) / length(f)
                prob_c <- sum(which(data.class == c)) / length(columna)
                prob_f <- length(dist_f) / length(columna)

                if (prob_c_f > 0){
                    value += prob_c_f * log_2(prob_c_f / (prob_c * prob_f))
                }
            }
        }
        heuristic_info[i] = value
    }
    return heuristic_info
}
