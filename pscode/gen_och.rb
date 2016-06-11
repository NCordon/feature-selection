# cada path tiene longitud n, número de características
paths = [ permutation[1,0,0,0...,0] for i in {1...num_ants} ]

# Numero de características que escogera cada hormiga
num_características =  [ sample( 0:n-1, prob=trail_num_features)
                         for i in {1...num_ants} ]
