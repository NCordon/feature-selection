population = [for i in [1..num_crom] yield{
    # n número de predictores del dataset
    mascara = [ random({0,1}) from 0 to n ]
    list(mask = mascara, fitness = tasa(mascara), evaluated = True)
 }]
