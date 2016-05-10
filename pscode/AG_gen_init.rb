population = [for i in [1..n.crom] yield{
    # n número de predictores del dataset
    mascara = {1..n}.random(0 or 1)
    list(mask = mask, fitness = tasa(mascara), evaluated = True)
 }]
