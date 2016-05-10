def selection(num_crom){
    pairs = [ for i in {1,..,num_crom} {1..n}.random(0 or 1) ]

    v =[for p in pairs{
        if(pair.first().fitness < pair.second().fitness)
            yield pair.first()
        else
            yield pair.second()
    ]
}
