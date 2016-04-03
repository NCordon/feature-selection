def tasa_clasificacion (train, mascara){
  train = apply(mascara, train)
  clases = train[class]
  train = train[-class]

  fit = leave.one.out.3knn(train, clases)

  return (100 * length(which(clases == fit)) / length(clases))
}
