





# 2 clases
unique(wdbc$class)
# 15 clases
length(unique(mlibras$class))
# 5 clases
length(unique(arrhythmia$class))





library(reshape2)

# SFS
cols <- colnames(wdbc)[SFS(wdbc)==1]

wdbc.melted <- melt(wdbc, id.vars='class', measure.vars = cols)
ggplot(data=wdbc.melted, aes(variable, value, color=class)) + geom_boxplot() +
  theme(text=element_text(size=20), axis.text.x = element_blank())


wdbc.melted <- melt(wdbc, id.vars='class', measure.vars = colnames(wdbc)[1:30])
ggplot(data=wdbc.melted, aes(variable, value, color=class)) + geom_boxplot() +
  theme(text=element_text(size=20), axis.text.x = element_blank())

help(melt)

# mlibras
cols.SFS <- colnames(mlibras)[which(SFS(mlibras)==1)]
cols.tabuext <- colnames(mlibras)[which(BT.ext(mlibras)==1)]

mlibras.melted <- melt(mlibras, id.vars='class', measure.vars = cols.SFS)
ggplot(data=mlibras.melted, aes(variable, value, color=class)) + geom_boxplot() +
  theme(text=element_text(size=20), axis.text.x = element_blank())

