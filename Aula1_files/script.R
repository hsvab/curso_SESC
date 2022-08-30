#Importando biblioteca
library(RColorBrewer)
data(VADeaths)

#Exemplo de Histograma
par(mfrow=c(2,3))
hist(VADeaths,col=brewer.pal(8,"Greens"),main="Greens 8 colors")

#Exemplo de Gráfico de linha
plot(AirPassengers,type="l")

#Exemplo de Box Plot
boxplot(iris$Petal.Length~iris$Species) 