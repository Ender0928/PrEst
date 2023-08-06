#P2
data_SOCR = read.table("SOCR.dat", header = TRUE)

par(mfrow = c(1, 2))

#hsitoriograma del peso
hist(data_SOCR$weight, main = "historiograma peso", xlab = "Libras", ylab = "Densidad", freq = FALSE, col="blue")

#curva de densidad de la normal asociada al historiograma peso
x_weight = seq(min(data_SOCR$weight), max(data_SOCR$weight), length = 100)
y_weight = dnorm(x_weight, mean(data_SOCR$weight), sd(data_SOCR$weight))
lines(x_weight, y_weight, col = "red")

#historiograma de altura
hist(data_SOCR$height, main = "historiograma altura", xlab = "Pulgadas", ylab = "Densidad", freq = FALSE, col="green")

#curva de densidad de la normal asociada al historiograma altura
x_height = seq(min(data_SOCR$height), max(data_SOCR$height), length = 100)
y_height = dnorm(x_height, mean(data_SOCR$height), sd(data_SOCR$height))
lines(x_height, y_height, col = "red")


#calcular media de altura
cat("Media Altura:")
mean(data_SOCR$height)

#calcular desviación estándar altura
cat("Desviacion estandar altura:")
sd(data_SOCR$height)

#calcular media peso
cat("Media Peso:")
mean(data_SOCR$weight)

#calcular desviación estándar peso
cat("Desviacion estandar peso:")
sd(data_SOCR$weight)

#calcular probabilidad que una persona tenga altura entre 66 y 69 pulgadas
cat("Probabilidad altura entre 66 y 69:")
pnorm(69, mean(data_SOCR$height), sd(data_SOCR$height)) - pnorm(66, mean(data_SOCR$height), sd(data_SOCR$height))

#calcular posibilidad que una persona tenga un peso mayor a 134 libras
cat("Probabilidad peso mayor a 134:")
1 - pnorm(134, mean(data_SOCR$weight), sd(data_SOCR$weight))

#calcular probabilidad que al menos 4 de las 20 personas seleccionadas tengan una altura mayor a 50 pulgadas
cat("Probabilidad al menos 4 tengan altura mayor a 50:")
Nrow = nrow(data_SOCR)
1 - pbinom(3, 20, sum(data_SOCR$height > 50)/Nrow) #todas las personas son mayores a 50 pulgadas

#calcular probabilidad que al menos 4 de las 20 personas seleccionadas tengan altura mayor a 70 pulgadas y 11 menos de 70 pulgadas
cat("Probabilidad al menos 4 tengan altura mayor a 70 y 11 menos de 70:")
sumMas70 = sum(data_SOCR$height > 70)
p1 = sum(dhyper(4:20, sumMas70, Nrow-sumMas70, 20))
p2 = sum(dhyper(0:9, Nrow-sumMas70, sumMas70, 20))
p3 = sum(dhyper(0:10, Nrow-sumMas70, sumMas70, 20))
p4 = sum(dhyper(0:11, Nrow-sumMas70, sumMas70, 20))

ptotal = p1 + p2 + p3 + p4
ptotal

#calcular cuantil 0.10 variable peso
cat("Cuantil 0.10 variable peso:") 
quantile(data_SOCR$weight, 0.1)

#calcular cuantil 0.60 variable altura
cat("Cuantil 0.60 variable altura:")
quantile(data_SOCR$height, 0.6)


#generación de muestra aleatoria de tamaño 100000 con probabilidad de salir cara de 0.002%
#a partir de distribución binomial. Se guardará como muestra_aleatoria
n = 100000
size = 10000
prob = 0.002
λ = 2
muestra_aleatoria = rbinom(n, size, prob)
#historiograma de muestra_aleatoria
hist(muestra_aleatoria, col = "blue", main = "Muestra Binomial(1000, 0.002)")

#realizar nueva muestra aleatoria
muestra_aleatoria_Poisson = rpois(n, λ)
#historiograma de muestra_aleatoria_Poisson
hist(muestra_aleatoria_Poisson, col = "green", main = "Distribución Poisson λ = 2 ")
