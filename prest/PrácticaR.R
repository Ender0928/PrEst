# Instalar la librería ggplot2 si no está instalada, será usada para la mejora de gráficas
install.packages("ggplot2")


#P1

#importar datos a RStudio
googlPl <- read.csv("googleplay.csv", sep=";", dec=".", na.strings="NaN", stringsAsFactors = FALSE)
googlPl$Rating <- as.numeric(googlPl$Rating) 
googlPl$Reviews <- as.numeric(googlPl$Reviews)
googlPl$Price <- as.numeric(googlPl$Price)

#eliminar filas que tienen valores faltantes

googlPl = na.omit(googlPl)

#identificar variables numéricas 
var_num <- sapply(googlPl, is.numeric)
cat("Variables numéricas:", names(googlPl[var_num]), "\n")

#cargar tabla de frecuencias
table(googlPl$Category)

#diagrama de sectores tabla de frecuencias
pie(table(googlPl$Category), cex = 0.47, main = "Categorías de aplicaciones en Google Play Store")

#encontrar la categoría con más aplicaciones
max <- sort(table(googlPl$Category), decreasing = TRUE)[1]
cat("La categoría con más aplicaciones es:", names(max), "\n")

#encontrar las 5 categorías con menos aplicaciones
min <- sort(table(googlPl$Category), decreasing = FALSE)[1:5]
cat("Las 5 categorías con menos aplicaciones son:", names(min), "\n")

#encontrar las 5 categorías con mayor disponibilidad de aplicaciones
max <- sort(table(googlPl$Category), decreasing = TRUE)[1:5]
cat("Las 5 categorías con mayor disponibilidad de aplicaciones son:", names(max), "\n")


# Obtener 3 categorías dentro de "Category". Se han elegido "game", "family" y "business"
game <- subset(googlPl, Category == "GAME")
family <- subset(googlPl, Category == "FAMILY")
business <- subset(googlPl, Category == "BUSINESS")

# Crear los histogramas de las 3 categorías seleccionadas anteriormente 
par(mfrow = c(3, 1))
hist(game$Rating, main = "Juegos", xlab = "Calificación general", ylab = "Frecuencia absoluta", col="blue")
hist(family$Rating, main = "Familia", xlab = "Calificación general", ylab = "Frecuencia absoluta", col="red")
hist(business$Rating, main = "Negocios", xlab = "Calificación general", ylab = "Frecuencia absoluta", col = "green")

# Crear el subconjunto de datos de las 3 categorías seleccionadas anteriormenete
subset_googlPl <- subset(googlPl, Category %in% c("GAME", "FAMILY", "BUSINESS"))

# Cargar la librería ggplot2
library(ggplot2)
# Crear el gráfico conjunto
ggplot(subset_googlPl, aes(x = Category, y = Reviews)) +
  geom_boxplot(aes(fill = Category)) +
  scale_y_log10() +
  scale_fill_manual(values = c("GAME" = "blue", "FAMILY" = "red", "BUSINESS" = "green")) +
  labs(x = "Categoría", y = "Cantidad de reseñas (log10)")

#Se observa que las tres categorías tienen una gran cantidad de datos atípicos, 
#especialmente la categoría "GAME". Además, en todas las categorías se observa 
#una gran asimetría hacia valores altos de cantidad de reseñas, lo que sugiere 
#que la mayoría de las aplicaciones tienen pocas reseñas y solo un pequeño
#número de aplicaciones tienen muchas reseñas.

# Cargar datos desde el archivo "vitamin.dat"
datos_vitaminas<- read.table("vitamin.dat", header=TRUE)

# Generación de diagrama de medias
ggplot(datos_vitaminas, aes(x=supp, y=len)) +
  geom_bar(stat="summary", fun="mean", fill="orange", color="black") +
  ggtitle("Longitud del incisivo por formato de vitamina C") +
  xlab("Formato de vitamina C") +
  ylab("Longitud del incisivo (mm)")

#Coeficiente de correlación lineal entre la longitud de los incisivos y la dosis de vitamina C administrada en formato ácido ascórbico
cor_ascorbico <- cor(datos_vitaminas$len[datos_vitaminas$supp == "VC"], datos_vitaminas$dose[datos_vitaminas$supp == "VC"])

#Coeficiente de correlación lineal entre la longitud de los incisivos y la dosis de vitamina C administrada en formato de zumo de naranja
cor_zumo <- cor(datos_vitaminas$len[datos_vitaminas$supp == "OJ"], datos_vitaminas$dose[datos_vitaminas$supp == "OJ"])

# Imprimir los coeficientes de correlacion lineal

cat("Coeficiente de correlación lineal entre la longitud de los incisivos y la dosis de vitamina C administrada en formato ácido ascórbico:", cor_ascorbico, "\n")
cat("Coeficiente de correlación lineal entre la longitud de los incisivos y la dosis de vitamina C administrada en formato de zumo de naranja:", cor_zumo, "\n")

#En base a los resultados anteriores, presenta más correlación la dosis de vitamina C administrada 
#en formato ácido ascórbico

# Ajustar una recta de regresión lineal
recta_regresion <- lm(len ~ dose, data = datos_vitaminas)

# Imprimir los coeficientes de la recta de regresión
cat("Intercepto:", coefficients(recta_regresion)[1], "\n")
cat("Pendiente:", coefficients(recta_regresion)[2], "\n")

# Imprimir el coeficiente de determinación (R^2) del modelo
cat("Coeficiente de determinación (R^2):", summary(recta_regresion)$r.squared, "\n")

############################################################################################################
############################################################################################################

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
