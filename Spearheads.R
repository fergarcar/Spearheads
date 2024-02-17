getwd()
nuevo_dir <- "C:/Spearheads"
setwd (nuevo_dir)

install.packages("readxl")
library(readxl)

spear <-read_excel("C:/spear/spearheads.xlsx")

View(spear)
str(spear)
class(spear)

spear <- as.data.frame(spear)

#Ejercicio2. renombramos las variables

names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservacion"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Maxwit"] <- "Ancho_max_encaje"
names(spear)[names(spear) == "Weight"] <- "Peso"

spear
View(spear)

#Ejercicio3. asigna las etiquas
spear$Contexto=factor(spear$Contexto,levels=c('1', '2', '3'), labels=c("s/c", "Habitacional", "Funerario"))
spear$Conservacion=factor(spear$Conservacion,levels=c(1,2,3,4), labels=c("Excelente", "Bueno", "Regular", "Malo"))
spear$Remache=factor(spear$Remache,levels=c(1,2), labels=c('Si', 'No'))
spear$Materiales=factor(spear$Materiales,levels=c(1,2), labels=c('Bronce', 'Hierro'))

#Visualizamos

View(spear)

#Ejercicio 4. Generar tablas de frecuencias a tres de las variables
tabla_materiales <- table(spear$Materiales)
print (tabla_materiales)

tabla_contexto <- table(spear$Contexto)
print(tabla_contexto)

tabla_conservacion <- table(spear$Conservacion)
print (tabla_conservacion)

#Ejercicio 5. Generar tablas cruzadas de porcentaje de materiales sobre contexto y conservacion


#Ejercicio 6. Generar tablas de porcentaje de las variables materiales, contexto y conservacion
porcentaje_materiales <- prop.table(table(spear$Materiales))
print (porcentaje_materiales)

porcentaje_contexto <- prop.table(table(spear$Contexto))
print(porcentaje_contexto)

porcentaje_conservacion <- prop.table(table(spear$Conservacion))
print(porcentaje_conservacion)

#Ejercicio 7. Genera tablas cruzadas de porcentaje


#Ejercicio 8. Genera graficos de barras verticales para variables conservacion y contexto indicando la frecuencia de cada factor
plot(factor(spear$Conservacion), main = "Grafico verticales conservacion", col = rainbow(3))

plot(factor(spear$Contexto), main = "Grafico verticales contexto", col = rainbow(4))

#Ejercicio 9.Genera graficos horizontales para variables materiales y remache
barplot(tabla_materiales, main = "Grafico barras horizontales", col = rainbow(2),
        ylab = "Materiales", xlab = "Frecuencia",
        horiz = TRUE)

tabla_remache <- table(spear$Remache)
print(tabla_remache)

barplot(tabla_remache, main = "Grafico barras horizontales remache", col = rainbow(2),
        ylab = "Remache", xlab = "Frecuencia",
        horiz = TRUE)

#Ejercicio 10.

#Ejercicio 11.

#Ejercucui 12. Genera un histograma de probabilidad de las variables
set.seed(1)

x <- rnorm(spear$Contexto)
y <- rnorm(spear$Materiales)
z <- rnorm(spear$Conservacion)

hist(x, main = "Tres variables", ylab = "Frecuencia")
hist(y, add = TRUE)
hist(z, add = TRUE, col = rgb(1,0,0, alpha = 0.5))