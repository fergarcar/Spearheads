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
tabla_contexto <- table(spear$Contexto)
print(tabla_contexto)

tabla_conservacion <- table(spear$Conservacion)
print (tabla_conservacion)

tabla_materiales <- table(spear$Materiales)
print(tabla_materiales)

#Ejercicio 5. Generar tablas cruzadas de porcentaje de materiales sobre contexto y conservacion
cross.matcon=table (spear$Materiales,spear$Contexto)
print(cross.condcon)

cross.matcond=table (spear$Materiales, spear$Conservacion)
print (cross.matcond)

#Ejercicio 6. Generar tablas de porcentaje de las variables materiales, contexto y conservacion
porcentaje_materiales <- prop.table(table(spear$Materiales))
print (porcentaje_materiales)

porcentaje_contexto <- prop.table(table(spear$Contexto))
print(porcentaje_contexto)

porcentaje_conservacion <- prop.table(table(spear$Conservacion))
print(porcentaje_conservacion)

#Ejercicio 7. Genera tablas cruzadas de porcentaje
cruzada_porc_materiales= round(prop.table(table (porcentaje_materiales)))
print(cruzada_porc_materiales)

cruzada_porc_contexto = round(prop.table(table(spear$Contexto)))
print(cruzada_porc_contexto)

cruzada_porc_conservacion = round((prop.table(table(porcentaje_conservacion))))
print(cruzada_porc_conservacion)


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

#Ejercicio 10. Genera graficos de barras agrupados 
barplot(cross.matcond, width = 0.85, ylim = c(0,
       sum(cross.matcond [,1])*1.1),
       main = "Grafico de barras apilado",
       ylab = "Frecuencia",
       col = c("yellow", "skyblue"),
       legend = TRUE)
        

#Ejercicio 11. Genera un grafico de sectores
pie(tabla_conservacion, labels = paste0(tabla_conservacion, "%"))

legend("topleft", legend = c("Excelente", "Bueno", "Regular", "Malo"),
       fill = c("lightgreen", "lightblue", "lightyellow", "pink"))


#Ejercicio 12. Genera un histograma de probabilidad de las variables
hist <-data.frame(spear)

barplot(table(hist$Materiales, hist$Contexto, hist$Conservacion, hist$Remache),
        legend = TRUE,
        col = rainbow(2, alpha = 4),
        beside = TRUE,
        main = "Histograma de probabilidad",
        )


set.seed(1)

x <- rnorm(spear$Contexto)
y <- rnorm(spear$Materiales)
z <- rnorm(spear$Conservacion)

hist(x, main = "Tres variables", ylab = "Frecuencia")
hist(y, add = TRUE)
hist(z, add = TRUE, col = rgb(1,0,0, alpha = 0.5))
