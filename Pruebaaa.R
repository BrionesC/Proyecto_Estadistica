install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library(openxlsx)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("GGally")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

file.choose()
# Lee el archivo Excel
datos <- read_excel("DatosFiltrados.xlsx")

# Muestra los primeros datos para verificar que se leyeron correctamente
head(datos)

# Filtrar filas con el nombre "Guayas" en la columna "Provincia inscrita"
filas_guayas_ <- subset(datos, cant_insc == "Guayaquil")

filas_guayas <- filas_guayas_[filas_guayas_$hijos_2 <= 20, ]

# Mostrar las filas que contienen "Guayaquil"
print(filas_guayas)

#Variables cualitativas:Parroquia inscrita, causa de divorcio, {parroquia habitada por Hombre,parroquia habitada por Mujer}
#Variables cuantitativas:[ano del matrimonio],duración del matrimonio en años, cantidad de hijos, año del divorcio, día del divorcio

#V. cualitativas headname:parr_insc, cau_div , {parr_hab1, parr_hab2}
#V. cuantitativas headname:anio_mat, dur_mat, hijos_2, anio_div, dia_div 

#ESTADÍSTICA DESCRIPTIVA UNIVARIANTE
#Variables cualitativas:

# Tabla de frecuencia de la parroquia inscrita
tabla_frec_parroquia <- data.frame(table(filas_guayas$parr_insc))
print(tabla_frec_parroquia)

#Guardar tabla de frecuencias en excel
write.xlsx(tabla_frec_parroquia, "C:\\Users\\jefer\\OneDrive\\Documents\\5 Semestre\\Estadística\\Proyecto-Briones-Guale", rownames = TRUE)
# Crear el gráfico de barras
barplot(tabla_frec_parroquia$Freq, names.arg = tabla_frec_parroquia$Var1, col = "lightgreen",
        main = "Parroquia inscrita", xlab = "Parroquia", ylab = "Frecuencia")

# Tabla de frecuencia de la causa de divorcio
tabla_frec_cau_div <- data.frame(table(filas_guayas$cau_div))

# Nueva columna
causa <- c("A", "B", "C", "D", "E", "F", "G", "H") # Valores para la nueva columna

# Agregar la nueva columna al dataframe utilizando cbind()
tabla_frec_cau_div <- cbind(tabla_frec_cau_div, causa)
print(tabla_frec_cau_div)
#Guardar tabla de frecuencias en excel
write.xlsx(tabla_frec_cau_div, "C:\\Users\\jefer\\OneDrive\\Documents\\5 Semestre\\Estadística\\Proyecto-Briones-Guale", rownames = TRUE)

# Crear el gráfico de barras
barplot(tabla_frec_cau_div$Freq, names.arg = tabla_frec_cau_div$causa, col = "purple",
        main = "Causa de divorcio", xlab = "Causa de divorcio", ylab = "Frecuencia")
legend("topleft",legend=paste (tabla_frec_cau_div$causa, ":", tabla_frec_cau_div$Var1),cex=0.3)

#~Tabla de Frecuencias de la parroquia habitada por Hombre
tabla_frec_parroquia_hombre <- data.frame(table(filas_guayas$parr_hab1))
print(tabla_frec_parroquia_hombre)
#Guardar tabla de frecuencias en excel
write.xlsx(tabla_frec_parroquia_hombre, "C:\\Users\\jefer\\OneDrive\\Documents\\5 Semestre\\Estadística\\Proyecto-Briones-Guale", rownames = TRUE)

#Crear el gráfico de barras
barplot(tabla_frec_parroquia_hombre$Freq, names.arg = tabla_frec_parroquia_hombre$Var1, col = "chartreuse",
        main = "Parroquia de origen del hombre", xlab = "Parroquia", ylab = "Frecuencia")

#~Tabla de Frecuencias de la parroquia habitada por Mujer
tabla_frec_parroquia_mujer <- data.frame(table(filas_guayas$parr_hab2))
print(tabla_frec_parroquia_mujer)
#Guardar tabla de frecuencias en excel
write.xlsx(tabla_frec_parroquia_mujer, "C:\\Users\\jefer\\OneDrive\\Documents\\5 Semestre\\Estadística\\Proyecto-Briones-Guale", rownames = TRUE)

#Crear el gráfico de barras
barplot(tabla_frec_parroquia_mujer$Freq, names.arg = tabla_frec_parroquia_mujer$Var1, col = "darkgoldenrod1",
        main = "Parroquia de origen de la mujer", xlab = "Parroquia", ylab = "Frecuencia")


#Variable cuantitativas:

# Tabla de frecuencia del año de divorcio (anio_div)
tabla_frec_anio_div <- table(filas_guayas$anio_div)
print(tabla_frec_anio_div)
# Gráficos
#Histograma
hist(filas_guayas$anio_div,
     main = "Histograma del año del divorcio", 
     xlab = "Año del divorcio", 
     ylab = "Frecuencia",
     col = "chocolate1",
     border = "black")

#Diagrama de cajas
boxplot(filas_guayas$anio_div,
        horizontal = TRUE,
        main = "Año del divorcio",
        col = "burlywood1",
        border = "black",
        ylim = c(1982,2022))

#mean(filas_guayas$anio_div)

#Ojiva
# Calcular la suma acumulada de las frecuencias
frecuencia_acumulada <- cumsum(tabla_frec_anio_div)
print(frecuencia_acumulada)
# Crear un vector de valores x para el eje x
valores_x <- seq_along(frecuencia_acumulada)
print(valores_x)

# Trazar la ojiva
plot(valores_x,frecuencia_acumulada, type = "o", 
     main = "Ojiva de Frecuencias Acumuladas",
     xlab = "Valores", ylab = "Frecuencias Acumuladas",
     col = "Blue")

#Resumen estadístico
resumen_anio_div <- summary(filas_guayas$anio_div)
print(resumen_anio_div)

# Tabla de frecuencia de la cantidad de hijos en el matrimonio (hijos_2)
tabla_frec_hijos_2 <- table(filas_guayas$hijos_2)
print(tabla_frec_hijos_2)
# Gráficos
#Histograma
hist(filas_guayas$hijos_2,
     main = "Histograma de hijos en el matrimonio", 
     xlab = "Hijos en el matrimonio", 
     ylab = "Frecuencia",
     col = "chocolate2",
     border = "black")

#Diagrama de cajas
boxplot(filas_guayas$hijos_2,
        horizontal = TRUE,
        main = "Hijos en el matrimonio",
        ylab = "Valores",
        col = "burlywood2",
        border = "black")
#Ojiva
# Calcular la suma acumulada de las frecuencias
frecuencia_acumulada_hijos_2<- cumsum(tabla_frec_hijos_2)

# Crear un vector de valores x para el eje x
valores_x_hijos_2 <- seq_along(frecuencia_acumulada_hijos_2)

# Trazar la ojiva
plot(valores_x_hijos_2, frecuencia_acumulada_hijos_2, type = "o", 
     main = "Ojiva de Frecuencias Acumuladas",
     xlab = "Hijos en el matrimonio", ylab = "Frecuencias Acumuladas",
     col = "Blue")

#Resumen estadístico
resumen_hijos_2 <- summary(filas_guayas$hijos_2)
print(resumen_hijos_2)

# Tabla de frecuencia del duración del matrimonio (dur_mat)
tabla_frec_dur_mat <- table(filas_guayas$dur_mat)
print(tabla_frec_dur_mat)
# Gráficos
#Histograma
hist(filas_guayas$dur_mat,
     main = "Histograma de duración del matrimonio", 
     xlab = "Duración del matrimonio en años", 
     ylab = "Frecuencia",
     col = "chocolate3",
     border = "black")

#Diagrama de cajas
boxplot(filas_guayas$dur_mat,
        horizontal = TRUE,
        main = "Años de duración del matrimonio",
        ylab = "Valores",
        col = "burlywood",
        border = "black")
#Ojiva
# Calcular la suma acumulada de las frecuencias
frecuencia_acumulada_dur_mat<- cumsum(tabla_frec_dur_mat)

# Crear un vector de valores x para el eje x
valores_x_dur_mat <- seq_along(frecuencia_acumulada_dur_mat)

# Trazar la ojiva
plot(valores_x_dur_mat, frecuencia_acumulada_dur_mat, type = "o", 
     main = "Ojiva de Frecuencias Acumuladas",
     xlab = "Años de duración del matrimonio", ylab = "Frecuencias Acumuladas",
     col="Blue")

#Resumen estadístico
resumen_dur_mat <- summary(filas_guayas$dur_mat)
print(resumen_dur_mat)

# Tabla de frecuencia del año de matrimonio
tabla_frec_anio_mat <- table(filas_guayas$anio_mat)
print(tabla_frec_anio_mat)
# Gráficos
#Histograma
hist(filas_guayas$anio_mat,
     main = "Histograma del año del matrimonio", 
     xlab = "Año de inicio del matrimonio", 
     ylab = "Frecuencia",
     col = "chocolate",
     border = "black")

#Diagrama de cajas
boxplot(filas_guayas$anio_mat,
        horizontal = TRUE,
        main = "Año de inicio del matrimonio",
        ylab = "Valores",
        col = "burlywood3",
        border = "black")
#Ojiva
# Calcular la suma acumulada de las frecuencias
frecuencia_acumulada_anio_mat <- cumsum(tabla_frec_anio_mat)
print(frecuencia_acumulada_anio_mat)

# Crear un vector de valores x para el eje x
valores_x_anio_mat <- seq_along(frecuencia_acumulada_anio_mat)

# Trazar la ojiva
plot(valores_x_anio_mat, frecuencia_acumulada_anio_mat, type = "o", 
     main = "Ojiva de Frecuencias Acumuladas",
     xlab = "Valores", ylab = "Frecuencias Acumuladas",
     col="Blue")

#Resumen estadístico
resumen_anio_mat <- summary(filas_guayas$anio_mat)
print(resumen_anio_mat)


# Tabla de frecuencia del dia de divorcio
tabla_frec_dia_div <- table(filas_guayas$dia_div)
print(tabla_frec_dia_div)
# Gráficos
#Histograma
hist(filas_guayas$dia_div,
     main = "Histograma de día del divorcio", 
     xlab = "Día del matrimonio", 
     ylab = "Frecuencia",
     col = "chocolate4",
     border = "black")

#Diagrama de cajas
boxplot(filas_guayas$dia_div,
        horizontal = TRUE,
        main = "Día del divorcio",
        ylab = "Valores",
        col = "burlywood4",
        border = "black")
#Ojiva
# Calcular la suma acumulada de las frecuencias
frecuencia_acumulada_dia_div <- cumsum(tabla_frec_dia_div)

# Crear un vector de valores x para el eje x
valores_x_dia_div <- seq_along(frecuencia_acumulada_dia_div)

# Trazar la ojiva
plot(valores_x_dia_div, frecuencia_acumulada_dia_div, type = "o", 
     main = "Ojiva de dia del divorcio",
     xlab = "Valores", ylab = "Frecuencias Acumuladas",
     col="Blue")

#Resumen estadístico
resumen_dia_div <- summary(filas_guayas$dia_div)
print(resumen_dia_div)


#Punto 2
#a)Gráficos de dispersión
attach(datos)
names(datos)
#1Par-anio_mat y dur_mat
grafica_aniomat_durmat = ggplot(datos,aes(anio_mat, dur_mat))
grafica_aniomat_durmat + geom_point() + labs( x= "Anio de matrimonio", y= "Duración de matrimonio") + geom_smooth(method = "lm",colour = "Red")

#2Par-anio_mat y hijos_2
grafica_aniomat_hijos_2 = ggplot(datos,aes(anio_mat, hijos_2))
grafica_aniomat_hijos_2 + geom_point() + labs( x= "Anio de matrimonio", y= "Número de hijos") + geom_smooth(method = "lm",colour = "Blue")

#3Par-anio_mat y anio_div
grafica_aniomat_aniodiv = ggplot(datos,aes(anio_mat, anio_div))
grafica_aniomat_aniodiv + geom_point() + labs( x= "Anio de matrimonio", y= "Anio de divorcio") + geom_smooth(method = "lm",colour = "Green")

#4Par-anio_mat y dia_div
grafica_aniomat_diadiv = ggplot(datos,aes(anio_mat, dia_div))
grafica_aniomat_diadiv + geom_point() + labs( x= "Anio de matrimonio", y= "Día de Divorcio") + geom_smooth(method = "lm",colour = "Blue")

#5Par-dur_mat y hijos_2
grafica_durmat_hijos_2 = ggplot(datos,aes(dur_mat, hijos_2))
grafica_durmat_hijos_2 + geom_point() + labs( x= "Duración del matrimonio", y= "Número de hijos") + geom_smooth(method = "lm",colour = "Yellow")

#6Par-dur_mat y anio_div
grafica_durmat_aniodiv = ggplot(datos,aes(dur_mat, anio_div))
grafica_durmat_aniodiv + geom_point() + labs( x= "Duración del matrimonio", y= "Anio de Divorcio") + geom_smooth(method = "lm",colour = "Orange")

#7Par-dur_mat y dia_div
grafica_durmat_diadiv = ggplot(datos,aes(dur_mat, dia_div))
grafica_durmat_diadiv + geom_point() + labs( x= "Duración del matrimonio", y= "Día de Divorcio") + geom_smooth(method = "lm",colour = "Purple")

#8Par-hijos_2 y anio_div
grafica_hijos2_aniodiv = ggplot(datos,aes(hijos_2, anio_div))
grafica_hijos2_aniodiv + geom_point() + labs( x= "Número de hijos", y= "Anio de divorcio") + geom_smooth(method = "lm",colour = "Yellow")

#9Par-hijos_2 y dia_div
grafica_hijos2_diadiv = ggplot(datos,aes(hijos_2, dia_div))
grafica_hijos2_diadiv + geom_point() + labs( x= "Número de hijos", y= "Día de divorcio") + geom_smooth(method = "lm",colour = "Green")

#10Par-anio_div y dia_div
grafica_aniodiv_diadiv = ggplot(datos,aes(anio_div, dia_div))
grafica_aniodiv_diadiv + geom_point() + labs( x= "Anio de divorcio", y= "Día de divorcio") + geom_smooth(method = "lm",colour = "Blue")

#b)Ojivas de todos en un gráfico
#Ojiva Base anio_div
plot(valores_x, frecuencia_acumulada, type = "o", 
     main = "Ojiva de Frecuencias Acumuladas",
     xlab = "Valores", ylab = "Frecuencias Acumuladas",
     col = "Blue")
#agregar ojiva 2 cantidad de hijos
lines(valores_x_hijos_2,frecuencia_acumulada_hijos_2, type = "o",
      col= "Red")
#agregar ojiva 3 duración del matrimonio
lines(valores_x_dur_mat,frecuencia_acumulada_dur_mat, type = "o",
      col= "Green")
#agregar ojiva 4 anio del matrimonio
lines(valores_x_anio_mat,frecuencia_acumulada_anio_mat, type = "o",
      col= "Yellow")
#agregar ojiva 5 dia de divorcio
lines(valores_x_dia_div,frecuencia_acumulada_dia_div, type = "o",
      col= "Purple")

#c)Diagramas de caja en un solo gráfico  (CORREGIR)-----

boxplot(filas_guayas$anio_div, filas_guayas$dur_mat, filas_guayas$hijos_2, filas_guayas$anio_mat,
        main = "Multiple boxplots for comparision",
        loc = c(1,2,4,5),
        names = c("Año del divorcio", "Duración del matrimonio", "Cantidad de hijos", "Año del matrimonio"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

#d)Creación de diagramas de cajas para una variable cualitativa ----CORREGIR----

filas_guayas_r<- subset(filas_guayas, parr_insc == "Rocafuerte")
print(filas_guayas_r)
filas_guayas_rocafuerte <- filas_guayas_r[filas_guayas_r$hijos_2 <= 20, ]

filas_guayas_t<- subset(filas_guayas, parr_insc == "Tarqui")
print(filas_guayas_t)
filas_guayas_tarqui<- filas_guayas_t[filas_guayas_t$hijos_2 <= 20, ]

boxplot(filas_guayas_rocafuerte$anio_div,filas_guayas_tarqui$anio_div, filas_guayas_rocafuerte$anio_mat,filas_guayas_tarqui$anio_mat,
        main = "Multiple boxplots for comparision",
        loc = c(1,2,4,5),
        names = c("Año_div_R", "Año_div_T", "Año_mat_R", "Año_mat_T"),
        las = 2,
        col = c("mediumpurple1","hotpink1"),
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

#filas_guayas$dur_mat, filas_guayas$hijos_2

boxplot(filas_guayas_rocafuerte$dur_mat,filas_guayas_tarqui$dur_mat, filas_guayas_rocafuerte$hijos_2,filas_guayas_tarqui$hijos_2,
        main = "Multiple boxplots for comparision",
        loc = c(1,2,4,5),
        names = c("DurMat_R", "DurMat_T", "CantH_R", "CantH_T"),
        las = 2,
        col = c("mediumpurple3","hotpink3"),
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

boxplot(filas_guayas$anio_div~filas_guayas$mes_div, 
        Horizontal = TRUE,
        data = filas_guayas,
        main = "Diagrama de Cajas",
        ylab = "Valores",
        col = "royalblue1",
        border = "black")

boxplot(filas_guayas$anio_div~filas_guayas$cau_div, 
        Horizontal = TRUE,
        data = filas_guayas,
        main = "Diagrama de Cajas",
        ylab = "Valores",
        col = "royalblue2",
        border = "black")

boxplot(filas_guayas$hijos_2~filas_guayas$cau_div, 
        Horizontal = TRUE,
        data = filas_guayas,
        main = "Diagrama de Cajas",
        ylab = "Valores",
        col = "royalblue3",
        border = "black")

#ggplot(data, aes(x = factor(cau_div), y = dur_mat, fill = factor(cau_div))) +
#  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.6) +
#  labs(title = "Diagramas de Cajas de la Duraación del matrimonio por causa de divorcio",
#       x = "Causa de divorcio", y = "Duración del matrimonio") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje X para mejor visualización

#e)matriz de correlación para variables cuantitativas
# Selecciona solo las variables cuantitativas para la matriz de correlación
data_numeric <- filas_guayas[c("anio_mat", "dur_mat", "hijos_2", "anio_div", "dia_div")]
print(data_numeric)

#f)Determine la matriz de Varianzas y Covarianzas, para las variables cuantitativas.



#g)Muestre el gráfico multivariante de dispersion de las variables cuantitativas y considere un color distinto para cada variable.




## ESTADÍSTICO DE PRUEBA

#Xi-cuadrado

#H0 = La causa del divorcio no depende de la parroquia inscrita.

#H1 = La causa del divorcio depende de la parroquia inscrita.

# Crear una tabla de contingencia
tabla_contingencia <- table(filas_guayas$parr_insc, filas_guayas$cau_div)

# Mostrar la tabla de contingencia
print(tabla_contingencia)

# Realizar la prueba de Chi-cuadrado
resultado_chi_cuadrado <- chisq.test(tabla_contingencia)
print(resultado_chi_cuadrado)

#Análisis
if (resultado_chi_cuadrado$p.value < 0.05) {
  cat("Hay una correlación significativa entre la causa de divorcio y la parroquia inscrita.")
} else {
  cat("No hay suficiente evidencia para afirmar una correlación significativa.")
}

# Estadística de Prueba: El valor de la estadística de prueba chi-cuadrado es 76.605.
# 
# Grados de Libertad (df): Hay 7 grados de libertad en este caso.
# 
# Valor p (p-value): El valor p asociado con la prueba de chi-cuadrado es 6.769e-14.
# 
# La hipótesis nula H0 en la prueba de chi-cuadrado es que no hay relación entre las variables (son independientes).
# 
# El valor p es 6.769e-14., lo cual es mucho menor que un nivel de significancia común de 0.05. Por lo tanto, hay evidencia suficiente para rechazar la hipótesis nula.

#REGRESION LINEAL

#La duración del matrimonio depende de la cantidad de hijos

regresion <- lm(filas_guayas$dur_mat ~ filas_guayas$hijos_2  , data = filas_guayas)
summary(regresion)

plot(filas_guayas$hijos_2, filas_guayas$dur_mat, xlab='Duración del matrimonio en años', ylab='Cantidad de hijos')
abline(regresion)

anova(regresion)

#Ho = pendientes es igual a cero

#H1 = pendientes es diferente a cero

#Verificación de supuestos

par(mfrow = c(2, 2))
plot(regresion)

install.packages("visreg")
library(visreg)
# Notese que solo requiere incluir la variable independiente, para graficar.
#visreg(regresion, "hijos_2", partial = F)  #regresion es nuestro vector que contiene la ecuacion de regresion.

install.packages("ggplot2")
library(ggplot2)

tabla_regresion <- data.frame(filas_guayas$dur_mat, filas_guayas$hijos_2)

ggplot(tabla_regresion, aes(x=hijos_2, y=dur_mat)) +
  geom_point(shape=1) +    # genera circulos en el grafico
  geom_smooth(method=lm)   # adjunta la linea de regresion por defecto es al 95% de confianza

shapiro.test(resid(regresion))  #Normalidad

install.packages("car")
library(car)
ncvTest(regresion)  #Prueba de heterocedasticidad, Ho=varianzas son constantes 

#Cálculo de predicciones

nuevos.datos <- data.frame(hijos_2 = seq(0, 10))
predict(regresion, nuevos.datos)

#Diagnóstico del modelo

residuos <- rstandard(regresion)
valores.ajustados <- fitted(regresion)
plot(valores.ajustados, residuos)

qqnorm(residuos)
qqline(residuos)


##################################################
#Documento final-------
########################################################
#Xi cuadrado para media
#H0= la media de la edad del hombre es igual a la media de la edad de la Mujer
#H1= la media de la edad del hombre es diferente a la media de la edad de la mujer



#########################################################
#Xi cuadrado para Varianza
#Ho= la varianza es igual
#H1= la varianza es diferente

#Variables: parroquia del hombre, parroquia de la mujer, se compara con la edad de hombre y edad de mujer

#Varianzas Hipotesis
# Seleccionar las variables de interés

edades_hombres <- DatosUltimoFiltro$edad_1
edades_mujeres <- DatosUltimoFiltro$edad_2
parroquia_hombre<- DatosUltimoFiltro$parr_hab1
parroquia_mujer<- DatosUltimoFiltro$parr_hab2

# Crear una tabla de contingencia para la parroquia habitada
tabla_contingencia <- table(DatosUltimoFiltro$parr_hab1,DatosUltimoFiltro$parr_hab2)
print(tabla_contingencia)

prop.table(tabla_contingencia) # Porcentaje de las celdas

# Realizar la prueba de chi-cuadrado para comparar las distribuciones de nivel de instrucción
prueba_chi <- chisq.test(tabla_contingencia)
print(prueba_chi)

#mosaicplot(, color=TRUE, main="")




# Obtener las muestras de las edades para cada grupo y nivel de instrucción
# Obtener las muestras de las edades para cada grupo y nivel de instrucción
edades_hombre_parroquiaH <- DatosUltimoFiltro$edad_1[DatosUltimoFiltro$parr_hab1 == "Tarqui"]
edades_mujer_ParroquiaH <- DatosUltimoFiltro$edad_2[DatosUltimoFiltro$parr_hab2 == "Tarqui"]
edades_hombre_ParroquiaM <- DatosUltimoFiltro$edad_1[DatosUltimoFiltro$parr_hab1 == "Tarqui"]
edades_mujer_ParroquiaM <- DatosUltimoFiltro$edad_2[DatosUltimoFiltro$parr_hab2 == "Tarqui"]

# Verificar el tamaño de las muestras
print("Tamaño de la muestra para el grupo 1 y nivel de instrucción 1:")
print(length(edades_hombre_parroquiaH))

print("Tamaño de la muestra para el grupo 2 y nivel de instrucción 1:")
print(length(edades_mujer_ParroquiaH))

# Realizar la prueba de Fisher para comparar las varianzas
resultados_edades_hombre_parroquiaH <- var.test(edades_hombre_parroquiaH, edades_hombre_ParroquiaM)
resultados_edades_mujer_ParroquiaH <- var.test(edades_mujer_ParroquiaH, edades_mujer_ParroquiaM)

# Mostrar los resultados de las pruebas
print("Prueba de Fisher para parroquia del Hombre:")
print(resultados_edades_hombre_parroquiaH)
print("Prueba de Fisher para parroquia de lamujer:")
print(resultados_edades_mujer_ParroquiaH)

if (resultados_edades_hombre_parroquiaH$p.value < 0.05) {
  cat("Hay evidencia para rechazar la hipótesis nula.\n")
} else {
  cat("No hay evidencia suficiente para rechazar la hipótesis nula.\n")
}

if (resultados_edades_mujer_parroquiaH$p.value < 0.05) {
  cat("Hay evidencia para rechazar la hipótesis nula.\n")
} else {
  cat("No hay evidencia suficiente para rechazar la hipótesis nula.\n")
}

# Gráfico de Boxplot para comparar las varianzas en diferentes parroquias
par(mfrow=c(1,2))  # Dividir la ventana gráfica en dos
boxplot(edades_hombre_parroquiaH, edades_hombre_ParroquiaM, names=c("Hombres - Parroquia H", "Hombres - Parroquia M"), col=c("blue", "green"), main="Varianza en Parroquia Hombres", ylab="Edad")
boxplot(edades_mujer_ParroquiaH, edades_mujer_ParroquiaM, names=c("Mujeres - Parroquia H", "Mujeres - Parroquia M"), col=c("pink", "purple"), main="Varianza en Parroquia Mujeres", ylab="Edad")



#####################################################
#Xi cuadrado para proporcion
#H0=No existe una diferencia significativa de los matrimonios con parroquia de hombre,
#respecto a los matrimonios con parroquia de mujer
# H1= existe una diferencia significativa de los matrimonios con parroquia de hombre,
#respecto a los matrimonios con parroquia de mujer

# Datos observados (en formato decimal)
matrimonios_hombres = c(1, 0, 0, 0)
matrimonios_mujeres = c(0.84, 0.03, 0.03, 0.09)

# Crear una tabla de contingencia
tabla_contingenciaP = matrix(c(matrimonios_hombres, matrimonios_mujeres), nrow = 2)
print(tabla_contingenciaP)

# Obtener estimación puntual de la diferencia de proporciones
estimacion_puntual = mean(matrimonios_hombres) - mean(matrimonios_mujeres)

# Calcular el error estándar de la diferencia de proporciones
n_hombres = sum(matrimonios_hombres)
n_mujeres = sum(matrimonios_mujeres)
error_estandar = sqrt((mean(matrimonios_hombres) * (1 - mean(matrimonios_hombres)) / n_hombres) +
                        (mean(matrimonios_mujeres) * (1 - mean(matrimonios_mujeres)) / n_mujeres))

# Calcular el intervalo de confianza manualmente
z_value = qnorm(0.975)  # Z-value para un intervalo de confianza del 95%
intervalo_confianza = c(estimacion_puntual - z_value * error_estandar, estimacion_puntual + z_value * error_estandar)

# Imprimir los resultados
print(paste("Estimación puntual de la diferencia de proporciones:", estimacion_puntual))
print(paste("Error estándar de la diferencia de proporciones:", error_estandar))
print(paste("Intervalo de confianza del 95%:", intervalo_confianza))

# Crear un dataframe con los datos
datos_proporciones <- data.frame(
  Grupo = c("Matrimonios con Parroquia de Hombres", "Matrimonios con Parroquia de Mujeres"),
  Proporciones = c(mean(matrimonios_hombres), mean(matrimonios_mujeres)),
  Error = c(error_estandar, error_estandar)
)

# Crear un gráfico de barras con barras de error
barplot(datos_proporciones$Proporciones, names.arg = datos_proporciones$Grupo, ylim = c(0, 1),
        col = c("blue", "pink"), main = "Diferencia en Proporciones", ylab = "Proporción",
        sub = paste("Intervalo de Confianza (95%): [", round(intervalo_confianza[1], 3), ", ", round(intervalo_confianza[2], 3), "]"))

# Agregar barras de error al gráfico
arrows(1, datos_proporciones$Proporciones, 1, datos_proporciones$Proporciones + datos_proporciones$Error, angle = 90, code = 3, length = 0.1)
arrows(1, datos_proporciones$Proporciones, 1, datos_proporciones$Proporciones - datos_proporciones$Error, angle = 90, code = 3, length = 0.1)


# Crear un dataframe con los datos de la tabla de contingencia
datos_chi_cuadrado <- as.data.frame(tabla_contingenciaP)
rownames(datos_chi_cuadrado) <- c("Hombres", "Mujeres")

# Crear un gráfico de barras apiladas
barplot(as.matrix(datos_chi_cuadrado), beside = TRUE, col = c("blue", "pink"),
        main = "Distribución de Parroquias por Género", ylab = "Frecuencia", legend.text = TRUE)

# Agregar leyenda
legend("topright", legend = rownames(datos_chi_cuadrado), fill = c("blue", "pink"))



#Documento final-------
########################################################
#Xi cuadrado para media
#H0= la media de la edad del hombre es igual a la media de la edad de la Mujer
#H1= la media de la edad del hombre es diferente a la media de la edad de la mujer
# Datos de edad para hombres y mujeres (reemplaza con tus propios valores)
edad_hombres = c(DatosUltimoFiltro$edad_1)
edad_mujeres = c(DatosUltimoFiltro$edad_2)

# Realizar la prueba t de Student
resultado_prueba_t = t.test(edad_hombres, edad_mujeres)

# Imprimir los resultados
cat("Resultado de la prueba t:\n")
print(resultado_prueba_t)

# Imprimir conclusión basada en el valor p
cat("\nConclusión:\n")
if (resultado_prueba_t$p.value < 0.05) {
  cat("Hay evidencia para rechazar la hipótesis nula.\n")
} else {
  cat("No hay evidencia suficiente para rechazar la hipótesis nula.\n")
}


# Crear un dataframe con los datos
datos_edades <- data.frame(
  Grupo = c("Hombres", "Mujeres"),
  Edades = c(mean(edad_hombres), mean(edad_mujeres)),
  Error = c(sd(edad_hombres), sd(edad_mujeres))  # Desviación estándar como error para la barra
)

# Crear un gráfico de barras con barras de error
barplot(datos_edades$Edades, names.arg = datos_edades$Grupo, ylim = c(0, max(datos_edades$Edades) + max(datos_edades$Error)),
        col = c("blue", "pink"), main = "Comparación de Medias de Edades", ylab = "Edad",
        sub = paste("Prueba t: p =", resultado_prueba_t$p.value))

# Agregar barras de error al gráfico
arrows(1, datos_edades$Edades, 1, datos_edades$Edades + datos_edades$Error, angle = 90, code = 3, length = 0.1)
arrows(1, datos_edades$Edades, 1, datos_edades$Edades - datos_edades$Error, angle = 90, code = 3, length = 0.1)
