#PROYECTO DE ESTADÍSTICA
#GRUPO #3

# Instalar y cargar las librerías necesarias
install.packages(c("readxl", "dplyr", "ggplot2", "modeest"))
library(readxl)
library(dplyr)
library(ggplot2)
library(modeest)

--------------------------------------------------------------------
# Leer los datos desde el archivo Excel
datos <- read_excel("Pinko.xlsx")

# Mostrar las primeras filas del dataframe para verificar que se ha cargado correctamente
head(datos)

# Reemplazar valores en la columna Inclinación
datos$Inclinación <- ifelse(datos$Inclinación == 20.03, "bajo",
                         ifelse(datos$Inclinación == 45, "medio",
                                ifelse(datos$Inclinación == 60, "alto", datos$Inclinación)))

# Verifica los cambios
table(datos$Inclinación)

# Verificar el cambio
head(datos)
---------------------------------------------------------------------
#DISTRIBUCIÓN GEOMÉTRICA
  
  # Definir la probabilidad de éxito
  p <- 0.375

# Calcular la distribución geométrica para diferentes intentos (fallos previos)
intentos <- 0:10  # Número de fallos previos (0 intentos hasta 10)
probabilidades <- dgeom(intentos, prob = p)

# Mostrar las probabilidades
data.frame(Intentos = intentos, Probabilidad = probabilidades)

# Crear un data frame con los datos
data_geom <- data.frame(Intentos = intentos, Probabilidad = probabilidades)

# Graficar la distribución geométrica
ggplot(data_geom, aes(x = Intentos, y = Probabilidad)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribución Geométrica Discreta",
       x = "Número de Fallos Previos",
       y = "Probabilidad") +
  theme_minimal()

#Comparación de probabilidades teóricas y los datos de la variable X

# Calcular frecuencias observadas de X
frecuencias_observadas <- table(datos$X)  # Tabla de frecuencias
frecuencias_observadas

# Definir la probabilidad de éxito
p <- 0.375

# Calcular las probabilidades geométricas teóricas para los valores observados en X
valores_unicos_X <- as.numeric(names(frecuencias_observadas))
probabilidades_teoricas <- dgeom(valores_unicos_X - 1, prob = p)  # -1 porque dgeom cuenta los fallos antes del éxito
valores_unicos_X
probabilidades_teoricas

# Crear un data frame para facilitar la comparación
comparacion <- data.frame(
  Intentos = valores_unicos_X,
  Frecuencia_Observada = as.numeric(frecuencias_observadas),
  Probabilidad_Teorica = probabilidades_teoricas
)

# Mostrar la comparación
print(comparacion)

# Graficar la comparación
ggplot(comparacion, aes(x = Intentos)) +
  geom_bar(aes(y = Frecuencia_Observada), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_point(aes(y = Probabilidad_Teorica * sum(frecuencias_observadas)), color = "red", size = 2) +
  labs(title = "Comparación de Frecuencias Observadas y Probabilidades Teóricas",
       x = "Número de Intentos",
       y = "Frecuencia / Probabilidad Ajustada") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~./sum(frecuencias_observadas), name = "Probabilidad Teórica"))

----------------------------------------------------------------------
# ANÁLISIS DESCRIPTIVO (Media, mediana, moda, desviación estándar, Mínimo, máximo)


# Estadísticas básicas para Pozo 1
media_pozo1 <- mean(datos$Pozo1, na.rm = TRUE)
mediana_pozo1 <- median(datos$Pozo1, na.rm = TRUE)
moda_pozo1 <- as.numeric(names(sort(table(datos$Pozo1), decreasing = TRUE))[1])
desviacion_estandar_pozo1 <- sd(datos$Pozo1, na.rm = TRUE)

# Estadísticas básicas para Pozo 2
media_pozo2 <- mean(datos$Pozo2, na.rm = TRUE)
mediana_pozo2 <- median(datos$Pozo2, na.rm = TRUE)
moda_pozo2 <- as.numeric(names(sort(table(datos$Pozo2), decreasing = TRUE))[1])
desviacion_estandar_pozo2 <- sd(datos$Pozo2, na.rm = TRUE)

# Estadísticas básicas para Pozo 3
media_pozo3 <- mean(datos$Pozo3, na.rm = TRUE)
mediana_pozo3 <- median(datos$Pozo3, na.rm = TRUE)
moda_pozo3 <- as.numeric(names(sort(table(datos$Pozo3), decreasing = TRUE))[1])
desviacion_estandar_pozo3 <- sd(datos$Pozo3, na.rm = TRUE)

# Estadísticas básicas para Pozo 4
media_pozo4 <- mean(datos$Pozo4, na.rm = TRUE)
mediana_pozo4 <- median(datos$Pozo4, na.rm = TRUE)
moda_pozo4 <- as.numeric(names(sort(table(datos$Pozo4), decreasing = TRUE))[1])
desviacion_estandar_pozo4 <- sd(datos$Pozo4, na.rm = TRUE)

# Estadísticas básicas para Pozo 5
media_pozo5 <- mean(datos$Pozo5, na.rm = TRUE)
mediana_pozo5 <- median(datos$Pozo5, na.rm = TRUE)
moda_pozo5 <- as.numeric(names(sort(table(datos$Pozo5), decreasing = TRUE))[1])
desviacion_estandar_pozo5 <- sd(datos$Pozo5, na.rm = TRUE)

# Estadísticas básicas para X
media_X <- mean(datos$X, na.rm = TRUE)
mediana_X <- median(datos$X, na.rm = TRUE)
moda_X <- as.numeric(names(sort(table(datos$X), decreasing = TRUE))[1])
desviacion_estandar_X <- sd(datos$X, na.rm = TRUE)

# Imprimir los resultados
cat("Estadísticas para Pozo 1:\n")
cat("Media:", media_pozo1, "\n")
cat("Mediana:", mediana_pozo1, "\n")
cat("Moda:", moda_pozo1, "\n")
cat("Desviación Estándar:", desviacion_estandar_pozo1, "\n\n")

cat("Estadísticas para Pozo 2:\n")
cat("Media:", media_pozo2, "\n")
cat("Mediana:", mediana_pozo2, "\n")
cat("Moda:", moda_pozo2, "\n")
cat("Desviación Estándar:", desviacion_estandar_pozo2, "\n\n")

cat("Estadísticas para Pozo 3:\n")
cat("Media:", media_pozo3, "\n")
cat("Mediana:", mediana_pozo3, "\n")
cat("Moda:", moda_pozo3, "\n")
cat("Desviación Estándar:", desviacion_estandar_pozo3, "\n\n")

cat("Estadísticas para Pozo 4:\n")
cat("Media:", media_pozo4, "\n")
cat("Mediana:", mediana_pozo4, "\n")
cat("Moda:", moda_pozo4, "\n")
cat("Desviación Estándar:", desviacion_estandar_pozo4, "\n\n")

cat("Estadísticas para Pozo 5:\n")
cat("Media:", media_pozo5, "\n")
cat("Mediana:", mediana_pozo5, "\n")
cat("Moda:", moda_pozo5, "\n")
cat("Desviación Estándar:", desviacion_estandar_pozo5, "\n\n")

cat("Estadísticas para X:\n")
cat("Media:", media_X, "\n")
cat("Mediana:", mediana_X, "\n")
cat("Moda:", moda_X, "\n")
cat("Desviación Estándar:", desviacion_estandar_X, "\n")

summary(datos)
-------------------------------------------------------------------
  
# DIAGRAMA DE CAJAS PARA LOS POZOS
  
boxplot(datos$Pozo1, datos$Pozo2, datos$Pozo3, datos$Pozo4, datos$Pozo5,
        main = "Distribución de los lanzamientos de la pelota en los diferentes pozos",
        loc = c(1,2,4,5),
        names = c("Pozo 1", "Pozo 2", "Pozo 3", "Pozo 4", "Pozo 5"),
        las = 2,
        col = c("orange","red"),
        border = "black",
        horizontal = TRUE,
        notch = FALSE
)

# DIAGRAMA DE CAJAS PARA X
boxplot(datos$X,
        horizontal = TRUE,
        main = "Distribución del conjunto de datos de la variable X",
        ylab = "X",
        col = "burlywood2",
        border = "black")

# HISTOGRAMAS para cada pozo y X
# Pozo 1
ggplot(datos, aes(x = Pozo1)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Ingresos de la pelota en el pozo 1", x = "Número de veces que ingresó la pelota antes del éxito", y = "Frecuencia")

# Pozo 2
ggplot(datos, aes(x = Pozo2)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Ingresos de la pelota en el pozo 2", x = "Número de veces que ingresó la pelota antes del éxito", y = "Frecuencia")

# Pozo 3
ggplot(datos, aes(x = Pozo3)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Ingresos de la pelota en el pozo 3", x = "Número de veces que ingresó la pelota antes del éxito", y = "Frecuencia")

# Pozo 4
ggplot(datos, aes(x = Pozo4)) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod", color = "black") +
  labs(title = "Ingresos de la pelota en el pozo 4", x = "Número de veces que ingresó la pelota antes del éxito", y = "Frecuencia")

# Pozo 5
ggplot(datos, aes(x = Pozo5)) +
  geom_histogram(binwidth = 1, fill = "lightpink", color = "black") +
  labs(title = "Ingresos de la pelota en el pozo 5", x = "Número de veces que ingresó la pelota antes del éxito", y = "Frecuencia")

# Histograma de X
ggplot(datos, aes(x = X)) +
  geom_histogram(binwidth = 1, fill = "Orange", color = "black") +
  labs(title = "Números de intentos hasta el éxito", x = "X", y = "Frecuencia")

----------------------------------------------------------------------------
  #VARIABLE CUANTITATIVA VS CUALITATIVA
  
  #X vs Inclinación
  ggplot(data = datos, aes(x = log(X), y = Inclinación, fill = Inclinación)) + 
  geom_boxplot() + labs(x = "Intentos hasta el éxito", y = "Inclinación", title = "Intentos hasta el éxito por tipo de inclinación" ) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  theme_light()

# Gráfico para visualizar la relación entre X e Inclinación
boxplot(X ~ Inclinación, data = datos, main = "Relación entre X e Inclinación", 
        xlab = "Inclinación", ylab = "X", col = c("lightblue", "lightgreen", "lightpink"))


# X vs Método

ggplot(data = datos, aes(x = log(X), y = Método, fill = Método)) + 
  geom_boxplot() + labs(x = "Intentos hasta el éxito", y = "Método", title = "Intentos hasta el éxito por el tipo de ingreso de la pelota al plinko" ) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  theme_light()

# Gráfico para visualizar la relación entre X y Método
boxplot(X ~ Método, data = datos, main = "Relación entre X y Método", 
        xlab = "Método", ylab = "X", col = c("lightgreen", "lightpink"))

------------------------------------------------------------------------------
#REGRESIÓN LINEAL SIMPLE

# Realizar la regresión lineal simple con X como variable dependiente e Inclinación como independiente
modelo <- lm(X ~ Inclinación, data = datos)

# Resumen del modelo
summary(modelo)

# Visualización de los coeficientes
coef(modelo)

# Realizar la regresión lineal simple con X como variable dependiente y Método como independiente
modelo_2 <- lm(X ~ Método, data = datos)

# Resumen del modelo
summary(modelo_2)

# Visualización de los coeficientes
coef(modelo_2)


# Filtrar los datos por cada nivel de inclinación
data_bajo <- subset(datos, Inclinación == "bajo")
data_medio <- subset(datos, Inclinación == "medio")
data_alto <- subset(datos, Inclinación == "alto")

# Regresión lineal para cada nivel de inclinación
modelo_bajo <- lm(X ~ 1, data = data_bajo)
modelo_medio <- lm(X ~ 1, data = data_medio)
modelo_alto <- lm(X ~ 1, data = data_alto)

# Resúmenes de los modelos
summary(modelo_bajo)
summary(modelo_medio)
summary(modelo_alto)

# Gráfico de dispersión con las regresiones
ggplot(datos, aes(x = Inclinación, y = X, color = Inclinación)) +
  geom_point() +
  geom_smooth(data = data_bajo, aes(x = Inclinación, y = X), method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = data_medio, aes(x = Inclinación, y = X), method = "lm", se = FALSE, color = "green") +
  geom_smooth(data = data_alto, aes(x = Inclinación, y = X), method = "lm", se = FALSE, color = "red") +
  labs(title = "Regresiones Lineales por Nivel de Inclinación",
       x = "Inclinación",
       y = "X") +
  theme_minimal()
  --------------------------------------------------------------------
#TABLA DE CONTIGENCIA
   
  # Tabla de frecuencia para la variable categórica Inclinación
  frecuencia_inclinacion <- table(datos$Inclinación)
frecuencia_inclinacion

# Tabla de frecuencias relativas para Inclinación
frecuencia_relativa_inclinacion <- prop.table(frecuencia_inclinacion)
frecuencia_relativa_inclinacion

# Si tienes otras variables categóricas, puedes hacer lo mismo para ellas.
# Por ejemplo, para una variable llamada 'Método':
frecuencia_metodo <- table(datos$Método)
frecuencia_metodo

# Tabla de frecuencias relativas para Método
frecuencia_relativa_metodo <- prop.table(frecuencia_metodo)
frecuencia_relativa_metodo

#tabla de contingencia para Inclinación y Método
tabla_contingencia <- table(datos$Inclinación, datos$Método)
tabla_contingencia

#Tabla de conteos marginales
(tabla_marginales <- addmargins(tabla_contingencia))

#proporciones relativas
tabla_proporciones <- prop.table(tabla_contingencia)
tabla_proporciones

(tabla_marginales_proporciones <- addmargins(tabla_proporciones))

barplot(tabla_contingencia,
        col = c(2:4),
        beside = TRUE,
        ylim = c(0, 60), ylab = "Número de intentos", main = "Tipo de Inclinación usado respecto al tipo de ingreso de la pelota");legend(x = "topright",legend = unique(datos$Inclinación),  fill = c(2:4))

#PRUEBA DE INDEPENDENCIA
# Crear la tabla de contingencia
datos <- matrix(c(179, 178, 185, 169, 183, 182), nrow = 2, byrow = TRUE)

# Asignar nombres a las filas y columnas
rownames(datos) <- c("rodando", "tirando")
colnames(datos) <- c("baja", "medio", "alta")

# Ver la tabla de contingencia
print(datos)

# Prueba de chi-cuadrado
resultado <- chisq.test(datos)
print(resultado)
---------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
#CHI CUADRADO Bondad de ajuste
  
#Hipótesis nula: Los datos siguen una distribución geométrica con parámetro p
#Hipótesis alternativa: Los datos no siguen una distribución geométrica con parámetro p
  
observed_frequencies <- table(datos$X)
expected_frequencies <- dgeom(as.numeric(names(observed_frequencies)) - 1, prob = p) * length(datos$X)
resultado_chi_cuadrado<-chisq.test(observed_frequencies, p = expected_frequencies / sum(expected_frequencies))
observed_frequencies
expected_frequencies
print(resultado_chi_cuadrado)
#Análisis
if (resultado_chi_cuadrado$p.value < 0.05) {
  cat("Los datos no siguen una distribución geométrica con parámetro p")
} else {
  cat("Los datos siguen una distribución geométrica con parámetro p")
}

#PRUEBA DE NORMALIDAD CON KOLMOGOROV SMIRNOV

# Realizar la prueba de Kolmogorov-Smirnov para comparar la muestra con una distribución normal
ks_result <- ks.test(datos$X, "pnorm", mean = mean(datos$X), sd = sd(datos$X))

# Resultado:
# H₀: La distribución de los datos sigue una distribución normal.
# H₁: La distribución de los datos no sigue una distribución normal.


# Graficar la distribución observada y la distribución teórica
ggplot(datos, aes(sample = X)) +
  stat_qq(distribution = qnorm, dparams = list(mean = mean(datos$X), sd = sd(datos$X))) +
  stat_qq_line(distribution = qnorm, dparams = list(mean = mean(datos$X), sd = sd(datos$X)), color = "red") +
  labs(title = "Prueba de Kolmogorov-Smirnov: QQ Plot",
       subtitle = paste("p-value:", round(ks_result$p.value, 5)),
       x = "Cuantiles Teóricos",
       y = "Cuantiles Observados") +
  theme_minimal()

----------------------------------------------------------------------
  
-----------------------------------------------------------------------------
#PRUEBAS DE HIPÓTESIS

#Pruebas Normalidad
  
qqnorm(datos$X)
qqline(datos$X)

set.seed(41)
mx <- sample(x = datos$X, size = 64)

#muestra

qqnorm(mx)
qqline(mx)

#Prueba KS

?ks.test
ks.test(x = datos$X, "pnorm", 3, 2)

#Normalidad de la muestra

chisq.test(mx)

ks.test(mx, "pnorm", 3, 2)

#Prueba de una sola media

#H0: mX = 3.5
#Ha: mX != 3.5

set.seed(41)
mx <- sample(x = datos$X , size = 64)

hist(mx)

mean(mx)
sd(mx)

?t.test
t.test(x = mx, 
       alternative = "two.sided",
       mu = 3.5, conf.level = .95)


#Prueba de 2 medias

mean(datos$X, na.rm = T)

hist(datos$X)
boxplot(datos$X)

?qqnorm
qqnorm(datos$X, main = "Gráfica normal QQ para la variable X", xlab = "Cuantiles teóricos", ylab = "Cuantiles observados de X")
qqline(datos$X)


#Separar los datos

library(dplyr)

bajo <- datos %>%
  filter(Inclinación == "bajo") %>%
  select(X) %>%
  unlist()


medio <- datos%>%
  filter(Inclinación == "medio") %>%
  select(X) %>%
  unlist()

mean(bajo, na.rm = T)
mean(medio, na.rm = T)

sd(bajo, na.rm = T)
sd(medio, na.rm = T)

hist(bajo)
boxplot(bajo)

qqnorm(bajo)
qqline(bajo)


hist(medio)
boxplot(medio)

qqnorm(medio)
qqline(medio)

#Pruebas de normalidad

ks.test(x = bajo, "pnorm",3.5, 2)
ks.test(x = medio, "pnorm",3.5, 2)

#Prueba de varianzas

#H0: var bajo == var medio
#Ha: var bajo != var medio
?var.test

var.test(x = medio, y = bajo, ratio = 1, 
         alternative = "two.sided", conf.level = .95)

#Prueba de medias

#H0: mu_bajo == mu_medio
#Ha: mu_bajo != mu_medio

?t.test
t.test(x = medio, y = bajo, 
       alternative = "two.sided", 
       mu = 0, paired = F, 
       var.equal = T, conf.level = .95)

