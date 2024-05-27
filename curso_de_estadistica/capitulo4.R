# 44. Consulte los datos Real Estate, que incluyen información sobre las casas vendidas en Goodyear,
# Arizona, el año pasado. Prepare un reporte sobre los precios de venta de las casas. Asegúrese de
# responder en su informe las siguientes preguntas:
#   a) Elabore un diagrama de caja. Estime el primer y tercer cuartiles. ¿Hay datos atípicos?
#   b) Desarrolle un diagrama de dispersión con el precio en el eje vertical y el tamaño de la casa en
# el horizontal. ¿Le parece que hay alguna relación entre las dos variables? ¿La relación es directa o inversa?
#   c) Elabore un diagrama de dispersión con el precio en el eje vertical y la distancia al centro de la
# ciudad en el horizontal. ¿Parece que hay alguna relación entre las dos variables? ¿La relación
# es directa o inversa?


# Cargar los datos de Real Estate
real_estate <- read.csv("goodyearArizona.txt", header = TRUE, sep = "")

# a) Diagrama de caja para los precios de venta
boxplot(real_estate$Precio, main = "Diagrama de caja de los precios de venta", ylab = "Precio de venta")
primer_cuartil <- quantile(real_estate$Precio, 0.25)
tercer_cuartil <- quantile(real_estate$Precio, 0.75)

# Identificación de datos atípicos
datos_atipicos <- boxplot.stats(real_estate$price)$out

cat("Primer cuartil:", primer_cuartil, "\n")
cat("Tercer cuartil:", tercer_cuartil, "\n")
cat("Datos atípicos:", datos_atipicos, "\n")

# b) Diagrama de dispersión del precio vs. tamaño de la casa
plot(real_estate$Tamano, real_estate$Precio, main = "Dispersión del precio vs. tamaño de la casa",
     xlab = "Tamaño de la casa (pies cuadrados)", ylab = "Precio de venta")
cor_size_price <- cor(real_estate$Tamano, real_estate$Precio)
cat("Correlación entre tamaño de la casa y precio de venta:", cor_size_price, "\n")

# c) Diagrama de dispersión del precio vs. distancia al centro de la ciudad
plot(real_estate$DistanciaCentro, real_estate$Precio, main = "Dispersión del precio vs. distancia al centro de la ciudad",
     xlab = "Distancia al centro de la ciudad (millas)", ylab = "Precio de venta")
cor_distance_price <- cor(real_estate$DistanciaCentro, real_estate$Precio)
cat("Correlación entre distancia al centro de la ciudad y precio de venta:", cor_distance_price, "\n")

#   45. Busque en Baseball 2009 la información sobre los 30 mejores equipos de la Liga Mayor en la temporada 2009.
# a) Seleccione la variable que se refiere al año en que el estadio fue construido. (Sugerencia: Reste
#                                                                                    el año en el que el estadio se construyó del año actual para determinar la edad del estadio, y
#                                                                                    trabaje con esta variable.) Diseñe un diagrama de caja ¿Hay datos atípicos?
#   b) Seleccione la variable relacionada con el salario del equipo y diseñe un diagrama de caja. ¿Hay
# datos atípicos? ¿Cuáles son los cuartiles? Redacte un breve resumen de su análisis. ¿Cómo se
# comparan los salarios de los Yanquis de Nueva York con los otros equipos?
#   c) Trace un diagrama de dispersión en cuyo eje vertical se indique el número de juegos ganados
# y el salario del equipo en el eje horizontal. ¿Cuáles son sus conclusiones?
#   d) Seleccione la variable juegos ganados. Trace un diagrama de puntos. ¿Qué conclusiones
# puede obtener a partir de esta gráfica?

# Cargar los datos de Baseball 2009
baseball_2009 <- read.csv("baseball.txt", header = TRUE, sep = "")

# a) Diagrama de caja para la edad del estadio
current_year <- 2024
baseball_2009$stadium_age <- current_year - baseball_2009$Construcción.
boxplot(baseball_2009$stadium_age, main = "Diagrama de caja de la edad del estadio", ylab = "Edad del estadio")
primer_cuartil_age <- quantile(baseball_2009$stadium_age, 0.25)
tercer_cuartil_age <- quantile(baseball_2009$stadium_age, 0.75)
datos_atipicos_age <- boxplot.stats(baseball_2009$stadium_age)$out

cat("Primer cuartil (edad del estadio):", primer_cuartil_age, "\n")
cat("Tercer cuartil (edad del estadio):", tercer_cuartil_age, "\n")
cat("Datos atípicos (edad del estadio):", datos_atipicos_age, "\n")

# b) Diagrama de caja para los salarios de los equipos
boxplot(baseball_2009$Salario., main = "Diagrama de caja de los salarios de los equipos", ylab = "Salario del equipo")
primer_cuartil_salary <- quantile(baseball_2009$Salario., 0.25)
tercer_cuartil_salary <- quantile(baseball_2009$Salario., 0.75)
datos_atipicos_salary <- boxplot.stats(baseball_2009$Salario.)$out

cat("Primer cuartil (salarios):", primer_cuartil_salary, "\n")
cat("Tercer cuartil (salarios):", tercer_cuartil_salary, "\n")
cat("Datos atípicos (salarios):", datos_atipicos_salary, "\n")

# Comparación de los salarios de los Yanquis de Nueva York con otros equipos
salario_yankees <- baseball_2009$Salario.[baseball_2009$quipo. == "New York Yankees"]
cat("Salario de los Yanquis de Nueva York:", salario_yankees, "\n")

# c) Diagrama de dispersión del número de juegos ganados vs. salario del equipo
plot(baseball_2009$Salario., baseball_2009$Victorias., main = "Dispersión del salario del equipo vs. juegos ganados",
     xlab = "Salario del equipo", ylab = "Número de juegos ganados")
cor_salary_wins <- cor(baseball_2009$Salario., baseball_2009$Victorias.)
cat("Correlación entre salario del equipo y juegos ganados:", cor_salary_wins, "\n")

# d) Diagrama de puntos del número de juegos ganados
plot(baseball_2009$Victorias., main = "Diagrama de puntos del número de juegos ganados",
     xlab = "Índice del equipo", ylab = "Número de juegos ganados", pch = 19)
summary(baseball_2009$Victorias.)



#   46. Consulte los datos sobre los autobuses que operan en el distrito escolar Buena.
# a) Refiérase a la variable costo de mantenimiento. Desarrolle un diagrama de caja. ¿Cuáles son el
# primer y tercer cuartiles? ¿Hay datos atípicos?
#   b) Determine el costo mediano de mantenimiento. Basándose en la mediana, desarrolle una tabla
# de contingencias en donde el fabricante sea una variable y la otra si el costo de mante



# Cargar los datos de los autobuses del Distrito Escolar Buena
school_buses <- read.csv("schoolBus.txt", header = TRUE, sep = "")

# a) Diagrama de caja para el costo de mantenimiento
boxplot(school_buses$mantenimiento., main = "Diagrama de caja del costo de mantenimiento", ylab = "Costo de mantenimiento")
primer_cuartil_mantenimiento <- quantile(school_buses$mantenimiento., 0.25)
tercer_cuartil_mantenimiento <- quantile(school_buses$mantenimiento., 0.75)
datos_atipicos_mantenimiento <- boxplot.stats(school_buses$mantenimiento.)$out

cat("Primer cuartil (costo de mantenimiento):", primer_cuartil_mantenimiento, "\n")
cat("Tercer cuartil (costo de mantenimiento):", tercer_cuartil_mantenimiento, "\n")
cat("Datos atípicos (costo de mantenimiento):", datos_atipicos_mantenimiento, "\n")

# b) Costo mediano de mantenimiento y tabla de contingencias
costo_mediano <- median(school_buses$mantenimiento.)
school_buses$high_cost <- school_buses$mantenimiento. > costo_mediano
tabla_contingencias <- table(school_buses$Fabricante., school_buses$high_cost)
cat("Costo mediano de mantenimiento:", costo_mediano, "\n")
print(tabla_contingencias)
