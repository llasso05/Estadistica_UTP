# 51. Consulte los datos de inmobiliarias que aparecen en el apéndice A,
# al final del libro, los cuales contienen información sobre las casas
# vendidas en el área de Goodyear, Arizona, el año pasado.
# Seleccione un intervalo de clase apropiado, y organice los precios de
#venta en una distribución de frecuencias. Escriba un breve reporte
# que resuma sus resultados.
# Asegúrese de contestar las
# siguientes preguntas en dicho reporte.
# a) ¿Alrededor de qué valores tienden a acumularse los datos?
#   b) ¿Cuál es el precio de venta más alto? ¿Cuál es el precio de venta
#        más bajo?
#   c) Elabore una distribución de frecuencias acumulativas.
#      ¿Cuántas casas se vendieron en menosde $200 000? Calcule el porcentaje
#       de casas que se vendieron en más de $220 000. ¿Qué porcentaje de casas
#       se vendió en menos de $125 000?
#   d) Remítase a la variable con respecto a los municipios. Elabore una gráfica
#      de barras que muestre el número de casas vendidas en cada municipio.
#      ¿Existen diferencias o el número de casas
#       que se vendieron en cada municipio es más o menos igual?
#    52. Consulte los datos Baseball 2009, los cuales contienen información
#        sobre los 30 equipos de las Ligas Mayores de Béisbol durante
#        la temporada 2009. Seleccione un intervalo de clase apropiado y
#        organice la información sobre los salarios de los equipos en una
#        distribución de frecuencias.
#        a) ¿Cuál es el salario típico de un equipo? ¿Cuál es el rango de
#           salarios?
#        b) Comente la forma de la distribución. ¿Parece que alguno de
#           los salarios de los equipos no se encuentra en línea con los
#           demás?
#        c) Diseñe una distribución de frecuencias acumulativas.
#           ¿Cuarenta por ciento de los equipos pagan menos que cuál
#           cantidad del salario total del equipo? ¿Cuántos equipos
#           aproximadamente tiene salarios totales inferiores a $80 000 000?
#           31 49 19 62 24 45 23 51 55 60
#           40 35 54 26 57 37 43 65 18 41
#           50 56 4 54 39 52 35 51 63 42
#           47 1 8 46 76 26 4 3 39 45
#           4 21 80 63 100 65 91 29 7 15
#           7 52 87 39 106 25 55 2 3 8
#           14 38 59 33 76 71 37 51 1 24
#           35 86 185 13 7 43 36 20 79 9

# Cargar los datos inmobiliarios
real_estate <- read.csv("goodyearArizona.txt", header = TRUE, sep = "")

# Seleccionar la columna de precios de venta
precios <- real_estate$Precio

# Seleccionar un intervalo de clase apropiado
intervalo <- 25000

# Organizar los precios de venta en una distribución de frecuencias
breaks <- seq(floor(min(precios)), ceiling(max(precios)), by = intervalo)
frecuencia <- cut(precios, breaks = breaks,
                  right = FALSE, include.lowest = TRUE)
tabla_frecuencia <- table(frecuencia)

# a) Alrededor de qué valores tienden a acumularse los datos
precio_medio <- mean(precios)
precio_mediano <- median(precios)

# b) Precio de venta más alto y más bajo
precio_max <- max(precios)
precio_min <- min(precios)

# c) Distribución de frecuencias acumulativas
tabla_frecuencia_acumulada <- cumsum(tabla_frecuencia)
casas_menor_200k <- sum(precios < 200000)
porcentaje_mayor_220k <- sum(precios > 220000) / length(precios) * 100
porcentaje_menor_125k <- sum(precios < 125000) / length(precios) * 100

# d) Gráfica de barras del número de casas vendidas en cada municipio
library(ggplot2)
grafica_municipios <- ggplot(real_estate, aes(x = Colonia)) +
  geom_bar() +
  labs(title = "Número de casas vendidas por municipio",
       x = "Municipio",
       y = "Número de casas vendidas")

# Mostrar la gráfica
print(grafica_municipios)

# Resumen del reporte
reporte <- paste(
  "Informe sobre los precios de venta de casas en Goodyear, Arizona:

  a) Los datos tienden a acumularse alrededor de los siguientes valores:
     - Precio medio de venta: $", round(precio_medio, 2), "
     - Precio mediano de venta: $", round(precio_mediano, 2), "
     - La medida más representativa del costo típico es el precio 
     mediano, ya que no se ve afectada por valores atípicos tanto 
     como el precio medio.

  b) El precio de venta más alto fue de $", precio_max, "
     El precio de venta más bajo fue de $", precio_min, "

  c) Distribución de frecuencias acumulativas:
     - Número de casas vendidas en menos de $200,000: ", casas_menor_200k, "
     - Porcentaje de casas que se vendieron en más de 
     $220,000: ", round(porcentaje_mayor_220k, 2), "%
     - Porcentaje de casas que se vendieron en menos de 
     $125,000: ", round(porcentaje_menor_125k, 2), "%

  d) Número de casas vendidas por municipio:
     (ver gráfica adjunta)"
)
cat(reporte)


# Ejercicios de la base de datos 53
# 
# 53. Consulte los datos de los autobuses del Distrito Escolar Buena. Seleccione la variable que se refiere al número de millas que recorrieron el mes pasado, y organice estos datos en una distribución
# de frecuencias.
# a) ¿Cuál es la cantidad típica de millas recorridas? ¿Cuál es el rango?
#   b) Comente la forma de la distribución. ¿Existen valores atípicos en términos de millas conducidas?
#   c) Diseñe una distribución de frecuencias acumulativas. ¿Cuarenta por ciento de los autobuses
# fueron conducidos durante menos de cuántas millas? ¿Cuántos autobuses fueron conducidos
# menos de 850 millas?
#   d) Consulte las variables con respecto al tipo de autobús y al númerode asientos en cada uno.
# Elabore una gráfica de pastel de cada variable y comente sus hallazgos.

# Cargar los datos de béisbol
baseball_2009 <- read.csv("baseball.txt", header = TRUE, sep = "")

# Seleccionar la columna de salarios de los equipos
salarios <- baseball_2009$Salario.

# Seleccionar un intervalo de clase apropiado
intervalo <- 10000000

# Organizar la información sobre los salarios de los equipos en una distribución de frecuencias
breaks <- seq(floor(min(salarios)), ceiling(max(salarios)), by = intervalo)
frecuencia <- cut(salarios, breaks = breaks, right = FALSE, include.lowest = TRUE)
tabla_frecuencia <- table(frecuencia)

# a) Salario típico de un equipo y rango de salarios
salario_medio <- mean(salarios)
rango_salarios <- range(salarios)

# b) Forma de la distribución
hist(salarios, breaks = breaks, main = "Distribución de Salarios de los Equipos", xlab = "Salarios", ylab = "Frecuencia")

# c) Distribución de frecuencias acumulativas
tabla_frecuencia_acumulada <- cumsum(tabla_frecuencia)
cuarenta_por_ciento <- quantile(salarios, 0.4)
equipos_menos_80m <- sum(salarios < 80000000)

# Resumen del reporte
reporte_baseball <- paste(
  "Informe sobre los salarios de los equipos de las Ligas
   Mayores de Béisbol durante la temporada 2009:

  a) Salario típico de un equipo:
     - Salario medio: $", round(salario_medio, 2), "
     - Rango de salarios: $", round(rango_salarios[1], 2), " a $", round(rango_salarios[2], 2), "

  b) Forma de la distribución:
     (ver histograma adjunto)

  c) Distribución de frecuencias acumulativas:
     - El 40% de los equipos paga menos que $", round(cuarenta_por_ciento, 2), "
     - Número de equipos con salarios totales inferiores a
      $80,000,000: ", equipos_menos_80m, "
"
)
cat(reporte_baseball)
