# 86. Consulte los datos Real Estate, que contienen información sobre casas que se vendieron en el área
# de Goodyear, Arizona, el año pasado. Redacte un breve informe sobre la distribución de los precios de venta. Asegúrese de contestar, en dicho reporte, las siguientes preguntas:
#   a) ¿Alrededor de cuáles variables tienden a concentrarse los datos? ¿Cuál es el precio medio de
# venta? ¿Cuál es el precio mediano de venta? ¿Es una medida más representativa que otras
# de los precios típicos de venta?
#   b) ¿Cuál es el rango de los precios de venta? ¿Cuál es la desviación estándar? ¿Entre cuáles valores se ubica cerca de 95% de los precios de venta?

# Cargar los datos de Real Estate
real_estate <- read.csv("goodyearArizona.txt", header = TRUE, sep = "")

# Seleccionar la columna de precios de venta
precios <- real_estate$Precio

# a) Análisis de la tendencia central
precio_medio <- mean(precios)
precio_mediano <- median(precios)

# b) Rango y desviación estándar
rango_precios <- range(precios)
desviacion_estandar <- sd(precios)

# Intervalo del 95% (asumiendo distribución normal)
intervalo_95 <- c(precio_medio - 2 * desviacion_estandar, precio_medio + 2 * desviacion_estandar)

# Redactar el informe
informe <- paste(
  "Informe sobre la distribución de los precios de venta de casas en Goodyear, Arizona:

a) Análisis de la tendencia central:
   - El precio medio de venta es $", round(precio_medio, 2), ".
   - El precio mediano de venta es $", round(precio_mediano, 2), ".
   - La medida más representativa de los precios típicos de venta es el precio mediano, ya que no se ve afectada por valores atípicos tanto como el precio medio.

b) Rango y desviación estándar:
   - El rango de los precios de venta es de $", round(rango_precios[1], 2), " a $", round(rango_precios[2], 2), ".
   - La desviación estándar de los precios de venta es $", round(desviacion_estandar, 2), ".
   - Aproximadamente el 95% de los precios de venta se ubican entre $", round(intervalo_95[1], 2), " y $", round(intervalo_95[2], 2), ".
")

cat(informe)




#   87. Consulte los datos Baseball 2009, que incluyen información sobre los 30 equipos de la liga mayor
# durante la temporada 2009. Seleccione la variable que se refiere a los salarios de los equipos.
# a) Prepare un reporte sobre los salarios de los equipos y responda en él las siguientes preguntas:
#   1. ¿Alrededor de cuáles valores tienden a acumularse los datos? En específico, ¿cuál es el
# salario medio? ¿Cuál es el salario mediano? ¿Es una medida más representativa que otras
# de los salarios típicos de los equipos?
#   2. ¿Cuál es el rango de los salarios? ¿Cuál es la desviación estándar? ¿Entre cuáles valores se
# ubica cerca de 95% de los salarios?
#   b) Refiérase a la información sobre el salario promedio de cada año. En 1989, el salario promedio
# de un jugador fue de $512 930. En 2009, el salario promedio de un jugador se incrementó a
# $3 240 000. ¿Cuál fue el rango de incremento en el periodo?

# Cargar los datos de Baseball 2009
baseball_2009 <- read.csv("baseball.txt", header = TRUE, sep = "")

# Seleccionar la columna de salarios de los equipos
salarios <- baseball_2009$Salario.

# a) Análisis de la tendencia central
salario_medio <- mean(salarios)
salario_mediano <- median(salarios)

# b) Rango y desviación estándar
rango_salarios <- range(salarios)
desviacion_estandar <- sd(salarios)

# Intervalo del 95% (asumiendo distribución normal)
intervalo_95 <- c(salario_medio - 2 * desviacion_estandar, salario_medio + 2 * desviacion_estandar)

# b) Análisis del incremento del salario promedio de jugadores
salario_1989 <- 512930
salario_2009 <- 3240000
incremento_salario <- salario_2009 - salario_1989

# Redactar el informe
informe <- paste(
  "Informe sobre los salarios de los equipos de las Ligas Mayores de Béisbol durante la temporada 2009:

a) Análisis de la tendencia central:
   - El salario medio de los equipos es $", round(salario_medio, 2), ".
   - El salario mediano de los equipos es $", round(salario_mediano, 2), ".
   - La medida más representativa de los salarios típicos de los equipos es el salario mediano, ya que no se ve afectada por valores atípicos tanto como el salario medio.

b) Rango y desviación estándar:
   - El rango de los salarios es de $", round(rango_salarios[1], 2), " a $", round(rango_salarios[2], 2), ".
   - La desviación estándar de los salarios es $", round(desviacion_estandar, 2), ".
   - Aproximadamente el 95% de los salarios se ubican entre $", round(intervalo_95[1], 2), " y $", round(intervalo_95[2], 2), ".

b) Incremento del salario promedio de jugadores (1989 a 2009):
   - En 1989, el salario promedio de un jugador fue de $512,930.
   - En 2009, el salario promedio de un jugador fue de $3,240,000.
   - El incremento en el salario promedio durante este período fue de $", round(incremento_salario, 2), ".
")

cat(informe)




#   88. Consulte los datos sobre los autobuses del Distrito Escolar Buena. Prepare un reporte sobre el
# costo de mantenimiento del mes pasado. Responda las siguientes preguntas en dicho informe:
#   a) ¿Alrededor de cuáles valores tienden a acumularse los datos? En específico, ¿cuál fue el costo
# medio de mantenimiento el mes pasado? ¿Cuál es el costo mediano? ¿Es una medida más
# representativa que otras del costo típico?
#   b) ¿Cuál es el rango de los costos de mantenimiento? ¿Cuál es la desviación estándar? ¿Entre
# cuáles valores se ubica cerca de 95% de estos costos?


# Cargar los datos de los autobuses del Distrito Escolar Buena
school_buses <- read.csv("schoolBus.txt", header = TRUE, sep = "")

# Seleccionar la columna de costos de mantenimiento
costos_mantenimiento <- school_buses$mantenimiento.

# a) Análisis de la tendencia central
costo_medio <- mean(costos_mantenimiento)
costo_mediano <- median(costos_mantenimiento)

# b) Rango y desviación estándar
rango_costos <- range(costos_mantenimiento)
desviacion_estandar <- sd(costos_mantenimiento)

# Intervalo del 95% (asumiendo distribución normal)
intervalo_95 <- c(costo_medio - 2 * desviacion_estandar, costo_medio + 2 * desviacion_estandar)

# Redactar el informe
informe <- paste(
  "Informe sobre el costo de mantenimiento de los autobuses del Distrito Escolar Buena el mes pasado:

a) Análisis de la tendencia central:
   - El costo medio de mantenimiento el mes pasado fue $", round(costo_medio, 2), ".
   - El costo mediano de mantenimiento el mes pasado fue $", round(costo_mediano, 2), ".
   - La medida más representativa del costo típico de mantenimiento es el costo mediano, ya que no se ve afectada por valores atípicos tanto como el costo medio.

b) Rango y desviación estándar:
   - El rango de los costos de mantenimiento es de $", round(rango_costos[1], 2), " a $", round(rango_costos[2], 2), ".
   - La desviación estándar de los costos de mantenimiento es $", round(desviacion_estandar, 2), ".
   - Aproximadamente el 95% de los costos de mantenimiento se ubican entre $", round(intervalo_95[1], 2), " y $", round(intervalo_95[2], 2), ".
")

cat(informe)
