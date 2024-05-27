# 92. Consulte los datos Real Estate, que contienen información sobre casas que se vendieron en el área
# de Goodyear, Arizona, durante el año pasado.
# a) Distribuya los datos en una tabla que muestre el número de casas con alberca frente al número de casas sin alberca en cada uno de los cinco municipios. Si selecciona una casa al azar,
# calcule las siguientes probabilidades:
#   1. La casa se localiza en Township 1 o tiene alberca.
# 2. Dado que la casa se encuentra en Township 3, que tenga alberca.
# 3. Tiene alberca y se localiza en Township 3.
# b) Distribuya los datos en una tabla que muestre el número de casas con cochera frente a las que
# no la tienen en cada uno de los cinco municipios. Se elige una casa al azar y calcule las siguientes probabilidades.
# 1. La casa tiene cochera.
# 2. Si la casa se localiza en Township 5, que no tenga cochera.
# 3. La casa tiene cochera y se localiza en Township 3.
# 4. No tiene cochera o se localiza en Township 2.

# Cargar los datos de Real Estate
real_estate <- read.csv("goodyearArizona.txt", header = TRUE, sep = "")

# a) Tabla para casas con y sin alberca por municipio
pool_table <- table(real_estate$Colonia, real_estate$Alberca)
colnames(pool_table) <- c("Sin Alberca", "Con Alberca")
print(pool_table)

# Calcular probabilidades
total_houses <- nrow(real_estate)
houses_in_township1_or_pool <- sum(real_estate$Colonia == "Township 1") + sum(real_estate$Alberca == 1) - sum(real_estate$municipality == "Township 1" & real_estate$pool == 1)
prob_township1_or_pool <- houses_in_township1_or_pool / total_houses

houses_in_township3 <- sum(real_estate$Colonia == "Township 3")
houses_in_township3_and_pool <- sum(real_estate$Colonia == "Township 3" & real_estate$Alberca == 1)
prob_pool_given_township3 <- houses_in_township3_and_pool / houses_in_township3

prob_pool_and_township3 <- houses_in_township3_and_pool / total_houses

cat("P(Township 1 o alberca):", prob_township1_or_pool, "\n")
cat("P(Alberca | Township 3):", prob_pool_given_township3, "\n")
cat("P(Alberca y Township 3):", prob_pool_and_township3, "\n")

# b) Tabla para casas con y sin cochera por municipio
garage_table <- table(real_estate$Colonia, real_estate$Cochera)
colnames(garage_table) <- c("Sin Cochera", "Con Cochera")
print(garage_table)

# Calcular probabilidades
houses_with_garage <- sum(real_estate$Cochera == 1)
prob_garage <- houses_with_garage / total_houses

houses_in_township5 <- sum(real_estate$Colonia == "Township 5")
houses_in_township5_without_garage <- sum(real_estate$Colonia == "Township 5" & real_estate$Cochera == 0)
prob_no_garage_given_township5 <- houses_in_township5_without_garage / houses_in_township5

houses_with_garage_and_township3 <- sum(real_estate$Colonia == "Township 3" & real_estate$Cochera == 1)
prob_garage_and_township3 <- houses_with_garage_and_township3 / total_houses

houses_without_garage_or_township2 <- sum(real_estate$Cochera == 0) + sum(real_estate$Colonia == "Township 2") - sum(real_estate$garage == 0 & real_estate$municipality == "Township 2")
prob_no_garage_or_township2 <- houses_without_garage_or_township2 / total_houses

cat("P(Cochera):", prob_garage, "\n")
cat("P(Sin cochera | Township 5):", prob_no_garage_given_township5, "\n")
cat("P(Cochera y Township 3):", prob_garage_and_township3, "\n")
cat("P(Sin cochera o Township 2):", prob_no_garage_or_township2, "\n")





# 93. Consulte los datos Béisbol 2009, que contienen información sobre los 30 equipos de la Liga Mayor
# de Béisbol durante la temporada 2009. Establezca tres variables:
#   • Divida a los equipos en dos grupos, los que ganaron en la temporada y los que no lo hicieron.
# Es decir, cree una variable para contar los equipos que ganaron 81 juegos o más y los que
# ganaron 80 juegos o menos.
# 182 CAPÍTULO 5 Estudio de los conceptos de la probabilidad
# 
# • Cree una nueva variable para la asistencia, con tres categorías: una asistencia inferior a 2.0
# millones; una asistencia de 2.0 millones a 3.0 millones y una asistencia de 3.0 millones o más.
# • Cree una variable que muestre los equipos que jugaron en un estadio de menos de 15 años de
# antigüedad, contra uno que tiene 15 años o más.
# Responda las siguientes cuestiones:
#   a) Elabore una tabla que muestre el número de equipos que ganaron en la temporada frente a los
# que perdieron de acuerdo con las tres categorías de asistencia. Si selecciona un equipo al azar,
# calcule las siguientes probabilidades:
#   1. Tener una temporada de victorias.
# 2. Tener una temporada de victorias o contar con una asistencia de más de 3.0 millones.
# 3. Dada una asistencia de más de 3.0 millones, tener una temporada de victorias.
# 4. Tener una temporada de derrotas y contar con una asistencia de menos de 2.0 millones.
# b) Elabore una tabla que muestre el número de equipos que tuvieron una temporada de victorias
# contra los que jugaron en estadios antiguos o nuevos. Si selecciona un equipo al azar, calcule
# las siguientes probabilidades:
#   1. Seleccionar un equipo con una temporada de victorias.
# 2. La probabilidad de seleccionar un equipo con un récord ganador que haya jugado en un
# estadio nuevo.
# 3. El equipo tuvo un récord ganador o jugó en un estadio nuevo.

# Cargar los datos de Baseball 2009
baseball_2009 <- read.csv("baseball.txt", header = TRUE, sep = "")

# Crear variables
baseball_2009$season_result <- ifelse(baseball_2009$Victorias. >= 81, "Ganaron", "Perdieron")
baseball_2009$stadium_age <- 2024 - baseball_2009$Construcción.
baseball_2009$attendance_category <- cut(baseball_2009$Asistencia., breaks = c(-Inf, 2e6, 3e6, Inf), labels = c("Menos de 2 millones", "2 a 3 millones", "Más de 3 millones"))
baseball_2009$stadium_age_category <- ifelse(baseball_2009$stadium_age < 15, "Nuevo", "Antiguo")

# a) Tabla de equipos que ganaron vs. asistencia
win_attendance_table <- table(baseball_2009$season_result, baseball_2009$attendance_category)
print(win_attendance_table)

# Calcular probabilidades
total_teams <- nrow(baseball_2009)
teams_with_victory <- sum(baseball_2009$season_result == "Ganaron")
prob_victory <- teams_with_victory / total_teams

teams_with_victory_or_high_attendance <- sum(baseball_2009$season_result == "Ganaron" | baseball_2009$attendance_category == "Más de 3 millones")
prob_victory_or_high_attendance <- teams_with_victory_or_high_attendance / total_teams

teams_with_high_attendance <- sum(baseball_2009$attendance_category == "Más de 3 millones")
teams_with_victory_and_high_attendance <- sum(baseball_2009$season_result == "Ganaron" & baseball_2009$attendance_category == "Más de 3 millones")
prob_victory_given_high_attendance <- teams_with_victory_and_high_attendance / teams_with_high_attendance

teams_with_defeat_and_low_attendance <- sum(baseball_2009$season_result == "Perdieron" & baseball_2009$attendance_category == "Menos de 2 millones")
prob_defeat_and_low_attendance <- teams_with_defeat_and_low_attendance / total_teams

cat("P(Temporada de victorias):", prob_victory, "\n")
cat("P(Temporada de victorias o asistencia > 3 millones):", prob_victory_or_high_attendance, "\n")
cat("P(Temporada de victorias | Asistencia > 3 millones):", prob_victory_given_high_attendance, "\n")
cat("P(Temporada de derrotas y asistencia < 2 millones):", prob_defeat_and_low_attendance, "\n")

# b) Tabla de equipos que ganaron vs. estadio nuevo o antiguo
win_stadium_table <- table(baseball_2009$season_result, baseball_2009$stadium_age_category)
print(win_stadium_table)

# Calcular probabilidades
teams_in_new_stadium <- sum(baseball_2009$stadium_age_category == "Nuevo")
teams_with_victory_in_new_stadium <- sum(baseball_2009$season_result == "Ganaron" & baseball_2009$stadium_age_category == "Nuevo")
prob_victory_in_new_stadium <- teams_with_victory_in_new_stadium / teams_in_new_stadium

teams_with_victory_or_new_stadium <- sum(baseball_2009$season_result == "Ganaron" | baseball_2009$stadium_age_category == "Nuevo")
prob_victory_or_new_stadium <- teams_with_victory_or_new_stadium / total_teams

cat("P(Temporada de victorias):", prob_victory, "\n")
cat("P(Temporada de victorias en estadio nuevo):", prob_victory_in_new_stadium, "\n")
cat("P(Temporada de victorias o estadio nuevo):", prob_victory_or_new_stadium, "\n")



# 94. Consulte los datos de los camiones escolares que operan en el Distrito Escolar Buena. Establezca
# una variable que divida la edad de las autobuses en tres grupos: nuevos (menos de 5 años de
#                                                                          edad), medios (5 años pero menores a 10 años) y viejos (10 o más años). El costo mediano
# de mantenimiento es de $456. Basándose en este valor, cree una variable para aquellos que están
# por debajo de la mediana (bajo mantenimiento) y los que están por encima de la mediana (alto
#                                                                                         mantenimiento). Finalmente, desarrolle una tabla que muestre la relación entre el costo de mantenimiento y la edad del autobús.
# a) ¿Qué porcentaje de los autobuses es nuevo?
#   b) ¿Qué porcentaje de los nuevos autobuses tiene un bajo mantenimiento?
#   c) ¿Qué porcentaje de los viejos autobuses tiene alto mantenimiento?
#   d) ¿El costo de mantenimiento parece estar relacionado con la edad del autobús? Sugerencia:
#   Compare el costo de mantenimiento de los viejos autobuses con el costo de los nuevos.
# ¿Concluiría usted que el costo de mantenimiento es independiente de la edad?


# Cargar los datos de los autobuses del Distrito Escolar Buena
# Cargar los datos de los autobuses del Distrito Escolar Buena
school_buses <- read.csv("schoolBus.txt", header = TRUE, sep = "")

# Crear variables
school_buses$age_category <- cut(school_buses$uso., breaks = c(-Inf, 5, 10, Inf), labels = c("Nuevo", "Medio", "Viejo"))
school_buses$maintenance_level <- ifelse(school_buses$mantenimiento. < 456, "Bajo", "Alto")

# a) Porcentaje de autobuses nuevos
percent_new_buses <- sum(school_buses$age_category == "Nuevo") / nrow(school_buses) * 100

# b) Porcentaje de autobuses nuevos con bajo mantenimiento
new_buses <- subset(school_buses, age_category == "Nuevo")
percent_new_low_maintenance <- sum(new_buses$maintenance_level == "Bajo") / nrow(new_buses) * 100

# c) Porcentaje de autobuses viejos con alto mantenimiento
old_buses <- subset(school_buses, age_category == "Viejo")
percent_old_high_maintenance <- sum(old_buses$maintenance_level == "Alto") / nrow(old_buses) * 100

# d) Relación entre costo de mantenimiento y edad del autobús
maintenance_age_table <- table(school_buses$age_category, school_buses$maintenance_level)
print(maintenance_age_table)

cat("Porcentaje de autobuses nuevos:", percent_new_buses, "%\n")
cat("Porcentaje de autobuses nuevos con bajo mantenimiento:", percent_new_low_maintenance, "%\n")
cat("Porcentaje de autobuses viejos con alto mantenimiento:", percent_old_high_maintenance, "%\n")

