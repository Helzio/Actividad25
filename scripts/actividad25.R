install.packages("tidyverse")
library(tidyverse)

speed_dating_data <- read_csv("data/Speed Dating Data.csv")

head(speed_dating_data)
summary(speed_dating_data)

library(ggplot2)


# ---------------------------------------------
# ----------- Sección de analisis -------------
# ---------------------------------------------

# Análisis de Matches:
total_dates <- nrow(speed_dating_data)
matches <- sum(speed_dating_data$match == 1)
match_percentage <- (matches / total_dates) * 100

# Evaluación de Atributos por Género:
library(dplyr)
attributes <- c("attr", "sinc", "intel", "fun", "amb", "shar")
attributes_desirability <- speed_dating_data %>%
  group_by(gender) %>%
  summarise(across(all_of(attributes), mean, na.rm = TRUE))

# Importancia de la Atracción Física:
attraction_importance <- colMeans(speed_dating_data[, c("attr", "attr_o")], na.rm = TRUE)

# Influencia de Intereses Compartidos:
shared_interests <- speed_dating_data %>%
  group_by(dec, dec_o) %>%
  summarise(mean_shar = mean(shar, na.rm = TRUE))

# Auto-percepción vs. Percepción de los Demás:
self_perception <- colMeans(speed_dating_data[, c("attr3_1", "attr")], na.rm = TRUE)

# Efecto del Orden de la Cita:
date_order_effect <- aggregate(dec ~ order, speed_dating_data, mean)


# ----------------------------------------------------
# ----------- Sección de visualizaciones -------------
# ----------------------------------------------------


# Distribución de Edades por Género
ggplot(speed_dating_data, aes(x = age, fill = as.factor(gender))) +
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.6) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Mujer", "Hombre")) +
  labs(title = "Distribución de Edades por Género", x = "Edad", y = "Frecuencia", fill = "Género") +
  theme_minimal()

# Calificaciones Promedio por Atributo y Género
speed_dating_data %>%
  group_by(gender) %>%
  summarise(across(c(attr, sinc, intel, fun, amb, shar), mean, na.rm = TRUE)) %>%
  gather(key = "attribute", value = "score", -gender) %>%
  ggplot(aes(x = attribute, y = score, fill = as.factor(gender))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Mujer", "Hombre")) +
  labs(title = "Calificaciones Promedio por Atributo y Género", x = "Atributo", y = "Calificación Promedio", fill = "Género") +
  theme_minimal()

# Visualización del porcentaje de matches
library(ggplot2)
ggplot(data.frame(Match=c("Sí", "No"), Count=c(matches, total_dates - matches)), aes(x=Match, y=Count)) +
  geom_bar(stat="identity", fill=c("green", "red")) +
  labs(title="Porcentaje de Matches", x="Match", y="Cantidad") +
  theme_minimal()

# Visualización de atributos menos deseados por género
library(reshape2)
attributes_desirability_long <- melt(attributes_desirability, id.vars = "gender")
ggplot(attributes_desirability_long, aes(x=variable, y=value, fill=as.factor(gender))) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("blue", "orange"), labels=c("Mujer", "Hombre")) +
  labs(title="Calificación Promedio de Atributos por Género", x="Atributo", y="Calificación Promedio") +
  theme_minimal()

# Visualización de la importancia de la atracción física
attraction_data <- data.frame(
  Category = c("Autoevaluación de Atracción", "Evaluación de Atracción por la Cita"),
  Score = c(attraction_importance['attr'], attraction_importance['attr_o'])
)

ggplot(attraction_data, aes(x = Category, y = Score, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("blue", "green")) +
  labs(title = "Comparación de la Atracción Física: Autoevaluación vs. Evaluación por la Cita",
       x = "Categoría",
       y = "Calificación Promedio") +
  theme_minimal()


# Visualización de la influencia de intereses compartidos
ggplot(shared_interests, aes(x=interaction(dec, dec_o), y=mean_shar)) +
  geom_bar(stat="identity", fill="#008080") + # Código hexadecimal para el color teal
  labs(title="Influencia de Intereses Compartidos en la Decisión de Segunda Cita", x="Decisión Combinada", y="Intereses Compartidos Promedio") +
  theme_minimal()

# Visualización de auto-percepción vs. percepción de los demás
perception_data <- data.frame(Type=c("Auto-percepción", "Percepción de los Demás"), 
                              Score=c(self_perception['attr3_1'], self_perception['attr']))
ggplot(perception_data, aes(x=Type, y=Score, fill=Type)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#FFD700", "#C0C0C0")) + # Códigos hexadecimales para gold y silver
  labs(title="Auto-percepción vs. Percepción de los Demás en Atracción Física", 
       x="Tipo", y="Calificación Promedio") +
  theme_minimal()


# Visualización del efecto del orden de la cita
ggplot(date_order_effect, aes(x=order, y=dec)) +
  geom_line(color="darkred") +
  labs(title="Efecto del Orden de la Cita en la Decisión de Segunda Cita", x="Orden de la Cita", y="Probabilidad de Decir 'Sí'") +
  theme_minimal()
