library(shiny)
library(ggplot2)
library(dplyr)

# Carga el dataset de citas rápidas
speed_dating_data <- read.csv("Speed Dating Data.csv")

# Calcula cualquier estadística o resultados de tu análisis previo aquí
# Por ejemplo, la proporción de matches, promedios, etc.

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

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Análisis del Dataset de Citas Rápidas"),
  sidebarLayout(
    sidebarPanel(
      # Añade controles para filtrar por género, rango de edad, etc.
      selectInput("gender", "Género:", choices = c("Todos", "Mujer", "Hombre")),
      sliderInput("ageRange", "Rango de Edad:", min = min(speed_dating_data$age, na.rm = TRUE), max = max(speed_dating_data$age, na.rm = TRUE), value = c(min(speed_dating_data$age, na.rm = TRUE), max(speed_dating_data$age, na.rm = TRUE)))
      # Más controles según lo que quieras permitir ajustar en tus visualizaciones
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Distribución de Edades", plotOutput("ageDistributionPlot")),
        tabPanel("Porcentaje de Matches", plotOutput("matchPercentagePlot")),
        tabPanel("Atributos por Género", plotOutput("attributeByGenderPlot")),
        tabPanel("Intereses Compartidos", plotOutput("sharedInterestsPlot")),
        tabPanel("Auto-percepción vs. Percepción de los Demás", plotOutput("selfPerceptionPlot")),
        tabPanel("Influencia de Intereses Compartidos", plotOutput("influenceSharedInterestsPlot")),
        tabPanel("Efecto del Orden de la Cita", plotOutput("orderEffectPlot"))
      )
    )
  )
)

# Define la lógica del servidor
server <- function(input, output) {
  # Filtra los datos según las entradas de los controles de la sidebar
  filtered_data <- reactive({
    data <- speed_dating_data
    if (input$gender != "Todos") {
      data <- data[data$gender == ifelse(input$gender == "Mujer", 0, 1), ]
    }
    data <- data[data$age >= input$ageRange[1] & data$age <= input$ageRange[2], ]
    data
  })
  
  # Visualización para la distribución de edades por género
  output$ageDistributionPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = age, fill = as.factor(gender))) +
      geom_histogram(binwidth = 1, position = 'identity', alpha = 0.6) +
      scale_fill_manual(values = c("blue", "red"), labels = c("Mujer", "Hombre")) +
      labs(title = "Distribución de Edades por Género", x = "Edad", y = "Frecuencia", fill = "Género") +
      theme_minimal()
  })
  
  # Visualización del porcentaje de matches
  # Calcula el porcentaje de matches para los datos filtrados
  output$matchPercentagePlot <- renderPlot({
    data <- filtered_data()
    matches <- sum(data$match == 1, na.rm = TRUE)
    total_dates <- nrow(data)
    match_percentage <- matches / total_dates * 100
    
    data <- data.frame(Match = c("Sí", "No"), Count = c(match_percentage, 100 - match_percentage))
    ggplot(data, aes(x = Match, y = Count, fill = Match)) +
      geom_bar(stat = "identity") +
      labs(title = "Porcentaje de Matches", x = "Match", y = "Porcentaje") +
      theme_minimal()
  })
  
  # Visualización de atributos por género
  output$attributeByGenderPlot <- renderPlot({
    data <- filtered_data()
    attributes <- data %>% 
      group_by(gender) %>% 
      summarise(across(c(attr, sinc, intel, fun, amb, shar), mean, na.rm = TRUE)) %>%
      gather(key = "attribute", value = "score", -gender)
    
    ggplot(attributes, aes(x = attribute, y = score, fill = as.factor(gender))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("blue", "red"), labels = c("Mujer", "Hombre")) +
      labs(title = "Calificaciones Promedio por Atributo y Género", x = "Atributo", y = "Calificación Promedio", fill = "Género") +
      theme_minimal()
  })
  
  # Visualización de intereses compartidos y su influencia en la decisión de segunda cita
  output$sharedInterestsPlot <- renderPlot({
    data <- filtered_data()
    shared_interests <- data %>%
      group_by(dec, dec_o) %>%
      summarise(mean_shar = mean(shar, na.rm = TRUE))
    
    ggplot(shared_interests, aes(x = interaction(dec, dec_o), y = mean_shar)) +
      geom_bar(stat = "identity", fill = "#008080") +
      labs(title = "Influencia de Intereses Compartidos en la Decisión de Segunda Cita", x = "Decisión Combinada", y = "Intereses Compartidos Promedio") +
      theme_minimal()
  })
  
  # Visualización de auto-percepción vs. percepción de los demás
  output$selfPerceptionPlot <- renderPlot({
    data <- filtered_data()
    self_perception <- colMeans(data[, c("attr3_1", "attr")], na.rm = TRUE)
    perception_data <- data.frame(Type = c("Auto-percepción", "Percepción de los Demás"), 
                                  Score = c(self_perception['attr3_1'], self_perception['attr']))
    
    ggplot(perception_data, aes(x = Type, y = Score, fill = Type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#FFD700", "#C0C0C0")) +
      labs(title = "Auto-percepción vs. Percepción de los Demás en Atracción Física", x = "Tipo", y = "Calificación Promedio") +
      theme_minimal()
  })
  
  # Visualización para la influencia de intereses compartidos en la decisión de segunda cita
  output$influenceSharedInterestsPlot <- renderPlot({
    data <- filtered_data()
    shared_interests <- data %>%
      group_by(dec, dec_o) %>%
      summarise(mean_shar = mean(shar, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Decision = case_when(
        dec == 1 & dec_o == 1 ~ "Ambos dicen 'Sí'",
        dec == 0 & dec_o == 1 ~ "Solo la cita dice 'Sí'",
        dec == 1 & dec_o == 0 ~ "Solo el participante dice 'Sí'",
        TRUE ~ "Ambos dicen 'No'"
      ))
    
    ggplot(shared_interests, aes(x = Decision, y = mean_shar, fill = Decision)) +
      geom_col() +
      labs(title = "Influencia de Intereses Compartidos en la Decisión de Segunda Cita",
           x = "Decisión Combinada",
           y = "Promedio de Intereses Compartidos") +
      theme_minimal()
  })
  
  # Visualización del efecto del orden de la cita
  output$orderEffectPlot <- renderPlot({
    data <- filtered_data()
    order_effect <- aggregate(dec ~ order, data, mean)
    
    ggplot(order_effect, aes(x = order, y = dec)) +
      geom_line(color = "darkred") +
      labs(title = "Efecto del Orden de la Cita en la Decisión de Segunda Cita", x = "Orden de la Cita", y = "Probabilidad de Decir 'Sí'") +
      theme_minimal()
  })
  
  
  # Agrega más outputs según sea necesario
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
