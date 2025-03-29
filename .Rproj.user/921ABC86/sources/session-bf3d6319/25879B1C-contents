library(shiny)
library(tidyverse)
library(glue)
library(janitor)

# -------------------------------
# Préparation des données
# -------------------------------
data <- tibble()

for (stage in 1:2) {
  file <- str_glue("data/stage_stats_{stage}.csv")
  
  stage_stats_X <- read_csv(file, show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(
      id = stage,
      date = as.Date(date),
      temperature_score = as.numeric(temperature_score),
      temperature_deviation_c = as.numeric(temperature_deviation_c),
      temperature_trend_deviation = as.numeric(temperature_trend_deviation),
      average_resting_heart_rate = as.numeric(average_resting_heart_rate),
      recovery_index_score = as.numeric(recovery_index_score),
      resting_heart_rate_score = as.numeric(resting_heart_rate_score),
      average_hrv = as.numeric(average_hrv),
      lowest_resting_heart_rate = as.numeric(lowest_resting_heart_rate),
      sleep_balance_score = as.numeric(sleep_balance_score),
      previous_day_activity_score = as.numeric(previous_day_activity_score),
      hrv_balance_score = as.numeric(hrv_balance_score)
    )
  
  data <- bind_rows(data, stage_stats_X)
}

data_clean <- data %>% drop_na()

# -------------------------------
# UI
# -------------------------------
ui <- navbarPage("Analyse Snowboard",
                 
                 tabPanel("Visualisation générale",
                          sidebarLayout(
                            sidebarPanel(
                              tagList(
                                tags$label("Variable à afficher :", `title` = "Choisissez l’indicateur à visualiser dans le graphique ci-dessous."),
                                selectInput("var", NULL,
                                            choices = c(
                                              "Total Sleep Score" = "total_sleep_score",
                                              "Recovery Index Score" = "recovery_index_score",
                                              "Average Resting Heart Rate" = "average_resting_heart_rate",
                                              "High Activity Time" = "high_activity_time"
                                            )
                                )
                              ),
                              tagList(
                                tags$label("Ajouter une courbe de tendance",
                                           `title` = "Affiche une tendance lissée sur le graphique (méthode LOESS)"),
                                checkboxInput("smoother", NULL, TRUE)
                              )
                              
                            ),
                            mainPanel(
                              plotOutput("linePlot"),
                              tableOutput("summaryStats")
                            )
                          )
                 ),
                 
                 tabPanel("Analyse Sommeil",
                          fluidPage(
                            plotOutput("sleepBoxplot")
                          )
                 ),
                 
                 tabPanel("FC moyenne au repos",
                          fluidPage(
                            plotOutput("hrPlot")
                          )
                 ),
                 
                 tabPanel("Température & Récupération",
                          fluidPage(
                            plotOutput("tempRecoveryPlot")
                          )
                 ),
                 
                 tabPanel("À propos",
                          fluidPage(
                            h2("À propos de l'application"),
                            p("Cette application permet d'explorer des données physiologiques issues d'un suivi de stage de snowboard. Elle permet d'observer différents indicateurs de sommeil, récupération, activité physique et température corporelle."),
                            h3("Onglets disponibles"),
                            tags$ul(
                              tags$li(strong("Visualisation générale : "), "évolution dans le temps de l’indicateur choisi."),
                              tags$li(strong("Analyse Sommeil : "), "comparaison des scores de sommeil et de récupération."),
                              tags$li(strong("FC moyenne au repos : "), "évolution de la fréquence cardiaque au repos."),
                              tags$li(strong("Température & Récupération : "), "corrélation entre température corporelle et récupération.")
                            ),
                            br(),
                            p("⚠️ Les données proviennent de capteurs connectés et ont été nettoyées avant analyse.")
                          )
                 )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    p <- ggplot(data_clean, aes_string(x = "date", y = input$var, color = "as.factor(id)")) +
      geom_line() +
      labs(x = "Date", y = input$var, color = "Stage") +
      theme_minimal()
    
    if (input$smoother) {
      p <- p + geom_smooth(se = FALSE, method = "loess")
    }
    p
  })
  
  output$summaryStats <- renderTable({
    req(input$var)
    data_clean %>%
      summarise(
        Moyenne = round(mean(.data[[input$var]], na.rm = TRUE), 2),
        Médiane = round(median(.data[[input$var]], na.rm = TRUE), 2),
        Min = round(min(.data[[input$var]], na.rm = TRUE), 2),
        Max = round(max(.data[[input$var]], na.rm = TRUE), 2)
      )
  })
  
  output$sleepBoxplot <- renderPlot({
    data_long <- data_clean %>%
      select(date, id, total_sleep_score, recovery_index_score) %>%
      pivot_longer(cols = c(total_sleep_score, recovery_index_score),
                   names_to = "variable", values_to = "valeur")
    
    ggplot(data_long, aes(x = variable, y = valeur, fill = variable)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Comparaison des scores de sommeil et récupération",
           x = "", y = "Score")
  })
  
  output$hrPlot <- renderPlot({
    ggplot(data_clean, aes(x = date, y = average_resting_heart_rate, color = as.factor(id))) +
      geom_line() +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = "Fréquence cardiaque moyenne au repos",
           x = "Date", y = "FC moyenne (bpm)", color = "Stage") +
      theme_minimal()
  })
  
  output$tempRecoveryPlot <- renderPlot({
    ggplot(data_clean, aes(x = temperature_deviation_c, y = recovery_index_score)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      labs(title = "Déviation de température vs Indice de récupération",
           x = "Déviation température (°C)",
           y = "Recovery Index Score") +
      theme_minimal()
  })
}

# -------------------------------
# Lancement de l'app
# -------------------------------
shinyApp(ui = ui, server = server)

