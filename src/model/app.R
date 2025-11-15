#library(shiny)
#library(readxl)
#library(dplyr)
#library(AER)
#library(stargazer)
#library(gt)
#library(ggrepel)
#library(ggplot2)
source("global.R")
ui <- fluidPage(
  
  titlePanel("Réplication de l'étude d'Acemoglu, Johnson et Robinson"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Table de regression"),
      
      selectInput("table_choice", 
                  label = "Choisir la table à afficher:",
                  choices = c(
                    "Table 4 - 2SLS",
                    "Table 4 - First Stage",
                    "Table 4 - Ordinary LS",
                    "Table 4 - 2SLS 2019",
                    "Table 4 - Ordinary LS 2019"

                  ),
                  selected = "OLS 1995 - Colonne 1")
    ),
    
    mainPanel(
      

      tabsetPanel(
        
        tabPanel("Tables de Régression", 
                 h2("Résultats de la régression"),
                 verbatimTextOutput(outputId = "model_table_output")
        ),

        tabPanel("Figure 2",
                 h2("OLS"),
                 plotOutput(outputId = "plot_ols_output")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$model_table_output <- renderPrint({
    if (input$table_choice == "Table 4 - 2SLS") {
      
      stargazer(iv_a1, iv_a2, iv_a3, iv_a4, iv_a5, iv_a6, iv_a7, iv_a8,
                column.labels = c('Base sample', 'Base sample', 'Base sample without neo europes', 'Base sample without neo europes', 'base sample without africa',
                                  'base sample without africa', 'base sample without africa', 'base sample other regions dummy', 'base sample other regions dummy'),
                type = "text",
                title = "2SLS - IV REGRESSIONS OF LOG GDP PER CAPITA",
                align = TRUE,
                df = FALSE,
                omit.stat = c("ser", "f"),
                omit = c("Constant"))
    } 
    

    else if (input$table_choice == "Table 4 - First Stage") {
      
      stargazer(fs_b1, fs_b2, fs_b3, fs_b4, fs_b5, fs_b6, fs_b7, fs_b8,
                type = "text", 
                title = "First Stage for Average Protection Against Expropriation Risk in 1985-1995",
                align = TRUE,
                df = FALSE,
                omit.stat = c("ser", "f"),
                omit = c("Constant"))
    }
    
    else if (input$table_choice == "Table 4 - Ordinary LS") {
      
      stargazer(ols_c1, ols_c2, ols_c3, ols_c4, ols_c5, ols_c6, ols_c7,
                type = "text", 
                title = "Ordinary Least Square",
                align = TRUE,
                df = FALSE,
                omit.stat = c("ser", "f"),
                omit = c("Constant"))
    }
    else if (input$table_choice == "Table 4 - 2SLS 2019") {
      
      stargazer(iv_2019_1,iv_2019_2, iv_2019_3, iv_2019_4, iv_2019_5, iv_2019_6,
                type = "text", 
                title = "2SLS - IV REGRESSIONS OF LOG GDP PER CAPITA 2019",
                align = TRUE,
                df = FALSE,
                omit.stat = c("ser", "f"),
                omit = c("Constant"))      
    }

    else if (input$table_choice == "Table 4 - Ordinary LS 2019") {
      
      stargazer(ols_2019_1, ols_2019_2, ols_2019_3, ols_2019_4, ols_2019_5,
                type = "text", 
                title = "Ordinary Least Square 2019",
                align = TRUE,
                df = FALSE,
                omit.stat = c("ser", "f"),
                omit = c("Constant"))
    }
    
  })
output$plot_ols_output <- renderPlot({
  print(graphique_ols)
  
})
}

shinyApp(ui = ui, server = server)