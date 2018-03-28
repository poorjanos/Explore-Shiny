library(shiny)
library(here)
library(ggplot2)
library(dplyr)
library(scales)

t_telj_napi <- read.csv(here("Data", "Telj_napi.csv"), stringsAsFactors = FALSE)

ui <- fluidPage(titlePanel("Napi teljesítmények alakulása"),
                sidebarLayout(
                  sidebarPanel(
                    uiOutput("userOutput")
                  ),
                  mainPanel(plotOutput("performance_plot"),
                            br(),
                            br(),
                            tableOutput("results"))
                ))


server <- function(input, output) {
  
  # Render selectInput
  output$userOutput <- renderUI({
    selectInput("userInput", "Dolgozó",
                sort(unique(t_telj_napi$NEV)),
                selected = "Dancsecs Júlia")
  })
  
  # Create reactive data input once for reuse in render* funcs below
  filtered <- reactive({
    if (is.null(input$userInput)) {
      return(NULL)
    }   
  
    t_telj_napi %>%
      filter(
        NEV == input$userInput
      )
  })
  
  # Render plot
  output$performance_plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(DATUM, TELJ_MENNYISEGI_ALAP, group = 1)) +
      geom_line() +
      geom_point() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_y_continuous(labels = percent) 
  })
  
  # Render table
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)