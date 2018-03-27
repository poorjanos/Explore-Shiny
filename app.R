library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(titlePanel("BC Liquor Store prices"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      "priceInput",
                      "Price",
                      min = 0,
                      max = 100,
                      value = c(10, 90),
                       pre = "$"
                    )
                    ,
                    radioButtons(
                      "typeInput",
                      "Product type",
                      choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                      selected = "WINE"
                    ),
                    selectInput(
                      "countryInput",
                      "Country",
                      choices = c("CANADA", "FRANCE", "ITALY")
                    )
                  ),
                  mainPanel(h2(textOutput("elemnum")),
                            plotOutput("coolplot"),
                            br(),
                            br(),
                            tableOutput("results"))
                ))


server <- function(input, output) {
  
  output$elemnum <- renderText({
    filtered <-
      bcl %>%
      filter(
        Price >= input$priceInput[1],
        Price <= input$priceInput[2],
        Type == input$typeInput,
        Country == input$countryInput
      )
    paste("Elemszám", dim(filtered)[1])
  })
  
  output$coolplot <- renderPlot({
    filtered <-
      bcl %>%
      filter(
        Price >= input$priceInput[1],
        Price <= input$priceInput[2],
        Type == input$typeInput,
        Country == input$countryInput
      )
    ggplot(filtered, aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered <-
      bcl %>%
      filter(
        Price >= input$priceInput[1],
        Price <= input$priceInput[2],
        Type == input$typeInput,
        Country == input$countryInput
      )
    filtered
  })
}

shinyApp(ui = ui, server = server)