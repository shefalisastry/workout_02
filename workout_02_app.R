
library(tidyverse)
library(reshape2)
library(shiny)
library(ggplot2)

# Future Value Function
future_value <- function(amount, rate, years) {
  FV <- amount*((1 + rate)^years)
  return(FV)
}


# Future Value of Annuity
annuity <- function(contrib, rate, years) {
  FVA <- contrib*(((1 + rate)^years - 1)/rate)
  return(FVA)
}


# Future Value of Growing Annuity
growing_annuity <- function(contrib, rate, growth, years) {
  FVGA <- contrib*((((1 + rate)^years) - ((1 + growth)^years))/(rate-growth))
  return(FVGA)
}


# Define UI for plots
ui <- fluidPage(
   
# Application title
titlePanel("Saving-Investing Modalities: An Interactive Visualization"),
  
# Sidebar with a slider input for values
fluidRow(
      column(4, 
             sliderInput("amount",
                         "Initial Amount",
                         min = 1,
                         max = 100000,
                         value = 1000,
                         pre = "$"),
             
             sliderInput("contrib",
                         "Annual Contribution",
                         min = 0,
                         max = 50000,
                         value = 2000,
                         pre = "$")),
      
      column(4, offset = 0.4,
             sliderInput("rate",
                         "Return Rate (in %)",
                         min = 0,
                         max = 20,
                         value = 5),
             sliderInput("growth",
                         "Growth Rate (in %)",
                         min = 0,
                         max = 20,
                         value = 2)),
      column(4, 
             sliderInput("year",
                         "Years",
                         min = 0,
                         max = 50,
                         value = 10),
             selectInput("facet",
                         "Facet?",
                         c("No", "Yes")))
      ),
mainPanel(
  h4("Timelines"),
plotOutput('distPlot', width = "900", height = "300"),
h4("Balances"),
verbatimTextOutput("modalities")
))
      

# Define server logic required to create plots
server <- function(input, output) {
  
  modalities <- reactive({
  year <- input$year
  amount <- input$amount
  rate <- input$rate/100
  contrib <- input$contrib
  growth <- input$growth/100
  mode_1 <- c()
  mode_2 <- c()
  mode_3 <- c()

  
    for(i in 0:year) {
      mode_1[i + 1] <- future_value(amount, rate, i)
      mode_2[i + 1] <- mode_1[i + 1] + annuity(contrib, rate, i)
      mode_3[i + 1] <- mode_1[i + 1] + growing_annuity(contrib, rate, growth, i)
      
    }
modalities <- data.frame("year" = 0:year, "no_contrib" = mode_1, "fixed_contrib" = mode_2, "growing_contrib" = mode_3)
modalities
})

  
  output$distPlot <- renderPlot({
    modalities_reshaped <- gather(modalities(), type, value, no_contrib:growing_contrib)
    melt_modalities <- melt(modalities(), id.vars = "year")
    
    if (input$facet == "No") {
      ggplot(melt_modalities, aes(x = year, y = value)) + 
        geom_line(aes(color = variable), size = 0.75) + geom_point(aes(color = variable), size = 1.5)
    }
    else {
      ggplot(melt_modalities, aes(x = year, y = value)) + 
        geom_line(aes(color = variable), size = 0.75) + geom_point(aes(color = variable), size = 1.5) + geom_area(aes(fill = variable), alpha = 0.5) + facet_grid(~variable)
    }
    })

output$modalities <- renderPrint({modalities()})


}
# Run the application 
shinyApp(ui = ui, server = server)
