# library(rsconnect)
# rsconnect::deployApp('cloud/project/coin_toss')

# preparations; required libraries
library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)

# user interface elements and layout
ui <- fluidPage(
  titlePanel("Cara o Cruz"),
  sidebarLayout(
    sidebarPanel(
      
      numericInput(inputId = "n", label = "Número de lanzadas:",
                  min = 1, max = 10000, value = 1),
      sliderInput(inputId = "prob", label = "Probabilidad (0: más probable cruz; 1: más probable cara):",
                  min = 0, max = 1, value = 0.5),
      plotOutput(outputId = "bars")
      
    ),
    mainPanel(
      plotOutput(outputId = "freq")
      )
    
  )
)

# server-side computations
server <- function(input, output) {
  
  # the bar plot
  output$bars <- renderPlot({
    
    # most of this is for ggplot2; note the input$x syntax
    flips <- tibble(flips = rbinom(input$n, 1, input$prob)) %>% 
      mutate(flips = if_else(flips == 1, "Cara", "Cruz"))  
    
    flips %>% 
      count(flips) %>% 
      ggplot(aes(flips, n, fill = flips)) +
      geom_col() +
      geom_label(aes(flips, n, label = n), size = 5) +
      theme(legend.position = "none",
            axis.text = element_text(size = 15)) +
      labs(x = "", y = "") +
      ggtitle(str_c("Resultado de ", input$n,
                    " lanzadas con P ",
                    sprintf("%.2f", input$prob)))
  })

  # the bar plot
  output$freq <- renderPlot({
    X <- as.numeric( runif(input$n) > input$prob )
    X <- cumsum(X) / (1:input$n)
    plot( 1:input$n, X, type="l", ylim=c(0,1), 
          xlab = "Número de lanzadas", ylab = "Proporción de Cruces", lwd=3
    )
    abline(h=.5,lty=2,col=emphGrey,lwd=2)
  })
}

# run it all
shinyApp(ui = ui, server = server)

