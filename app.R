library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(

  titlePanel("Dice Rolling Simulator"),
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                  .js-irs-0 .irs-bar {background: #CF2A26}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, 
                  .js-irs-1 .irs-bar {background: #31B995}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, 
                  .js-irs-2 .irs-bar {background: #FDB813}")),
   
  sidebarLayout(
      sidebarPanel(
        helpText("Imagine a world in which you needed to illustrate the central limit 
                  theorem and a normal distribution. 
                  In this world you are given virtual dice. 
                  As many dice, with as many sides, 
                  and as many rolls as you want (within reason!). "),
        sliderInput("die","Number of die:", min = 1, max = 800, value = 2),
        sliderInput("side","Number of sides per die:", min = 1, max = 800, value = 6),
        sliderInput("rolls","Number of rolls:", min = 1, max = 800, value = 30)
        ),
    
  mainPanel(
      plotOutput("distPlot", width = "auto"),
      textOutput("mean"), textOutput("sd"), textOutput("max"), textOutput("min")
  )
 )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({   
    die <- input$die
    side <- input$side
    rolls <- input$rolls
    p <- c()
    for(i in 1:rolls){
      roll <- function(side, die){
        hand <- c() 
        j <- 0
        repeat{
          number <- round(runif(1, min = 1, max = side), 0)
          hand <- c(number, hand)
          j <- j+1
          if(j == die){break}
        }
        hand_total <- sum(hand)
        return(hand_total)
      }
      q <- roll(side, die)
      p <- c(p, q)
    }

    hist(p, col = "#7ECBB5", 
         main = "Frequency Distribution of Dice Rolls", 
         xlab = "Sum of Dice")
    output$mean <- renderText(
      paste0("Mean = ", round(mean(p, na.rm = T), 2)))
    output$sd <- renderText(
      paste0("Standard Deviation = ", round(sd(p, na.rm = T), 2)))
    output$min <- renderText(
      paste0("Min. Value = ", round(min(p, na.rm = T), 0)))
    output$max <- renderText(
      paste0("Max. Value = ", round(max(p, na.rm = T), 0)))
  })
}

shinyApp(ui = ui, server = server)

