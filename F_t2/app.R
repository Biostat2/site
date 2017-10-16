library(shiny)
library(tidyverse)


ui <- fluidPage(
   
   # Application title
   titlePanel("F = t2"),
   

   sidebarLayout(
      sidebarPanel(
        sliderInput("df",
                    "t, degrees of freedom:",
                    min = 1,
                    max = 50,
                    value = 18),
         sliderInput("df1",
                     "F, degrees of freedom sample1:",
                     min = 1,
                     max = 50,
                     value = 1),
         sliderInput("df2",
                     "F, degrees of freedom sample2:",
                     min = 1,
                     max = 50,
                     value = 18)
      ),
      

      mainPanel(
         plotOutput("distPlot")
      )
   )
)


server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     data_frame(q = seq(0, 3, length.out = 100),
                t = (pt(q, df = input$df) * 2) - 1,
                f = pf((q)^2, df1 = input$df1, df2 = input$df2)) %>%
       gather(distribution, pdf, -q) %>%
       ggplot(aes(q)) +
       geom_line(aes(y = pdf, colour = distribution), size = 2, alpha = 0.75)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

