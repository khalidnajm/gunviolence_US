library(dslabs)
library(shiny)

data(murders)

ui <- navbarPage(title = "Gun Violence in the US",
             tabPanel("Boxplot", 
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("region",
                                             "Select region",
                                             choices = levels(murders$region),
                                             selected = levels(murders$region))
                                    ),
                        mainPanel(plotOutput("boxplot"))
                        )
                      ),
             
             tabPanel("Murder Rate",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("ascDsc", 
                                      "Choose order", 
                                      choices = c("Ascending","Descending"))
                        ),
                      mainPanel(plotOutput("murderRate"))
                      )
                )
)
    


server <- function(input, output) {

            output$boxplot = renderPlot({
                boxplot(murders$total[murders$region%in%input$region]~murders$region[murders$region%in%input$region],
                        ylab = "Murder Rate per 100,000",
                        xlab = "")
            })
            
            output$murderRate = renderPlot({
              ascDsc <- switch(input$ascDsc,
                               Ascending = order(murders$total),
                               Descending = order(murders$total, decreasing=TRUE),
                               order(murders$total))
              barplot(murders$total[ascDsc], names.arg = murders$abb[ascDsc])
            })
}


shinyApp(ui = ui, server = server)
