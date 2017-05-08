library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
        
        fluidPage(
                # Show the plot.
                plotlyOutput("pageview_plot"),
                checkboxInput("log_or_not", "Log-transform", TRUE)
        )

)
