#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(fBasics)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(plotly)
library(Cairo);options(shiny.usecairo=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage("NARS QAQC App",
                           tabPanel("NCCA", 
                                    sidebarPanel(
                             selectInput("indicator", "Choose Indicator",
                                         c("Water Chemistry" = "wc")),
                             selectInput("parameter", "Choose Parameter",
                                         c("Nitrogen" = "nit",
                                           "Dissolved Ozygen" = "do")),
                             selectInput("state", "Choose State",
                                         c("Alabama" = "al",
                                           "Arizona" = "az")),
                             checkboxGroupInput("graphoptions", "Graph Options:",
                                                c("Include National" = "incnat",
                                                  "Include Ecoregions" = "increg"))
                             
                           ),
                           mainPanel("GRAPHS",
                                     column(12,
                                            h3("Q-Q Plot"),
                                            plotlyOutput("nccaqq"),
                                            h3("Histograms"),
                                            plotlyOutput("nccahist"),
                                            h3("Box Plots"),
                                            plotlyOutput("nccabox")))),
                           tabPanel("NWCA", sidebarPanel(
                             
                           )))
  
  

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

