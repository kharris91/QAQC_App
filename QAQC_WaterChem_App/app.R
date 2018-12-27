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

hydrolab <- read.csv("assessed_ncca2010_hydrolab.csv")
siteinfo <- read.csv("assessed_ncca2010_siteinfo.revised.06212016.csv")
waterchem <- read.csv("assessed_ncca2010_waterchem.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage("NARS QAQC App",
                           tabPanel("NCCA", 
                                    sidebarPanel(
                             selectInput("indicatorui", "Choose Indicator",
                                         c("Water Chemistry" = "wc")),
                             selectInput("parameterui", "Choose Parameter",
                                         c("Nitrogen" = "NTL",
                                           "Dissolved Ozygen" = "do",
                                           "Phosphorus" = "PTL")),
                             selectInput("stateui", "Choose State",
                                         c("Alabama" = "al",
                                           "Arizona" = "az",
                                           "Virginia" = "va")),
                             checkboxGroupInput("graphoptions", "Graph Options:",
                                                c("Include National" = "incnat",
                                                  "Include Ecoregions" = "increg"))
                             
                           ),
                           mainPanel("GRAPHS",
                                     column(12,
                                            h3("Q-Q Plot"),
                                            plotOutput("nccaqq"),
                                            h3("Histograms"),
                                            plotOutput("nccahist"),
                                            h3("Box Plots"),
                                            plotOutput("nccabox")))),
                           tabPanel("NWCA", sidebarPanel(
                             
                           )))
  
  

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nccaqq <- renderPlot({
    filtered <-
      waterchem %>%
      filter(PARAMETER == input$parameterui)
    p <- ggplot(filtered, aes(sample = RESULT)) + stat_qq() 
    p
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

