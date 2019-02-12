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
                                           "Dissolved Oxygen" = "do",
                                           "Phosphorus" = "PTL")),
                             selectInput("stateui", "Choose State",
                                         c("Alabama" = "AL",
                                           "California" = "CA",
                                           "Connecticut" = "CT",
                                           "Deleware" = "DE",
                                           "Florida" = "FL",
                                           "Georgia" = "GA",
                                           "Illinois" = "IL",
                                           "Indiana" = "IN",
                                           "Louisiana" = "LA",
                                           "Massachusetts" = "MA",
                                           "Maryland" = "MD",
                                           "Maine" = "ME",
                                           "Michigan" = "MI",
                                           "Minnesota" = "MN",
                                           "Mississippi" = "MS",
                                           "North Carolina" = "NC",
                                           "New Hampshire" = "NH",
                                           "New Jersey" = "NJ",
                                           "New York" = "NY",
                                           "Ohio" = "OH",
                                           "Oregon" = "OR",
                                           "Pennsylvania" = "PA",
                                           "Rhode Island" = "RI",
                                           "South Carolina" = "SC",
                                           "Texas" = "TX",
                                           "Virginia" = "VA",
                                           "Washington" = "WA",
                                           "Wisconsin" = "WI")),
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
      filter(PARAMETER == input$parameterui,
             STATE == input$stateui)
    p <- ggplot(filtered, aes(sample = RESULT)) + stat_qq() 
    p
  })
  
  output$nccahist <- renderPlot({
    filtered <- 
      waterchem %>%
      filter(PARAMETER == input$parameterui,
            STATE == input$stateui)
    p <- ggplot(filtered, aes(x = RESULT)) + geom_histogram(color="black", fill="white")
    p
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

