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

# ncca10_hydrolab <- read.csv("assessed_ncca2010_hydrolab.csv")
ncca10_siteinfo <- read.csv("assessed_ncca2010_siteinfo.revised.06212016.csv", stringsAsFactors = F)
#colnames(ncca10_siteinfo)[1] <- "UID" # inspect the siteinfo file; my code does not require this step
ncca10_waterchem <- read.csv("assessed_ncca2010_waterchem.csv", stringsAsFactors = F)
ncca10_waterchem <- left_join(ncca10_waterchem, ncca10_siteinfo, by = c("UID", "SITE_ID", "STATE"))
ncca10_wc_est <- ncca10_waterchem[ncca10_waterchem$RSRC_CLASS=="NCA_Estuarine_Coastal",]
ncca10_wc_gl <- ncca10_waterchem[ncca10_waterchem$RSRC_CLASS!="NCA_Estuarine_Coastal",]
#ncca10_wc_gl <- ncca10_wc_gl[,-20] # the two date columns are not problematic in my code



# Define UI for application that draws graphics
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage("NARS QAQC App",
                           
                           tabPanel("NCCA 2010 - Estuaries", 
                                    sidebarPanel(
                                      selectInput("indicatorui_est", "Choose Indicator",
                                                  c("Water Chemistry" = "wc")),
                                      selectInput("parameterui_est", "Choose Parameter",
                                                  c("Dissolved Inorganic Nitrogen" = "DIN",
                                                    "Phosphate" = "SRP",
                                                    "Chloropyll-A" = "CHLA")),
                                      selectInput("stateui_est", "Choose State",
                                                  c("Alabama" = "AL",
                                                    "California" = "CA",
                                                    "Connecticut" = "CT",
                                                    "Delaware" = "DE",
                                                    "Florida" = "FL",
                                                    "Georgia" = "GA",
                                                    "Louisiana" = "LA",
                                                    "Massachusetts" = "MA",
                                                    "Maryland" = "MD",
                                                    "Maine" = "ME",
                                                    "Mississippi" = "MS",
                                                    "North Carolina" = "NC",
                                                    "New Hampshire" = "NH",
                                                    "New Jersey" = "NJ",
                                                    "New York" = "NY",
                                                    "Oregon" = "OR",
                                                    "Rhode Island" = "RI",
                                                    "South Carolina" = "SC",
                                                    "Texas" = "TX",
                                                    "Virginia" = "VA",
                                                    "Washington" = "WA"))
                                    ),
                                    mainPanel("Explore the data",
                                              column(12,
                                                     h3("Q-Q Plots"),
                                                     p("The following quantile-quantile plots display",
                                                       span("state data in red", style = "color:red"),
                                                       "and",
                                                       span("ecoregion data in blue", style = "color:blue"),
                                                       "."),
                                                     plotlyOutput("ncca10_wc_est_qq"),
                                                     h3("Histograms"),
                                                     p("The following histograms display",
                                                       span("state data in red", style = "color:red"),
                                                       "and",
                                                       span("ecoregion data in blue", style = "color:blue"),
                                                       "."),
                                                     plotlyOutput("ncca10_wc_est_hist"),
                                                     h3("Box Plot"),
                                                     p("The following box plot compares your state data with data from other states in your ecoregion."),
                                                     plotlyOutput("ncca10_wc_est_box")))),
                           
                           tabPanel("NCCA 2015 - Great Lakes",
                                    sidebarPanel(
                                      selectInput("indicatorui_gl", "Choose Indicator",
                                                  c("Water Chemistry" = "wc")),
                                      selectInput("parameterui_gl", "Choose Parameter",
                                                  c("Total Nitrogen" = "NTL",
                                                    "Total Phosphorus" = "PTL",
                                                    "Chloropyll-A" = "CHLA")),
                                      selectInput("stateui_gl", "Choose State",
                                                  c("Illinois" = "IL",
                                                    "Indiana" = "IN",
                                                    "Michigan" = "MI",
                                                    "Minnesota" = "MN",
                                                    "New York" = "NY",
                                                    "Ohio" = "OH",
                                                    "Pennsylvania" = "PA",
                                                    "Wisconsin" = "WI"))
                                      ),
                                    mainPanel(
                                      column(12,
                                             h3("Q-Q Plots"),
                                             p("The following quantile-quantile plots display",
                                               span("state data in red", style = "color:red"),
                                               "and",
                                               span("ecoregion data in blue", style = "color:blue"),
                                               "."),
                                             plotlyOutput("ncca10_wc_gl_qq"),
                                             h3("Histograms"),
                                             p("The following histograms display",
                                               span("state data in red", style = "color:red"),
                                               "and",
                                               span("ecoregion data in blue", style = "color:blue"),
                                               "."),
                                             plotlyOutput("ncca10_wc_gl_hist"),
                                             h3("Box Plot"),
                                             p("The following box plot compares your state data with data from other states in your ecoregion."),
                                             plotlyOutput("ncca10_wc_gl_box")))),
                                    
                           tabPanel("NWCA 2011(under construction)"))
                
              )     
                
                


# Define server logic required to draw graphics
server <- function(input, output) {
  
  output$ncca10_wc_est_qq <- renderPlotly({
    filtered <-
      ncca10_wc_est %>%
      filter(PARAMETER == input$parameterui_est,
             STATE == input$stateui_est)
    ecoreg <- unique(filtered$NCCR_REG)
    units <- unique(filtered$UNITS)
    p <- ggplot(ncca10_wc_est, aes(sample = RESULT)) + 
      stat_qq(data = subset(ncca10_wc_est, PARAMETER == input$parameterui_est & STATE == input$stateui_est), 
              color = "red", alpha = 0.2) +
      stat_qq(data = subset(ncca10_wc_est, PARAMETER == input$parameterui_est & NCCR_REG == ecoreg),
              color = "blue", alpha = 0.2) + 
      theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle(paste(ncca10_wc_est[ncca10_wc_est$PARAMETER==input$parameterui_est,7])) +
      ylab(paste('Concentration in',units,sep = ' ')) 
    p <- ggplotly(p)
    p
    
  })
  
  output$ncca10_wc_est_hist <- renderPlotly({
    filtered <- 
      ncca10_wc_est %>%
      filter(PARAMETER == input$parameterui_est,
             STATE == input$stateui_est)
    ecoreg <- unique(filtered$NCCR_REG)
    units <- unique(filtered$UNITS)
    p <- ggplot(ncca10_wc_est, aes(x = RESULT)) + 
      geom_histogram(data = subset(ncca10_wc_est, PARAMETER == input$parameterui_est & STATE == input$stateui_est),
                     fill="red", alpha = 0.2) +
      geom_histogram(data = subset(ncca10_wc_est, PARAMETER == input$parameterui_est & NCCR_REG == ecoreg),
                     fill = "blue", alpha = 0.2) +
      theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle(paste(ncca10_wc_est[ncca10_wc_est$PARAMETER==input$parameterui_est,7])) +
      xlab(paste('Concentration in',units,sep = ' ')) + ylab('Number of sites')
    p <- ggplotly(p) %>% layout(dragmode = "pan")
    p
  })
  
  output$ncca10_wc_est_box <- renderPlotly({
    filtered <-
      ncca10_wc_est %>%
      filter(PARAMETER == input$parameterui_est,
             STATE == input$stateui_est)
    ecoreg <- unique(filtered$NCCR_REG)
    units <- unique(filtered$UNITS)
    allStates <- 
      ncca10_wc_est %>%
      filter(PARAMETER == input$parameterui_est)
    p <- ggplot(allStates[allStates$NCCR_REG==ecoreg,], aes(x = STATE, y = RESULT)) + geom_boxplot() +
      theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle(paste(ncca10_wc_est[ncca10_wc_est$PARAMETER==input$parameterui_est,7])) +
      xlab('State') + ylab(paste('Concentration in',units,sep = ' '))
    p <- ggplotly(p)  %>% 
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)
  })
  
  output$ncca10_wc_gl_qq <- renderPlotly({
    filtered <-
      ncca10_wc_gl %>%
      filter(PARAMETER == input$parameterui_gl,
             STATE == input$stateui_gl)
    ecoreg <- unique(filtered$NCCR_REG)
    units <- unique(filtered$UNITS)
    p <- ggplot(ncca10_wc_gl, aes(sample = RESULT)) + 
      stat_qq(data = subset(ncca10_wc_gl, PARAMETER == input$parameterui_gl & STATE == input$stateui_gl), 
              color = "red", alpha = 0.2) +
      stat_qq(data = subset(ncca10_wc_gl, PARAMETER == input$parameterui_gl & NCCR_REG == ecoreg),
              color = "blue", alpha = 0.2) + 
      theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle(paste(ncca10_wc_gl[ncca10_wc_gl$PARAMETER==input$parameterui_gl,7])) +
      ylab(paste('Concentration in',units,sep = ' ')) 
    p <- ggplotly(p)
    p
    
  })
  
  output$ncca10_wc_gl_hist <- renderPlotly({
    filtered <- 
      ncca10_wc_gl %>%
      filter(PARAMETER == input$parameterui_gl,
             STATE == input$stateui_gl)
    ecoreg <- unique(filtered$NCCR_REG)
    units <- unique(filtered$UNITS)
    p <- ggplot(ncca10_wc_gl, aes(x = RESULT)) + 
      geom_histogram(data = subset(ncca10_wc_gl, PARAMETER == input$parameterui_gl & STATE == input$stateui_gl),
                     fill="red", alpha = 0.2) +
      geom_histogram(data = subset(ncca10_wc_gl, PARAMETER == input$parameterui_gl & NCCR_REG == ecoreg),
                     fill = "blue", alpha = 0.2) +
      theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle(paste(ncca10_wc_gl[ncca10_wc_gl$PARAMETER==input$parameterui_gl,7])) +
      xlab(paste('Concentration in',units,sep = ' ')) + ylab('Number of sites')
    p <- ggplotly(p) %>% layout(dragmode = "pan")
    p
  })
  
  output$ncca10_wc_gl_box <- renderPlotly({
    filtered <-
      ncca10_wc_gl %>%
      filter(PARAMETER == input$parameterui_gl,
             STATE == input$stateui_gl)
    ecoreg <- unique(filtered$NCCR_REG)
    units <- unique(filtered$UNITS)
    allStates <- 
      ncca10_wc_gl %>%
      filter(PARAMETER == input$parameterui_gl)
    p <- ggplot(allStates[allStates$NCCR_REG==ecoreg,], aes(x = STATE, y = RESULT)) + geom_boxplot() +
      theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle(paste(ncca10_wc_gl[ncca10_wc_gl$PARAMETER==input$parameterui_gl,7])) +
      xlab('State') + ylab(paste('Concentration in',units,sep = ' '))
    p <- ggplotly(p)  %>% 
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
