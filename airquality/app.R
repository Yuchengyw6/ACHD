#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(robotstxt)
library(lubridate)
library(stringr)
library(jsonlite)
source("/Users/wyc/scrape.R")
data = scrape()

# Define server for data output
server <- function(input, output,session) {
  data = reactive({
    invalidateLater(1000000,session)
    
    scrape()
  }) 
  
  # define thw title
  output$title <- renderText(data[[14]])
  
  # define the data table for AQI
  output$aqi_table <- renderTable(c(data[1],data[2],data[3]))
  output$aqi_text <- renderText(data[[4]])
  
  # define the data table for ADI
  output$adi_table <- renderTable(c(data[5],data[6],data[7],data[8]))
  
  # define the inversion report
  output$inversion_line1 <- renderText({
    HTML(paste0("</b>","This morning’s surface inversion of ","<b>", data[[9]],
                "</b>"," with a depth of ","<b>",data[[10]],"</b>",
                " is estimated to break at ","<b>",data[[11]],"</b>","."))
  })
  
  output$inversion_line2 <- renderText({
    HTML(paste0("</b>","This surface inversion can be characterized as: ",
                "<b>", data[[12]],"</b>","."))
  })
  
  output$inversion_line3 <- renderText({
    HTML(data[[13]])      
  })
  
}

# Define UI for application
ui <- fluidPage(
  
  # title
  fluidRow(
    column(8, h2(textOutput('title')), offset = 2)
  ),
  
  # air quality forecast
  p(h4(div("Air Quality Forecast:",style = "color:blue")), 
    h5("This is the daily forecasted Air Quality Index (AQI) for each area 
    provided by the PA Department of Environmental Protection. The AQI is based 
    on PM2.5 or Ozone, whichever is forecasted to be higher.")),
  
  fluidRow(
    column(6, wellPanel(tableOutput('aqi_table'))),
    column(6, p(div("Today's Forecast:",style = "color:blue"), textOutput('aqi_text')))
  ),
  
  fluidRow(
    column(6, "See Page 2 for the Air Quality Index guide"),
    column(6, a("Data provided by the PA Department of Environmental Protection", 
                href="https://www.ahs.dep.pa.gov/AQPartnersWeb/forecast_home.aspx"))
  ),
  
  br(),
  br(),
  
  
  # Air Dispersion 36-Hour Forecast:
  p(h4(div("ACHD Air Dispersion 36-Hour Forecast:",style = "color:blue"), 
       h5("This is the dispersion forecast for Allegheny County starting from this 
    morning through tomorrow afternoon. The atmospheric dispersion index is a 
    rating of the atmosphere’s ability to transport pollution away from its 
    source and is based on emissions and weather. Better atmospheric dispersion 
    can improve air quality."))),
  
  fluidRow(
    column(8, wellPanel(tableOutput('adi_table')),offset = 2),
  ),    
  
  p("Data provided by the National Weather Service (NWS) ",
    a("Fire Weather Planning Forecast", 
      href = "https://forecast.weather.gov/product.php?site=NWS&product=FWF&issuedby=PBZ"),
    "and",
    a("PIT NWS Products", 
      href = "http://weather.uwyo.edu/upperair/sounding.html")),
  
  br(),
  br(),
  
  # guide for ADI and AQI
  div(img(src = "guide.png", height = 400, width = 800), style="text-align: center;"),
  
  br(),
  br(),
  
  # air quality forecast
  p(h4(div("ACHD Surface Temperature Inversion Report:",style = "color:blue")), 
    h5("This is the 7 AM surface-based temperature inversion report for Allegheny County.")),
  
  uiOutput(outputId = "inversion_line1"),
  uiOutput(outputId = "inversion_line2"),
  uiOutput(outputId = "inversion_line3"),
  
  br(),
  br(),
  
  # Surface Temperature Inversion Report meaning
  div(img(src = "report_mean.png", height = 500, width = 1000), style="text-align: center;"),
  
  
)

# Preview the UI in the console
shinyApp(ui, server)
