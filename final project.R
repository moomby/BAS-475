library(fpp3)
library(dplyr)
library(tidyr)
library(ggeasy)
library(readxl)
library(tsibble)
library(zoo)
library(kableExtra)
library(ggplot2)
library(plotly)
library(fpp3)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(ggplot2)
library(tidyquant)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(tsibble)
library(fpp3)
library(shiny)
install.packages("rsconnect")
library(rsconnect)
view(aus_production)


ui <- ui <- dashboardPage(
  dashboardHeader(title = "Models for forecasting Australian Beer Production"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HomePage", tabName = "homepage", icon = icon("xbox")),
      menuItem("Naive Model", tabName = "naive", icon = icon("home")),
      menuItem("Seasonal Naive Model", tabName = "snaive", icon = icon("user-astronaut")),
      menuItem("Mean Model", tabName = "mean", icon = icon("aws")),
      menuItem("Drift Model", tabName = "drift", icon = icon("sun")),
      menuItem("Holts Model", tabName = "holts", icon = icon("android")),
      menuItem("Holts Winters Model", tabName = "holtsw", icon = icon("archway")),
      menuItem("Manual ARIMA Model", tabName = "manualARIMA", icon = icon("ankh")),
      menuItem("Auto ARIMA Model", tabName = "autoARIMA", icon = icon("atom"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "homepage",
              fluidPage(
                headerPanel(title = "Time Series Plots Homepage", windowTitle = "Time Series Homepage"),
                h3("Welcome to the instructions portion. Please select which type of time series plot you would like to use"),
                h5("There are nine different types of time series you can choose from: naive, seasonal naive, mean, drift, holts, holts winters, manual arima, and auto arima. Please go through all tabs. each tab will
                   already have the appropriate graph selected"))
      ),
      tabItem(tabName = "naive",
              fluidPage(
                titlePanel(h2("Naive Model of Beer Production in NSW", align= "center")),
                pickerInput("Picker1",
                            label = "Pick a graph",
                            choices = c("graph", "seasonality")),
                plotOutput("naiveplot"))),
      
      tabItem(tabName = "snaive",
              fluidPage(
                titlePanel(h2("Seasonal Naive Model of Beer Production", align = "center")),
                pickerInput("picker2",
                            label = "Select a graph", choices = "SNaive")),
              plotOutput("snaive_graph")),
      
      tabItem(tabName =  "mean",
              fluidPage(
                titlePanel(h2("Mean Model of Beer Production", align = "center")),
                pickerInput("picker3",
                            label = "Select Mean", choices = "Mean")),
              plotOutput("mean_graph")),
      
      tabItem(tabName = "drift",
              fluidPage(h2("Drift Model of Beer Production", align = "center"),
                        pickerInput("picker4",
                                    label = "Select drift", choices = "drift")),
              plotOutput("drift_graph")),
      tabItem(tabName = "holts",
              fluidPage(h2("Holts Model of Beer Production", align = "center"),
                pickerInput("picker5",
                            label = "Select Holts", choices = "Holts")),
              plotOutput("holts_graph")),
      
      tabItem(tabName = "holtsw",
              fluidPage(
                titlePanel(h2("Holts Winter Model of Beer Production", align = "center")),
                pickerInput("picker6",
                            label = "Select Holts Winter", choices = "HW")),
              plotOutput("hw_graph")),
      tabItem(tabName = "manualARIMA",
              fluidPage(
                titlePanel(h2("Manual ARIMA Model of Beer Production", align = "center")),
                pickerInput("picker7",
                            label = "Select Manual", choices = "manual")),
              plotOutput("manual_graph")),
      tabItem(tabName = "autoARIMA",
              fluidPage(
                titlePanel(h2("Auto ARIMA Model of Beer Production", align = "center")),
                pickerInput("picker8",
                            label = "Select Auto", choices = "auto")),
              plotOutput("autograph"))
      
      
      
    
  ))
)

    

server <- function(input, output, session) {
  output$naiveplot <- renderPlot({
    aus_production %>%
      model(
        Naive = NAIVE(Beer))%>%
      forecast(h = 12) %>%
      autoplot(aus_production)
  })
  
  output$snaive_graph <-renderPlot({
    aus_production %>%
      model(
        SNaive = SNAIVE(Beer)) %>%
      forecast(h=12) %>%
      autoplot(aus_production)
  })
  output$mean_graph <- renderPlot({
    aus_production %>%
      model(
        Mean = MEAN(Beer))%>%
      forecast(h = 12) %>%
      autoplot(aus_production)
})
  
  output$drift_graph <-renderPlot({
    aus_production %>%
     model(
       Drift = RW(Beer ~ drift())) %>%
      forecast(h = 12) %>%
      autoplot(aus_production)
  })  
  output$holts_graph <- renderPlot({
    aus_production %>%
      model(
        Holt = ETS(Beer~ error("A") + trend("N") + season("N"))) %>%
      forecast(h = 12) %>%
      autoplot(aus_production)
  })
  
output$hw_graph <- renderPlot({
  aus_production %>%
    model(
      HWinters = ETS(Beer ~ error("A") + trend("A") + season("A"))) %>%
    forecast(h = 12) %>%
    autoplot(aus_production)
    
})

output$manual_graph <- renderPlot({
  aus_production %>%
    model(
      manual = ARIMA(Beer ~ 0 + pdq(2, 1, 1))) %>%
    forecast(h = 12) %>%
    autoplot(aus_production)
    
})

output$autograph <- renderPlot({
  aus_production %>%
    model(
      auto = ARIMA(Beer)) %>%
    forecast(h = 12) %>%
    autoplot(aus_production)
})
}
shinyApp(ui, server)