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

ui <- dashboardPage(
  dashboardHeader(title = "Time Series Plots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HomePage", tabName = "homepage", icon = icon("xbox")),
      menuItem("Seasonal Plot", tabName = "graph1", icon = icon("home")),
      menuItem("AutoCorrelation Plot", tabName = "graph2", icon = icon("user-astronaut")),
      menuItem("Decomposition Plot", tabName = "graph3", icon = icon("aws")),
      menuItem("Time Series Linear Model", tabName = "graph4", icon = icon("sun"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "homepage",
              
              fluidPage(
                headerPanel(title = "Time Series Plots Homepage", windowTitle = "Time Series Homepage"),
                h3("Welcome to the instructions portion. Please select which type of time series plot you would like to use"),
                h5("There are four different types of time series you can choose from: Seasonality, decomposition, autocorrelation and time series linear. Please go through all tabs. each tab will
                   already have the appropriate graph selected"))
    ),
    tabItem(tabName = "graph1",
            fluidPage(
              titlePanel(h2("Seasonal Plot of Occupancy in NSW", align= "center")),
              pickerInput("Picker1",
                          label = "Pick a graph",
                          choices = c("graph", "seasonality")),
              plotOutput("accomidation_plot")
                
              
            )),
    tabItem(tabName = "graph4",
            fluidPage(
              titlePanel(h2("Time Series Linear Model of Aus Occupancy", align = "center")),
              pickerInput("picker4",
                          label = "Select a graph", choices = "TSLM")),
            plotOutput("TSLM_graph")
            ),
    
    tabItem(tabName =  "graph3",
            fluidPage(
              titlePanel(h2("Decomposition plot of Aus Occupancy", align = "center")),
              pickerInput("picker3",
                          label = "Select decomposition", choices = "Decomposition")),
            plotOutput("decomp_graph")
            ),
    tabItem(tabName = "graph2",
            fluidPage(h2("Autocorrelation Plot of Aus Occupancy", align = "center"),
            pickerInput("picker2",
                        label = "Select autocorrelation", choices = "autocorrelation")),
    plotOutput("auto_graph")
    ))
      
    )  
    )



server <- function(input, output, session) {
output$accomidation_plot <- renderPlot({
  aus_accommodation %>%
    filter(State == "New South Wales") %>%
    gg_season(Occupancy)
})

output$TSLM_graph <-renderPlot({
  aus_accommodation %>%
    model(
      Linear = TSLM(log10(Occupancy) ~ trend())
    ) %>%
    forecast(h=20) %>%
    autoplot(aus_accommodation)
})

output$decomp_graph <- renderPlot({
  aus_accommodation %>%
    filter(State == "New South Wales") %>%
    model(
      classical_decomposition(Occupancy)
    )%>%
    components()%>%
    autoplot()
    
})
output$auto_graph <-renderPlot({
  aus_accommodation %>%
    ACF(Occupancy, lag_max = 20) %>%
    autoplot() + labs(title = "Occupancy in Aus")
})

}

shinyApp(ui = ui, server=server)

