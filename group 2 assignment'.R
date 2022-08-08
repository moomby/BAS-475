library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(ggplot2)
library(tidyquant)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(tsibble)
library(quantmod)


linebreaks <- function(n){HTML(strrep(br(), n))}


ui <- dashboardPage(
  
  dashboardHeader(title = "Stock Web Application"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Home Page", tabName = "homepage", icon = icon("home")),
      
      menuItem("Stock History", tabName = "featureone",  icon = icon("lightbulb")),
      
      menuItem("Month Of July 2022 Stock Price", tabName = "feature2", icon = icon("sun")),
      
      menuItem("Feature Three", tabName = "featurethree", icon = icon("money-bill")),
      
      menuItem("Feature 4", tabName = "Feature 4", icon = icon("xbox"))
      
      
    )
  ), 
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "homepage",
              
              fluidPage(
                headerPanel(title = "Stock Application Homepage", windowTitle = "Stock App Homepage"),
                tags$img(src = "wsb.png"),
                
                linebreaks(3),
                
                h3("You can't lose money if you never sell", " - Warren Buffet"),
                
                h5("Welcome to the instructions.
                   Review the sidebar panels and choose which feature you would like to see"),
                
                
                
              )),
      
      tabItem(tabName = "featureone",
              
              fluidPage(
                
                textInput("plot1.ticker", label = ("Enter ticker symbol:"), value = "PEP"),
                
                dateRangeInput("plot1.dates",label = ("Date range"), 
                               start = "2018-01-01", end = Sys.Date()),
                
                plotOutput("plot1"),
                
                linebreaks(1),
                
                h4("High/Low Stock Price Data:"),
                
                br(),
                
                textOutput("high"),
                
                br(),
                
                textOutput("low"),
                h5("Search a company in the enter ticker symbol. Next, select date range. The dates can go back as far as you would like and are able to review history of the prices of a stock")
                
              )),
      
      tabItem(tabName = "feature2",
              
              fluidPage(
                
                textInput("plot2.ticker", label= ("Enter ticker symbol:"), value = "HD"),
                
                dateRangeInput("plot2.dates", label = ("Date Range"),
                               
                               start = "2022-07-01", end = Sys.Date()),
                
                plotOutput("plot2"),
                h5("Search a company symbol in the ticker. The date range is set to review the prices of the searched stock for this month of July")
                
              )),
      
      tabItem(tabName = "featurethree",
              fluidPage(
                textInput("plot3.ticker", label = ("pick multiple stocks"), value ="XOM"),
                dateRangeInput("plot3.dates",label = ("p3.Date range"), 
                               start = "1994-01-01", end = Sys.Date()),
                plotOutput("plot3")
                               
              )),
              
              
    tabItem(tabName = "Feature 4",
            fluidPage(
              textInput("plot4.ticker", label = "Pick Stock", value = "HD"),
              dateRangeInput("plot4.dates", label = "Date range",
                             start = "1994-01-01", end = Sys.Date()),
              plotOutput("plot4"),
              textOutput("Average"))
                      
      
      
    )
    )
)
)







server <- function(input, output) {
  
  plot1.data <- reactive({
    
    getSymbols(input$plot1.ticker, auto.assign = FALSE,
               
               from = input$plot1.dates[1], to = input$plot1.dates[2])
    
  })
  
  output$plot1 <- renderPlot({
    
    chartSeries(plot1.data())
    
  })
  
  
  
  output$high <- reactive({
    
    data <- as.data.frame(plot1.data())
    
    colnames(data)[2] = "High"
    
    high <- max(data$High)
    
    date <- rownames(which(data == high, arr.ind = TRUE))
    
    paste("High: ", round(high, 2), "occurred on ", date)
    
  })
  
  output$low <- reactive({
    
    data <- as.data.frame(plot1.data())
    
    colnames(data)[3] = "Low"
    
    low <- min(data$Low)
    
    date <- rownames(which(data == low, arr.ind = TRUE))
    
    paste("Low: ", round(low, 2), "occurred on ", date)
    
  })
  
  
  
  
  plot2.data <- reactive({
    getSymbols(input$plot2.ticker, auto.assign = FALSE,
               from = input$plot2.dates[1], to = input$plot2.dates[2])
  })
  output$plot2 <- renderPlot({
    chartSeries(plot2.data())
  })
  
  
plot3.data <- reactive({
  getSymbols(input$plot3.ticker, auto.assign = F,
             from = input$plot3.dates[1], to = input$plot3.dates[2])
})  

output$plot3 <- renderPlot({
  chartSeries(plot3.data())
})  
plot4.data <- reactive({
  getSymbols(input$plot4.ticker, auto.assign = F,
             from = input$plot4.dates[1], to = input$plot.4dates[2])
})

output$plot4 <-renderPlot({
  chartSeries(plot4.data())
})



output$avg <- reactive({
  data <- as.data.frame(plot4.data)
  colnames(data)[2] = "avg"
  
  Average <- mean(data$avg)
  
  date <- rownames(which(data == avg, arr.ind = TRUE))
  
  paste("avg: ", round(avg, 2), "occurred on ", date)
})  




  
}


shinyApp(ui = ui, server = server)