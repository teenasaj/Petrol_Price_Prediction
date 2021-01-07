library(shiny)
library(shinyWidgets)
library(plotly)

dataread<-read.csv("C:\\Users\\lenovo IP330s\\Desktop\\Python_Project\\petrol5.csv", header = T)

#Loading the dataset
data <- read.csv("C:\\Users\\lenovo IP330s\\Desktop\\Python_Project\\Petrol Price Original.csv", header = T)
datapp <- read.csv("C:\\Users\\lenovo IP330s\\Desktop\\Python_Project\\predicted11.csv", header = T)

# Packages
library(tseries)
library(forecast)
library(dplyr)

# Define UI ----
ui <- fluidPage(
  
  titlePanel(strong("Petrol Price Prediction - KKPP")),
  setBackgroundImage(src = "https://image.shutterstock.com/image-vector/petrol-station-icon-vector-illustration-260nw-1353795233.jpg"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  sidebarLayout(
    sidebarPanel(
      h3(strong("About the Project")),
      br(),
      p("Petrol Price in India varies everyday all over the country."),
      br(),
      p("This is determined by various factors like the price of crude oil , exchange rate etc."),
      br(),
      p("Given the price of petrol in the four Metropolitan cities of India, our project is to predict the price for the next 15 days."),
      br(),
      p("Various Forecasting models have been used for the same."),
      br(),
      img(src = "https://www.autodeals.pk/wp-content/uploads/2018/12/petrol-price-hike-exp.png", height = 350, width = 400)
    ),
    mainPanel( navbarPage("FORECAST MODELS",
                          
                          navbarMenu("DATA ",
                                     tabPanel("VIEW DATA", tableOutput("table1"))),
                          navbarMenu("ARIMA",
                                     tabPanel("Plot",uiOutput("selectbox1a"),plotlyOutput("plot1a")),
                                     tabPanel("Forecast",uiOutput("selectbox1b"),verbatimTextOutput("summ1b"))),
                          
                          navbarMenu("HOLT EXPONENTIAL",
                                     tabPanel("Plot",uiOutput("selectbox2a"),plotlyOutput("plot2a")),
                                     tabPanel("Forecast",uiOutput("selectbox2b"),verbatimTextOutput("summ2b"))),
                          
                          navbarMenu("COMPARISON",
                                     tabPanel("VIEW",uiOutput("selectbox4"),plotlyOutput("plot4")))
                          
    )
    
    
    )
  )
  
  
)


server <- function(input, output){
  
  #print data in the view tab
  output$table1 <- renderTable({
    dataread
  })
  
  #--------------------ARIMA--------------------------
  
  #selectbox city in plot 
  output$selectbox1a <- renderUI({
    
    selectInput(inputId = "select_dev1a",
                
                label = "Select City",
                
                choices = names(data))
    
  })
  
  #selectbox city in forecast 
  output$selectbox1b <- renderUI({
    
    selectInput(inputId = "select_dev1b",
                
                label = "Select City",
                
                choices = names(data))
    
  })
  
  #print plot of arima
  output$plot1a<-renderPlotly({
    a <- hpfilter(data[[input$select_dev1a]],freq=270400,type="lambda",drift=FALSE)
    
    fit_arima1 <- auto.arima(a$trend, lambda = "auto")
    price_forecast1 <- forecast(fit_arima1, h=15)
    d1<-seq(as.Date('2018-01-01'), as.Date('2019-12-31'), by = "days")
    d2<-seq(as.Date('2020-01-01'), as.Date('2020-01-15'), by = "days")
    plot_ly() %>%
      add_lines(x = d1, y = data[[input$select_dev1a]],
                color = I("black"), name = "observed") %>%
      add_lines(x = d2, y = price_forecast1$mean, 
                color = I("red"), name = "prediction")
    
  })
  
  #summary of arima
  output$summ1b <- renderPrint({
    #a <- hpfilter(data[[input$select_dev1b]],freq=270400,type="lambda",drift=FALSE)
    
    fit_arima1 <- auto.arima(data[[input$select_dev1b]], lambda = "auto")
    price_forecast1 <- forecast(fit_arima1, h=15)
    
    summary(price_forecast1)
  })
  
  #--------------------HOLT--------------------------
  
  #selectbox city in plot 
  output$selectbox2a <- renderUI({
    
    selectInput(inputId = "select_dev2a",
                
                label = "Select City",
                
                choices = names(data))
    
  })
  
  #selectbox city in forecast 
  output$selectbox2b <- renderUI({
    
    selectInput(inputId = "select_dev2b",
                
                label = "Select City",
                
                choices = names(data))
    
  })
  
  #print plot of holt
  output$plot2a<-renderPlotly({
    fit_holt1 <- holt(data[[input$select_dev2a]],h=15)
    d1<-seq(as.Date('2018-01-01'), as.Date('2019-12-31'), by = "days")
    d2<-seq(as.Date('2020-01-01'), as.Date('2020-01-15'), by = "days")
    plot_ly() %>%
      add_lines(x = d1, y = data[[input$select_dev2a]],
                color = I("black"), name = "observed") %>%
      add_lines(x = d2, y = fit_holt1$mean, 
                color = I("red"), name = "prediction")
    
  })
  
  #summary of holt
  output$summ2b <- renderPrint({
    fit_holt1 <- holt(data[[input$select_dev2b]],h=15)
    summary(fit_holt1)
  })
  
 
  #-----------------------------COMPARISON---------------------
  
  #selectbox city in forecast 
  output$selectbox4 <- renderUI({
    
    selectInput(inputId = "select_dev4",
                
                label = "Select City",
                
                choices = names(data))
    
  })
  
  output$plot4<-renderPlotly({
    d2<-seq(as.Date('2020-01-01'), as.Date('2020-01-15'), by = "days")
    a <- hpfilter(data[[input$select_dev4]],freq=270400,type="lambda",drift=FALSE)
    
    fit_arima4 <- auto.arima(a$trend, lambda = "auto")
    price_forecast4 <- forecast(fit_arima4, h=15)
    
    fit_holt4 <- holt(data[[input$select_dev4]],h=15)
    
    ar=price_forecast4$mean
    h=fit_holt4$mean
    
    orig=datapp[[input$select_dev4]]
    df4=data.frame(orig,ar,h)
    plot_ly(df4, x = ~d2
    ) %>%
      add_trace(y = ~orig, name = 'Actual Price', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      add_trace(y = ~ar, name = 'ARIMA Forecast', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) %>%
      add_trace(y = ~h, name = 'Holt Forecast', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash')) %>%
      layout(title = "Comparison of the forecasts with the Actual price",
             xaxis = list(title = "Dates of the forecast"),
             yaxis = list (title = "Price"))
    
    
  })
  
  
  
}


# Run the app ----

shinyApp(ui = ui, server = server)
