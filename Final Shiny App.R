library(fpp3)
library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)

#data
us_gasoline$Barrels <- as.numeric(us_gasoline$Barrels)
#copy of orignal data if needed
us_gas <- us_gasoline
#data used in app
midterm_data <- us_gas
midterm_data$year <- year(midterm_data$Week)

#for holts and holts/winter
midterm_data_monthly <- midterm_data %>%
  index_by(Month = yearmonth(Week)) %>%
  summarise(
    BarrelsSum = sum(Barrels),
    BarrelsMean = mean(Barrels),
    BarrelsMed = median(Barrels)
  )
  


ui <- fluidPage(
  theme = shinytheme("slate"),
  tabsetPanel(
    tabPanel(
      title = 'Information Page',
      textOutput('text_instruction')
    ),
    tabPanel(  
      title = 'US Gasonline Data (1991-2017)',
      plotOutput('ts_plot'),
      textOutput('time_series_text'),
      ),
    tabPanel(  
      title = 'Analyzing US Gasonline Data (1991-2017) Trends',
      selectInput(
        inputId = "select_plot",
        label = "Select a analysis plot",
        choices = c("Seasonality", "Autocorrelation", "Decomposition")),
        plotOutput('selected_plot'),
        textOutput('interpretation'),
       ),
    tabPanel(
      title = 'Percent Change in Barrels used by Year',
      selectInput(
        inputId = "select_year",
        label = "Select a Year",
        choices = unique(midterm_data$year)),
        plotlyOutput('selected_year')
      ),
    tabPanel(  
      title = 'US Gasonline Data (1991-2017) Simple Models',
      selectInput(
        inputId = "select_model",
        label = "Select a Simple Model",
        choices = c("Naive Model", "Seasonal Naive Model", "Mean Model", "Drift Model")),
      plotOutput('selected_model'),
    ),
    tabPanel(  
      title = 'US Gasonline Data (1991-2017) Exponential Smoothing Models',
      selectInput(
        inputId = "select_ets",
        label = "Select a Exponential Smoothing Model",
        choices = c("Holts", "Holts/Winter")),
      plotOutput('selected_ets'),
    ),
    tabPanel(  
      title = 'US Gasonline Data (1991-2017) ARIMA Models',
      selectInput(
        inputId = "select_arima",
        label = "Select a type of ARIMA Model",
        choices = c("Automated Model", "Manual Model")),
      plotOutput('selected_arima'),
    ),
    )
  )




server <- function(input, output) {
  output$text_instruction <- renderText({
    "Welcome to my app. Here you find data that analyzes the Barrels of gasoline 
    used weekly in the United States from 1991-2017. If you follow the tabs to 
    'US Gasonline Data (1991-2017)' you will find a complete graph showing the 
    entire data from 1991-2017. If you move to the next tab, you'll be asked to 
    select a certain type of analysis on the data's. Selecting a plot will give 
    you a plot, along with a little information about what the graph is telling 
    you. In the last tab you'll find a box that ask you for the year. This graph
    will show you you the percent change from day to day over the year."
  })
  
  output$time_series_text <- renderText({
    "From 1991 to 2017, the number of barrels of gas used each week shows a 
    positve trend over time. Additonally, we can see a slight trend downward 
    from around 2008 to 2012."
  })
  output$ts_plot <- renderPlot({
    
    plot_df <- midterm_data
    autoplot(plot_df) + xlab("Year")
  })
  
  
  output$selected_plot <- renderPlot({
    
    if(input$select_plot == "Seasonality"){
      gg_lag(midterm_data, geom = 'point')
    }
    else if(input$select_plot == "Autocorrelation"){
      autoplot(ACF(midterm_data))
    }
    else if(input$select_plot == "Decomposition"){
      midterm_data %>%
        model(classical_decomposition(Barrels, type = 'multiplicative')) %>%
        components() %>%
         autoplot() + xlab("Year")
    }
  })
  
  output$interpretation <- renderText({
    "Here you will find 3 different graphs that show different trends in our 
     data. 
    
     In seasonality, each week throughout the year is represented by a dot.
     This plot does not suggest we have much seasonlity because each lag 
     reflects strongly of each other.
    
     In autocorrelation, we can see that each of the period 
     strongly correlates with the period in front of it. It does however take a 
     small drop off near the end suggesting there could be some seasonlity, but 
     not much.  
     
     In Decomposition, we can see that over time, the number of barrels is 
     generally trending upward with very little variability in the data. Since 
    the data trends upwards over time it causes some variability in the 
    seasonality part of our model "
  })
  
  output$selected_year <- renderPlotly({
    filtered_data <- midterm_data[midterm_data$year == input$select_year, ]
    filtered_data <- filtered_data[ , c("Week","Barrels")]
    filtered_data$percent_change <- c(NA, diff(filtered_data$Barrels))
    
    autoplot(filtered_data, .vars = percent_change) %>%
      ggplotly()
    
  })
  
  output$selected_model <- renderPlot({
    
    if(input$select_model == "Naive Model"){
      
     naive.model <- midterm_data %>%
        model(NAIVE(Barrels))
     
     naive.model %>%
       forecast() %>%
       autoplot(midterm_data) + 
       labs(title = "Naive Model")
    }
    else if(input$select_model == "Seasonal Naive Model"){
      
      snaive.model <- midterm_data %>%
        model(SNAIVE(Barrels))
      
      snaive.model %>%
        forecast() %>%
        autoplot(midterm_data) + 
        labs(title = "Seasonal Naive Model")
    }
    else if(input$select_model == "Mean Model"){
      
     mean.model <- midterm_data %>%
        model(MEAN(Barrels))
     
     mean.model %>%
       forecast() %>%
       autoplot(midterm_data) +
       labs(title = "Mean Model")
    }
    else if(input$select_model == "Drift Model"){
      
      drift.model <- midterm_data %>%
        model(RW(Barrels ~ drift()))
      
      drift.model %>%
        forecast() %>%
        autoplot(midterm_data) +
        labs(title = "Drift Model")
    }
  })
  
  output$selected_ets <- renderPlot({
    
    if(input$select_ets == "Holts"){
      
      holt.ets <- midterm_data_monthly %>%
        model(ETS(BarrelsMean ~ error("A") + trend("A") + season("N")))
      
      holt.ets %>% 
        forecast() %>%
        autoplot(midterm_data_monthly) + 
        labs(title = "Holts ETS Model")
    }
    else if(input$select_ets == "Holts/Winter"){
      
      holts.winter.ets <- midterm_data_monthly %>%
        model(
          additive = ETS(BarrelsMean ~ error("A") + trend("A") + season("A")),
          multiplicative = ETS(BarrelsMean ~ error("M") + trend("A") + season("M")),
        )
      
      holts.winter.ets %>%
        forecast() %>%
        autoplot(midterm_data_monthly, level = NULL) +
        labs(title = "Holts/Winter ETS Model") + 
        guides(colour = guide_legend(title = "Forecast"))
    }
  })
  
  output$selected_arima <- renderPlot({
      
      if(input$select_arima == "Automated Model"){
        midterm_data %>%
          model(ARIMA(Barrels)) %>%
          forecast() %>%
          autoplot()
      }
      else if(input$select_model == "Manual Model"){
        midterm_data %>%
          model(ARIMA(Barrels ~ pdq(2, 1, 0))) %>%
          forecast()%>%
          autoplot()
      }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
