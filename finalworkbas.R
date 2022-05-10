
library(forecast)
library(fpp3)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggeasy)
library(ggplot2)
data(aus_livestock)
library(shiny)

ui <- navbarPage(strong("Interactive Austrialian Livestock App for States in Australia"), fluid = TRUE,
                 tabPanel("Seasonalities",
                          sidebarLayout(
                            sidebarPanel(
                              awesomeRadio(
                                inputId = "Choice",
                                label = "Pick an Option", 
                                choices = c("Seasonality", "Autocorrelation","Decomposition"),
                                selected = "Seasonality",
                                status = "warning"
                              ),
                              tags$hr(),
                              pickerInput(
                                inputId = "Animal",
                                label = "Pick an Animal", 
                                choices = levels(aus_livestock$Animal),
                                selected = "Pigs",
                                options = list(
                                  title = "Choose an Animal")
                              ),
                              pickerInput(
                                inputId = "State",
                                label = "Pick a State", 
                                choices = levels(aus_livestock$State),
                                selected = "Victoria",
                                options = list(
                                  title = "Choose a State")
                              ),
                            ),
                            mainPanel(
                              p("The following application will allow the user to see different visualizations regarding  the count of Australian livestock from different states. The user will select an animal, as well as select the specific state they want to look at, and then select the type of visualization. The application will then generate the appropriate chart."), 
                              plotlyOutput("Hist2"),
                              plotOutput("Hist"),
                              strong("Interpretation for the time series models pictured", style = "color:Blue"),
                              p("In order to better visualize seasonality, I created seasonal subseries plots in order to better visualize the seasonality trends. For our example we will be looking at Pigs in the state of Victoria. When observing the seasonality graph the blue line represents the mean for each month. As we can see there was a slight increase of the mean for pigs over these years. Moving onto the autocorrelatioon plot, An autocorrelation function or ACF was used to measure the relationship between the animals selected and its state. from our model we can see there is a decrease in the measure as the lag increases, this is due to the trend that was found in the components of y and itself. Although there was a decrease in the measure as the lag increased, on months 12 we see that the measure is greater than its surrounding months. Finally the decomposition graph breaks down the components found within the time series. The visualization for decomposition within this time series breaks down the graph and depicts the count for the selected animal, clearly shows the trend, and models the seasonality over the months this data was pulled from. Within this decomposition the two main factors that affect the Count was found to be the trend-cycle and the season. When observing all of these we can see where seasonality is low and the trend that relates to the variable  being measured. Additionally, reviewing all of these charts allows the analyist to create better forcasts due to easily being able to see how the graphs affect the count. We can also study these charts to see if there was something occuring in the economy that could have affected the item being measured.For our case we are observing the different variables that affect the Australian livestock count for specific animals and the state chosen."),
                              
                            ),
                          )
                 ), 
                 
                 
                 tabPanel("HoltsWay",
                          sidebarLayout(
                            sidebarPanel(
                              awesomeRadio(
                                inputId = "Choice3",
                                label = "Select Exponential Smoothing Model", 
                                choices = c("Holts", "Holts/Winters"),
                                selected = "Holts"
                              ),
                              pickerInput(
                                inputId = "State2",
                                label = "Pick a State", 
                                choices = levels(aus_livestock$State),
                                selected = "Victoria",
                                options = list(
                                  title = "Choose a State")
                              ),
                              pickerInput(
                                inputId = "Animal2",
                                label = "Pick an Animal", 
                                choices = levels(aus_livestock$Animal),
                                selected = "Pigs",
                                options = list(
                                  title = "Choose an Animal")
                              ),
                            ),
                            mainPanel(
                              plotOutput("Hist4"),
                            ),
                          )
                 ),
                 tabPanel("SIMPLE",
                          sidebarLayout(
                            sidebarPanel(
                              radioGroupButtons(
                                inputId = "Choice1",
                                label = "Select a Simple Model",
                                choices = c("Naive", 
                                            "Seasonal Naive", "Mean", "Drift"),
                                selected = "Naive"
                              ),
                              pickerInput(
                                inputId = "Animal1",
                                label = "Pick an Animal", 
                                choices = levels(aus_livestock$Animal),
                                selected = "Pigs",
                                options = list(
                                  title = "Choose an Animal")
                              ),
                              pickerInput(
                                inputId = "State1",
                                label = "Pick a State", 
                                choices = levels(aus_livestock$State),
                                selected = "Victoria",
                                options = list(
                                  title = "Choose a State")
                              ),
                            ), 
                            mainPanel(
                              plotOutput("Hist3"),
                            ),
                          )
                 ),
                 tabPanel("ARIMA",
                   sidebarLayout(
                     sidebarPanel(
                       radioGroupButtons(
                         inputId = "Choice4",
                         label = "Select an ARIMA Model",
                         choices = c("Manually Selected", 
                                     "Auto Selected"),
                         selected = "Manually Selected"
                       ),
                       pickerInput(
                         inputId = "Animal3",
                         label = "Pick an Animal", 
                         choices = levels(aus_livestock$Animal),
                         selected = "Pigs",
                         options = list(
                           title = "Choose an Animal")
                       ),
                       pickerInput(
                         inputId = "State3",
                         label = "Pick a State", 
                         choices = levels(aus_livestock$State),
                         selected = "Victoria",
                         options = list(
                           title = "Choose a State")
                       ),
                     ),
                     mainPanel(
                       plotOutput("Hist5"),
                     ),
                   )
                 )
                 
)


server <- function(input, output, session) {
  output$Hist2 <- renderPlotly({
    aus_livestock %>% 
      filter(Animal == input$Animal,
             State == input$State) %>% 
      autoplot()
  })
  
  output$Hist <- renderPlot({
    
    if(input$Choice == "Seasonality"){
      aus_livestock %>% 
        filter(Animal == input$Animal,
               State == input$State) %>% 
        gg_subseries()
    }else
      if(input$Choice == "Autocorrelation"){
        aus_livestock %>% 
          filter(Animal == input$Animal,
                 State == input$State) %>% 
          ACF(Count, lag_max = 48) %>%
          autoplot()  
      }else
        if(input$Choice == "Decomposition"){
          aus_livestock %>% 
            filter(Animal == input$Animal,
                   State == input$State) %>% 
            model(
              classical_decomposition(Count, type = "additive")
            ) %>%
            components() %>%
            autoplot()}
    
  })
  
  
  output$Hist3  <- renderPlot({
    if(input$Choice1 == "Naive"){
      train <- aus_livestock %>% 
        filter(Animal == input$Animal1,
               State == input$State1)
      Naivefit <- train %>% 
        model(`Naïve` = NAIVE(Count))
      Naivefc <- Naivefit %>% forecast(h=12)
      Naivefc %>%
        autoplot(train, level = NULL)
    }else
      if(input$Choice1 == "Seasonal Naive"){
        train <- aus_livestock %>% 
          filter(Animal == input$Animal1,
                 State == input$State1)
        SNaivefit <- train %>% 
          model(SNAIVE(Count~ lag("year")))
        SNaivefc <- SNaivefit %>% forecast(h=12)
        SNaivefc %>%
          autoplot(train, level = NULL)
      }else
        if(input$Choice1 == "Mean"){
          train <- aus_livestock %>% 
            filter(Animal == input$Animal1,
                   State == input$State1)
          Meanfit <- train %>% 
            model(MEAN(Count))
          Meanfc <- Meanfit %>% forecast(h=12)
          Meanfc %>%
            autoplot(train, level = NULL)
        }else
          if(input$Choice1 == "Drift"){
            train <- aus_livestock %>% 
              filter(Animal == input$Animal1,
                     State == input$State1)
            Driftfit <- train %>% 
              model(RW(Count ~ drift()))  
              Driftfc <- Driftfit %>% forecast(train)
              Driftfc %>%
                autoplot(train, level = NULL)
          }
  })
  
  output$Hist4 <- renderPlot({
    if(input$Choice3 == "Holts"){
      aut_an <- aus_livestock %>%
        filter(Animal == input$Animal2,
               State == input$State2) %>%
        summarise(Count = sum(Count))
      autoplot(aut_an, Count)
    }else
      if(input$Choice3 == "Holts/Winters"){
        aut_an <- aus_livestock %>%
          filter(Animal == input$Animal2,
                 State == input$State2) %>%
          summarise(Count = sum(Count))
        fit <- aut_an %>%
          model(
            additive = ETS(Count ~ error("A") + trend("A") +
                             season("A")),
            multiplicative = ETS(Count ~ error("M") + trend("A") +
                                   season("M"))
          )
        fc <- fit %>% forecast(h = "12 months")
        fc %>%
          autoplot(aut_an, level = NULL) +
          labs(title="Australian livestock Count",
               y="Livestock_Count") +
          guides(colour = guide_legend(title = "Forecast"))
      }
  })
  
  output$Hist5 <- renderPlot({
    if(input$Choice4 == "Manually Selected"){
      train2 <- aus_livestock %>%
        filter(Animal == input$Animal3,
               State == input$State3)
      ARIMA_fit <- train2 %>% 
        model(arima210 = ARIMA(Count ~ pdq(2,1,0)),
              arima013 = ARIMA(Count ~ pdq(0,1,3)),
              stepwise = ARIMA(Count),
              search = ARIMA(Count, stepwise=FALSE))
      ARIMA_fc <- ARIMA_fit %>% forecast(h=12) 
      ARIMA_fc %>% 
        autoplot(train2, level = NULL)
    }else
      if(input$Choice4 == "Auto Selected"){
       train2 <- aus_livestock %>% 
          filter(Animal == input$Animal3,
                 State == input$State3)
        ARIMAfit <- train2 %>%
          model(ARIMA(Count))
          ARIMAfc <- ARIMAfit %>% forecast(h=12)
          ARIMAfc %>%
            autoplot(train2, level = NULL)
         
      }
  })
  
}


shinyApp(ui, server)