library(shiny)
ui <- fluidPage(
  # Application title
  titlePanel("Fitbit Data"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = paste('Date Range'),
                     start = Sys.Date() - 7, end = Sys.Date(),
                     separator = " - ", format = "mm/dd/yy",
                     startview = 'month', weekstart = 1
      ),
      checkboxGroupInput("measures", "Measures:",
                          c("Steps" = "steps",
                            "Distance" = "distance",
                            "Floors" = "floors",
                            "Minutes Very" = "minutesVery",
                            "Calories Burned" = "cal_ratio",
                            "Resting Heart Rate" = "hr",
                            "Time in Heart Rate Zones" = "hr_zones",
                            "Sleep" = "sleep",
                            "Weight" = "weight",
                            # Intra-day
                            "Active Minutes" = "active-minutes",
                            "Calories Burned" = "calories-burned",
                            "Heart Rate" = "bpm"))
      
      # Daily basis (once per day):
      # from get_daily_data
      # steps
      # distance
      # floors
      # minutesVery
      # caloriesBurnedVsIntake
      # getTimeInHeartRateZonesPerDay
      # getRestingHeartRateData
      # from get_sleep_data
      # sleep
      # from get_weight_data
      # weight
      
      # Intra-day (multiple times per day):
      # from get_intraday_data
      # steps
      # distance
      # floors
      # active-minutes
      # calories-burned
      # heart-rate
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    
  })
  
}

shinyApp(ui, server)