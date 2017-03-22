#' @include global_var.R Person.R experiments.R viz_daily.R viz_sleep.R viz_intraday.R
library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("lifelogr"),
  tabsetPanel(
    tabPanel("Setup",
             fluidRow(
               column(3,
                      h3("Enter setup information:"),
                      textInput("name", label = "Name"),
                      numericInput("age", label = "Age (in years)", value = 40),
                      radioButtons("gender", label = "Gender",
                                   c("Female" = "female",
                                     "Male" = "Male",
                                     "Other" = "other")),
                      actionButton("done", "Done")
                      ),
               column(3,
                      br(),
                      textInput("email", label = "Fitbit Email", 
                                value = "example@domain.com"),
                      passwordInput("password", "Fitbit Password:"),
                      br(),
                      numericInput("target_steps", label = "Target Steps Per Day", 
                                   value = 10000),
                      dateRangeInput("dates", label = "Date range",
                                     start = "2017-03-18", end = "2017-03-20"),
                      fileInput("apple", label = "Apple Data", 
                                multiple = FALSE,
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"))),
               column(6,
                      tableOutput("table"))
             )
             ),
    tabPanel("Sleep",
             sidebarLayout(
               sidebarPanel(
                 # user should be able to adjust units, etc.
                   radioButtons(inputId = "sleep_measure", "Measures:",
                                c("By Weekday" = "by_weekday",
                                  "By Start and End Time" = "by_start_end_time",
                                  "By Date-Time" = "by_datetime",
                                  "By Proportion of Restless Sleep" = "by_restless_prop",
                                  "By Length of Restless Sleep" = "by_restless_min",
                                  "By Quality of Sleep" = "by_quality"),
                                selected = "by_weekday"
                   )
               ),
               
               mainPanel(
                 plotOutput("sleepPlot")
               )
             )
    ),
    
    tabPanel("Daily Totals",
             sidebarLayout(
               sidebarPanel(
                 # user should be able to adjust units, etc.
                 radioButtons(inputId = "daily_measure", "Measures:",
                              c("Steps" = "steps",
                                "Floors" = "floors",
                                "Distance" = "distance",
                                "Calories Burned/Consumed" = "calories",
                                "Minutes 'Very Active'" = "mins_very",
                                "Resting Heart Rate" = "rest_hr"),
                              selected = "steps")
               ),
               mainPanel(
                 plotOutput("dailyPlot")
               )
             )
    ),
    
    tabPanel("Typical Day",
             sidebarLayout(
               sidebarPanel(
                 # user should be able to adjust units, etc.
                 radioButtons(inputId = "typical_day_measure", "Measures:",
                              c("Steps" = "steps",
                                "Floors" = "floors",
                                "Distance" = "distance",
                                "Calories Burned" = "caloriesBurned",
                                "Minutes 'Active'" = "activeMin",
                                "Heart Rate" = "bpm",
                                "Weight" = "weight"),
                              selected = "steps"
                 )
               ),
               
               mainPanel(
                 plotOutput("typicalDayPlot")
               )
             )
    ),
    
    tabPanel("Over All Time in the Range",
             sidebarLayout(
               sidebarPanel(
                 # user should be able to adjust units, etc.
                 radioButtons(inputId = "over_all_time_measure", "Measures:",
                              c("Steps" = "steps",
                                "Floors" = "floors",
                                "Distance" = "distance",
                                "Calories Burned" = "caloriesBurned",
                                "Minutes 'Active'" = "activeMin",
                                "Heart Rate" = "bpm",
                                "Weight" = "weight"),
                              selected = "steps"
                 )
               ),
               
               mainPanel(
                 plotOutput("overAllTimePlot")
               )
             )
    ),
    
    tabPanel("Experimentation",
             sidebarLayout(
               sidebarPanel(
                 h3("Variables (x)"),
                 checkboxGroupInput("fitbit_daily", "Daily Variables",
                                    c("Steps" = "steps",
                                      "Distance (mi)" = "distance",
                                      "Floors" = "floors",
                                      "Calories Burned" = "caloriesBurned",
                                      "Calories Consumed" = "caloriesIntake",
                                      "Resting Heart Rate" = "restingHeartRate",
                                      "Restlessness Duration" = "restlessDuration",
                                      "Restlessness Proportion" = "restlessProp",
                                      "Minutes Asleep" = "minAsleep",
                                      "Sleep Quality Score" = "sleepQUalityScoreA")),
                 # Not done with this yet
                 checkboxGroupInput("fitbit_intraday", "Intraday Variables:",
                                    c("Steps" = "steps",
                                      "Distance (mi)" = "distance",
                                      "Floors" = "floors",
                                      "Calories Burned" = "caloriesBurned",
                                      "Calories Consumed" = "caloriesIntake",
                                      "Resting Heart Rate" = "restingHeartRate",
                                      "Restlessness Duration" = "restlessDuration",
                                      "Restlessness Proportion" = "restlessProp",
                                      "Minutes Asleep" = "minAsleep",
                                      "Sleep Quality Score" = "sleepQUalityScoreA"))
               ),
               
               mainPanel(
                 # plotOutput("overAllTimePlot")
               )
             )
    )
  )

)


server <- function(input, output) {
  
  df <- eventReactive(input$done, {
    person <- Person$new(fitbit_user_email = input$email, 
                     fitbit_user_pw = input$password,
                     apple_data_file = input$apple,
                     user_info = list("name" = input$name, 
                                      "age" = input$age, 
                                      "gender" = input$gender),
                     target_steps = input$target_steps,
                     #group_assignments = list(data.frame(NA), data.frame(NA)),
                     start_date = input$dates[1], end_date = input$dates[2])
    #print(person$fitbit_intraday)
  })
  
  output$table <- renderText({""})#{person$fitbit_intraday})
  
  #output$table <- renderText({df$fitbit_intraday})#renderTable({
    #person$fitbit_intraday
    #})
  #{
    #df()
  #})

  output$sleepPlot <- renderPlot({
    plot_sleep(df(), input$sleep_measure)
  })
  
  output$dailyPlot <- renderPlot({
    # eventReactive((input$daily_measure == 'distance'), {
    #   print(input$unit)
    #   plot_daily(person, input$daily_measure, input$unit)
    # })
    plot_daily(person, input$daily_measure)
  })
  
  output$typicalDayPlot <- renderPlot({
    plot_intraday(person, input$typical_day_measure, TRUE)
  })
  
  output$overAllTimePlot <- renderPlot({
    plot_intraday(person, input$over_all_time_measure, FALSE)
  })
  
}

shinyApp(ui, server)