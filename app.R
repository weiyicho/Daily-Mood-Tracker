library(ggplot2)
library(shiny)
library(dplyr)
library(shinythemes)
library(reticulate)

user_desktop_path <- file.path(current_dir <- getwd(), "data")

source_python("/Users/weiyicho/Desktop/Daliy Mood Tracker/sentiment.py")

ui <- navbarPage(
  
  theme = shinytheme("flatly"),
  title = "Daily Mood Tracker",
  tabPanel("Data Entry",
           fluidRow(
             column(6,
                    br(),
                    dateInput("date", "What date is today?"),
                    sliderInput("scale", "How do you feel today?
                                From 1 to 10, 1 : extremely sad, 10: extremely happy",
                                min = 1, max = 10, value = 1, width =  400),
                    textAreaInput("diary", "How's your day?", width = "400px", height = "400px"),
                    br(),
                    br(),
                    br(),
                    actionButton("submit_button", "save"),
             ),
             column(6,
                    br(),
                    uiOutput("choice"))
           )
  ),
  tabPanel("Data Visualization",
           fluidRow(
             column(6,
                    dateInput("startdate", "Start Date"),
                    dateInput("Enddate", "End Date"),
                    plotOutput("plot"),
                    plotOutput("plot2"),
             ),
             column(6,
                    dateInput("str", "Start Date"),
                    dateInput("end", "End Date"),
                    tableOutput("table")
             )
           )
  ),
  tabPanel("Project.RMD",
           includeMarkdown("Project_Diary.Rmd")
  )
)

low = c(" ", "stress", "Sadiness", "Angry")
middle = c("", "Normal", "Not thing speical", "calm")
high = c(" " ,"Happiness", "Accomplishment", "Relaxation")

server <- function(input, output, session) {
  filepath <- file.path(user_desktop_path, "user_data.csv")
  
  output$choice = renderUI({
    if(input$scale <= 3) {
      selectInput("reason", "Reason of Mood",low)
    } else if(input$scale <= 6) {
      selectInput("reason", "Reason of Mood",middle)
    } else {
      selectInput("reason", "Reason of Mood",high)
    }
  })
  
  read_data <- function() {
    if (file.exists(filepath)) {
      return(read.csv(filepath))
    } else {
      return(data.frame(Date=character(), Edit_time=character(), Mood=numeric(), Diary=character(), Reason=character(), Mood_total =  numeric(),stringsAsFactors=FALSE))
    }
  }
  
  filter_date <- reactive({
    csv <- read_data()
    csv |> filter(Date >= input$startdate & Date <= input$Enddate)
  })
  
  table_date <- reactive({
    csv <- read_data()
    csv |> filter(Date >= input$str & Date <= input$end) |> select(Mood)
  })
  
  observeEvent(input$submit_button, {
    new_entry <- data.frame(Date=as.character(input$date), Edit_time=as.character(Sys.time()), Mood=input$scale, Diary=input$diary, Reason=input$reason, Mood_total = sentiment_cal(input$diary,input$scale), stringsAsFactors=FALSE)
    existing_data <- read_data()
    updated_data <- rbind(existing_data, new_entry)
    write.csv(updated_data, filepath, row.names = FALSE)
  })
  
  output$plot <- renderPlot({
    num_days <- as.numeric(difftime(input$Enddate, input$startdate, units = "days"))
    
    base_width <- 400
    increment_per_day <- 20
    
    dynamic_width <- max(base_width, base_width + num_days * increment_per_day)
    
    p = ggplot(data = filter_date(), mapping = aes(x = Date, y= Mood, group = 1)) +
      geom_line() + geom_point()
    print(p, width = dynamic_width)
  })
  output$plot2 <- renderPlot({
    num_days <- as.numeric(difftime(input$Enddate, input$startdate, units = "days"))
    
    base_width <- 400
    increment_per_day <- 20
    
    dynamic_width <- max(base_width, base_width + num_days * increment_per_day)
    
    p = ggplot(data = filter_date(), mapping = aes(x = Date, y= Mood_total, group = 1)) +
      geom_line() + geom_point()
    print(p, width = dynamic_width)
  })
  
  output$table <- renderTable({
    stats = as.data.frame(summary(table_date()))
    stats[c(1,3,4,6), c(3)]
  })
}



shinyApp(ui = ui, server = server)
