library(shiny)
library(magrittr)
library(DT)
library(Metrics)
library(yaml)

options(stringsAsFactors = FALSE)

LEADERBORD_PATH <- 'data/leaderboard.csv'

config <- read_yaml("config.yaml")
actual <- read.csv("data/actual.csv", header=FALSE)[1]
participants <- read_yaml("data/participants.yaml")
df_leaderboard <- read.csv(LEADERBORD_PATH)

#' Calculate score of registered answers.
calculate_score <- function (submittion) {
  score <- list(
      rmse = rmse(actual, submittion),
      rmsle = rmsle(actual, submittion)
  )
  return(score)
}

update_leaderboard <- function (input) {
  submit_input <- input$submit
  if (is.null(submit_input)) return(df_leaderboard)
  submitter <- input$submitter
  submit_date <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  submittion <- read.csv(submit_input$datapath, header=FALSE)[1]
  score <- calculate_score(submittion)
  df_leaderboard <<- rbind(df_leaderboard, c(list(submitter = submitter, submission_date = submit_date), score))
  write.csv(df_leaderboard, LEADERBORD_PATH, row.names = FALSE)
  return(df_leaderboard)
}

ui <- fluidPage(
  titlePanel(config$app_name),
  fluidRow(
    fileInput(
        "submit",
        "Submit your prediction",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
    ),
    selectInput("submitter", "Submitter", participants)
  ),
  fluidRow(
    DT::dataTableOutput("table")
  )
)

server <- function(input, output) {
  output$table <- DT::renderDataTable({
    input$submit
    table <- isolate(update_leaderboard(input))
    DT::datatable(table)
  })
}


shinyApp(ui, server)
