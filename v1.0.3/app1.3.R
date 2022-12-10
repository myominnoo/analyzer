
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(phosphoricons)
library(datamods)


source("helpers.R", echo = TRUE)

importUI <- function(id) {
  fileInput(NS(id, "upload"), "Select")
}

importServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$upload) 
      rio::import(input$upload$datapath)
    })
  })
}

renameUI <- function(id) {
  showModal(modalDialog(
    title = "Rename variables", 
    checkboxGroupInput(NS(id, "vars"), "Select"), 
    actionButton(NS(id, "confirm"), "Apply changes", 
                 class = "btn-primary", width = "100%"), 
    footer = NULL, 
    easyClose = TRUE
  ))
}

renameServer <- function(id, data, launch) {
  moduleServer(id, function(input, output, session) {
    observeEvent(launch(), {
      req(data()) 
      df <- data()
      updateCheckboxGroupInput(session, "vars", choices = names(df), inline = TRUE)
    })
    
    updated_data <- reactiveValues(x = NULL)
    observeEvent(input$confirm, {
      df <- data() 
      updated_data$x <- if (is.null(input$vars)) df else subset(df, select = input$vars)
    })
    return(reactive(updated_data$x))
  })
}


ui <- fluidPage(
  importUI("import-data"), 
  actionButton("launch_rename", "Rename variables"), 
  tableOutput("info")
)

server <- function(input, output, session) {
  env <- reactiveValues(data = NULL)
  imported <- importServer("import-data")
  observeEvent(imported(), {
    env$data <- imported()
  })
  output$info <- renderTable({
    head(env$data)
  })
  observeEvent(input$launch_rename, {
    renameUI("rename-vars")
  })
  renamed <- renameServer("rename-vars", reactive(env$data), 
                          reactive(input$launch_rename))
  observeEvent(renamed(), {
    str("changed")
    env$data <- renamed() 
  })
}

shinyApp(ui, server)