
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(phosphoricons)
library(datamods)


# ui - main ---------------------------------------------------------------
ui <- fluidPage(
  br(), 
  wellPanel(
    actionLink("launch_import", phosphoricons::ph("upload", height = "2em", title = "Import data")), 
    actionLink("launch_rename", phosphoricons::ph("textbox", height = "2em", title = "Rename variables"))
  ), 
  uiOutput("show_active_dataname")
)


# server - main -----------------------------------------------------------
server <- function(input, output, session) {
  env <- reactiveValues(data = NULL, dataname = NULL)
  
  observeEvent(input$launch_import, {
    datamods::import_modal(
      id = "import_data",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data"
    )
  })
  
  imported <- datamods::import_server("import_data", return_class = "tbl_df")
  observe({
    req(imported$data())
    env$data <- imported$data()
    env$dataname <- imported$name()
  })
  
  output$show_active_dataname <- renderUI({
    req(env$dataname)
    shinyWidgets::alert(
      status = "success",
      phosphoricons::ph("database"), 
      sprintf("Active dataset: %s", env$dataname)
    )
  })
  
  
}

shinyApp(ui, server)