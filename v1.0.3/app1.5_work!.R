
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(phosphoricons)
library(datamods)
library(rhandsontable)

source("helpers.R", echo = TRUE)



# ui - rename -------------------------------------------------------------

rename_ui <- function(id) {
  ns <- NS(id)
  showModal(modalDialog(
    title = modal_btn_close("Rename Variables"),
    tag_place_holder(id, "info", "info", paste0(
      "Add new names to corresponding variables in the table below, ", 
      "then apply changes by clicking the button."
    )), 
    rhandsontable::rHandsontableOutput(ns("rename_table")),
    br(), 
    actionButton(ns("validate"), btn_lbl_icon("arrow-circle-right", "Apply changes"),
                 class = "btn-primary", width = "100%"),
    footer = NULL,
    size = "xl"
  ))
}

rename_server <- function(id, data, launch) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$rename_table <- rhandsontable::renderRHandsontable({
      ## show error message if no dataset is imported
      if (is.null(data())) insert_error("No dataset!", selector = "update")
      req(launch())
      req(data())
      
      rhandsontable(data.frame(old = names(data()), new = ""),
                    colHeaders = c("Variable", "New Name"),
                    stretchH = "all") %>%
        hot_col(1, readOnly = TRUE) %>%
        hot_cols(colWidths = 200)
    })
    
    env <- reactiveValues(updated_data = NULL)
    observeEvent(input$validate, {
      res_update <- try({
        df <- data()
        vars <- rhandsontable::hot_to_r(input$rename_table)
        vars$new <- trimws(vars$new)
        num_changed <- sum(vars$new != "")
        vars$new <- ifelse(vars$new == "", vars$old, vars$new)
        names(df) <- vars$new
      })
      
      if (inherits(res_update, "try-error")) {
        insert_error(selector = "update")
        env$updated_data <- data()
      } else {
        insert_alert(
          selector = ns("update"),
          status = "success",
          tags$b(phosphoricons::ph("check"), "Variables successfully renamed!")
        )
        env$updated_data <- df
      }
    })

    return(reactive(env$updated_data))
  })
}

# ui - main ---------------------------------------------------------------
ui <- fluidPage(
  br(), 
  wellPanel(
    div(actionLink("launch_import", btn_lbl_icon("upload", "Import data"))),
    div(actionLink("launch_rename", btn_lbl_icon("textbox", "Rename variables")))
  ), 
  uiOutput("show_active_dataname")
)


# server - main -----------------------------------------------------------
server <- function(input, output, session) {
  env <- reactiveValues(data = NULL, dataname = NULL)
  

  # server - import data ----------------------------------------------------
  observeEvent(input$launch_import, {
    datamods::import_modal(
      id = "import-data",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data"
    )
  })
  imported <- datamods::import_server("import-data", return_class = "tbl_df")
  observeEvent(imported$data(), {
    env$data <- imported$data() 
    env$dataname <- imported$name()
  })

  # server - show active dataset name ---------------------------------------
  output$show_active_dataname <- renderUI({
    req(env$dataname)
    shinyWidgets::alert(
      status = "success",
      phosphoricons::ph("database"), 
      sprintf("Active dataset: %s", env$dataname)
    )
  })
  

  # server - rename ---------------------------------------------------------
  observeEvent(input$launch_rename, {
    # browser()
    rename_ui("rename-vars")
  })
  renamed <- rename_server("rename-vars", reactive(env$data), 
                           reactive(input$launch_rename))
  observeEvent(renamed(), {
    env$data <- renamed()
    str(renamed())
  })
  
}

shinyApp(ui, server)