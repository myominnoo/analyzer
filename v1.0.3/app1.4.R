
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
    rhandsontable::rHandsontableOutput(NS(id, "rename_table")), 
    actionButton(NS(id, "confirm"), "Apply changes", 
                 class = "btn-primary", width = "100%"), 
    footer = NULL, 
    easyClose = TRUE
  ))
}

renameServer <- function(id, data, launch) {
  moduleServer(id, function(input, output, session) {
    env <- reactiveValues(data = NULL, updated_data = NULL, token = NULL)
    
    observeEvent(launch(), {
      req(data()) 
      env$data <- data()
      env$token <- genId()
      # str(names(env$data))
      str(env$token)
      vars_names <- data.frame(old = names(env$data), new = NA)
      vars_names$new <- set_as_text_input(
        names(env$data), ns(paste("var-name", env$token, sep = "-"))
      )
      output$rename_table <- reactable::renderReactable({
        show_table(vars_names, columns = list(
          old = reactable::colDef(name = "Variable"),
          new = reactable::colDef(name = "New Name")
        ))
      })
    })
    
    observeEvent(input$confirm, {
      str(env$token)
      res_update <- try({
        get_inputs(paste("var-name", env$token, sep = "-")) %>%  str
        lapply(paste("#var-name", env$token, 1:ncol(env$data), sep = "-"), removeUI)
        removeUI(selector = paste("var-name", env$token, 1, sep = "-"))
        # removeUI(
        #   selector = 
        # )
        reactiveValuesToList(session$input) %>% str
        # new_names <- unlist(get_inputs(paste("var-name", env$token, sep = "-")))
        # index <- gsub(paste0("var-name-", env$token, "-"), "", names(new_names))
        # new_names <- data.frame(index = as.numeric(index), new = new_names)
        # new_names <- new_names[order(new_names$index), ]
        # names(env$data) <- ifelse(trimws(new_names$new) == "",
        #                       names(env$data), new_names$new)
        # names(env$data)[1] <- "date"
      }, silent = TRUE)

      if (inherits(res_update, "try-error")) {
        env$update_data <- data()
      } else {
        env$update_data <- env$data
        removeModal()
      }
    })
    return(reactive(env$update_data))
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