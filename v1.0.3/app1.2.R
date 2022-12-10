
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(phosphoricons)
library(datamods)


# helpers -----------------------------------------------------------------

pad0 <- function(x) {
  NAs <- which(is.na(x))
  x <- formatC(x, width = max(nchar(as.character(x)), na.rm = TRUE), flag = "0")
  x[NAs] <- NA
  x
}

set_as_text_input <- function(vars_names, id = "variable", width = "100%") {
  mapply(
    FUN = function(inputId, value) {
      input <- textInput(
        inputId = inputId,
        label = NULL,
        value = value,
        width = width
      )
      input <- htmltools::tagAppendAttributes(input, style = "margin-bottom: 0px;")
      htmltools::doRenderTags(input)
    },
    inputId = paste(id, (seq_along(vars_names)), sep = "-"),
    value = "",
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}
get_inputs <- function(pattern, session = shiny::getDefaultReactiveDomain()) {
  all_inputs <- isolate(reactiveValuesToList(session$input))
  filtered <- sort(names(all_inputs))
  filtered <- grep(pattern = pattern, x = filtered, value = TRUE)
  all_inputs[filtered]
}


# icon helper for actionLink() with icon and label side by side
button_lbl <- function(icon = "upload", title = "Import data", label = title) {
  tagList(phosphoricons::ph(icon, title = title), label)
}

# custom reactable 
show_table <- function(data, columns = NULL) {
  tbl <- reactable::reactable(
    data = data, 
    defaultColDef = reactable::colDef(html = TRUE), 
    columns = columns, 
    height = "400px", 
    pagination = FALSE,
    bordered = TRUE,
    compact = TRUE,
    striped = TRUE,
    wrap = FALSE
  )
  htmlwidgets::onRender(tbl, jsCode = "function() {Shiny.bindAll();}")
}

insert_error <- function(mssg = i18n("Something went wrong..."),
                         selector = "import",
                         session = shiny::getDefaultReactiveDomain()) {
  insert_alert(
    selector = session$ns(selector),
    status = "danger",
    tags$b(phosphoricons::ph("warning"), i18n("Ooops")),
    mssg
  )
}

insert_alert <- function(selector, ...) {
  removeUI(selector = paste0("#", selector, "-result"))
  insertUI(
    selector = paste0("#", selector, "-placeholder"),
    ui = alert(
      id = paste0(selector, "-result"),
      ...
    )
  )
}

modalButtonClose <- function(title = "Rename Variables") {
  tagList(
    tags$button(
      phosphoricons::ph("x", title = "Close", height = "2em"),
      class = "btn btn-link",
      style = htmltools::css(border = "0 none", position = "absolute",
                             top = "5px", right = "5px"),
      `data-dismiss` = "modal",
      `data-bs-dismiss` = "modal",
      `aria-label` = "Close"
    ),
    title
  )
}


# ui - rename -------------------------------------------------------------

rename_ui <- function(id) {
  ns <- NS(id)
  showModal(modalDialog(
    # title = "Rename variables", 
    title = modalButtonClose("Rename Variables"), 
    tags$div(
      id = ns("update-placeholder"),
      alert(
        id = ns("update-result"),
        status = "info",
        phosphoricons::ph("info"),
        paste("Add new names to corresponding variables below,",
                   "then apply changes by clicking the button below.")
      )
    ),
    reactable::reactableOutput(ns("rename_table")), 
    actionButton(ns("rename"), "Apply changes", 
                 class = "btn-primary", width = "100%"), 
    footer = NULL
  ))
}

rename_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    updated_data <- reactiveValues(x = NULL)
    
    output$rename_table <- reactable::renderReactable({
      data <- data()
      if (is.null(data)) 
        insert_error("No dataset!", selector = "update")
      req(data)
      vars_names <- data.frame(old = names(data), new = NA)
      vars_names$new <- set_as_text_input(vars_names$old, ns("var-name"))
      show_table(vars_names, columns = list(
        old = reactable::colDef(name = "Variable"),
        new = reactable::colDef(name = "New Name")
      ))
    })
    
    observeEvent(input$rename, {
      data <- data()
      str(input$rename)
      req(data)
      res_update <- try({
        new_names <- unlist(get_inputs("var-name")) 
        index <- gsub("var-name-", "", names(new_names))
        new_names <- data.frame(index = as.numeric(index), new = new_names)
        new_names <- new_names[order(new_names$index), ]
        names(data) <- ifelse(trimws(new_names$new) == "", 
                                  names(data), new_names$new) 
      }, silent = TRUE)
      
      if (inherits(res_update, "try-error")) {
        insert_error(selector = "update")
      } else {
        insert_alert(
          selector = ns("update"),
          status = "success",
          tags$b(phosphoricons::ph("check"), "Variables successfully renamed!")
        )
        updated_data$x <- data
      }
    })
    
    return(reactive(updated_data$x))
  })
}



# ui - main ---------------------------------------------------------------
ui <- fluidPage(
  br(), 
  wellPanel(
    actionLink("launch_import", button_lbl("upload", "Import data")),
    br(), 
    actionLink("launch_rename", button_lbl("textbox", "Rename variables")) 
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
  observeEvent(imported$data(), {
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
  
  observeEvent(input$launch_rename, {
    str(env$data)
    str(env$dataname)
    rename_ui("rename_vars")
  })
  d <- rename_server("rename_vars", reactive(env$data))
  observeEvent(d(), {
    env$data <- d()
  })
}

shinyApp(ui, server)