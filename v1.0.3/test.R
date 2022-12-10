

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

filterUI <- function(id) {
  tagList(
    checkboxGroupInput(NS(id, "vars"), "Select vars:"), 
    actionButton(NS(id, "confirm"), "OK")    
  )
}

filterServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    observeEvent(df(), {
      df <- df()
      updateCheckboxGroupInput(session, "vars", choices = names(df), 
                               selected = names(df), inline = TRUE)
    })
    
    updated_data <- reactiveValues(x = NULL)
    observeEvent(input$confirm, {
      df <- df()
      if (is.null(input$vars)) {
        updated_data$x <- df
      } else {
        updated_data$x <- subset(df, select = input$vars)
      }
      str(input$vars)
    })
    return(reactive(updated_data$x))
  })
}


library(shiny)

ui <- fluidPage(
  filterUI("vars"), 
  importUI("import"),
  tableOutput("info")
)

server <- function(input, output, session) {
  env <- reactiveValues(data = NULL)
  imported <- importServer("import")
  observeEvent(imported(), env$data <- imported())
  output$info <- renderTable({
    req(env$data)
    env$data
  })
  
  filtered <- filterServer("vars", reactive(env$data))
  observeEvent(filtered(), {
    env$data <- df <- filtered()
    str(env$data)
  })
}

shinyApp(ui, server)




library(shiny)
library(rhandsontable)

ui <- fluidPage(
  rHandsontableOutput("hottable")  
)

server <- function(input, output, session) {
  observe({
    print(hot_to_r(input$hottable))
  })
  
  output$hottable <- renderRHandsontable({
    rhandsontable(mtcars)
  })
}

shinyApp(ui, server)


library(shiny)

df <- data.frame(old = names(mtcars), new = "")

ui <- fluidPage(
  actionButton("ok", "OK"), 
  rHandsontableOutput("hottable")  
)

server <- function(input, output, session) {
  env <- reactiveValues(updated_data = NULL)
  observeEvent(input$ok, {
    env$updated_data <- hot_to_r(input$hottable)
    print(env$updated_data)
  })
  
  output$hottable <- renderRHandsontable({
    rhandsontable(df, stretchH = "all")
  })
}

shinyApp(ui, server)