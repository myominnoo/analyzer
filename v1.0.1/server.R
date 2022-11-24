
max_1_item_opts <- sortable_options(
  group = list(
    name = "my_shared_group",
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 1 child already
        return to.el.children.length < 1;
      }
    ")
  )
)


body <- mainPanel(
  
)

vars_select_box <- sidebarLayout(
  sidebarPanel(uiOutput("sortable")), 
  mainPanel(
    fluidRow(
      rank_list("X axis:", 
                labels = c(), 
                input_id = "x", 
                options = max_1_item_opts), 
      rank_list("X axis:", 
                labels = c(), 
                input_id = "x", 
                options = max_1_item_opts), 
      rank_list("X axis:", 
                labels = c(), 
                input_id = "x", 
                options = max_1_item_opts)
    )
  )
)

vars_selector <- bsplus::bs_accordion("vars_selector") %>% 
  bs_set_opts(panel_type = "success") %>% 
  bsplus::bs_append(
    title = "Variable Selector", 
    content = vars_select_box, 
    selected = FALSE
  )

sidebar <- sidebarPanel(
  width = "100%",
  column(12, align = "center", h4("Create Plot")), 
  ## variable selector
  vars_selector
)

main_ui <- fluidPage(
  br(), 
  sidebarLayout(sidebar, body)
)



server <- function(input, output, session) {
  output$main_ui <- renderUI(main_ui)  
  
  df <- reactive(my_data)
  
  observeEvent(df(), {
    output$sortable <- renderUI({
      div(style = "height: 60vh; overflow-y: auto;", 
          rank_list(NULL, names(df()), "sortable",
                    options = sortable_options(group = "my_shared_group")))
    })
  })
}

