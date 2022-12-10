
# helpers -----------------------------------------------------------------

## label with icon for action buttons
btn_lbl_icon <- function(icon = "upload", title = "Import data", label = title) {
  tagList(phosphoricons::ph(name = icon, title = title), label)
}

## custom close button for modal dialog
modal_btn_close <- function(title = "Rename Variables") {
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


## place holder for alert messages 
tag_place_holder <- function(id, status = "info", icon = status, 
                             msg = "Message") {
  ns <- NS(id)
  tags$div(
    id = ns("update-placeholder"),
    alert(id = ns("update-result"), status = status,
          phosphoricons::ph(icon), msg
    )
  )
}

## alert messages for placeholder
insert_error <- function(mssg = "Something went wrong...",
                         selector = "import", status = "danger", 
                         session = shiny::getDefaultReactiveDomain()) {
  insert_alert(
    selector = session$ns(selector),
    status = status,
    tags$b(phosphoricons::ph("warning"), "Ooops"),
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

