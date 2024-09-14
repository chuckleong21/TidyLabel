box::use(
  shiny[NS, moduleServer, 
        uiOutput, renderUI, renderTable,
        tags]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(id)
}

#' @export
server <- function(id, pdf = FALSE, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    x <- list(...)
    
    if(id == "app-label-table") {
      invisible()
    }
    
    if(id == "app-tax-table") {
      if(!pdf) {
        # stopifnot('Check whether "table" argument present' = "table" %in% names(x))
        output[[id]] <- renderTable(x$x())
      } else {
        # save a copy in local directory as an anchor point
        tmp <- paste("app", "static", x$name(), sep = .Platform$file.sep)
        file.copy(x$path(), tmp)
        
        output[[id]] <- renderUI({
          tags$iframe(style = "height:600px; width:100%", src = tmp)
        })
      }
    }
  })
}