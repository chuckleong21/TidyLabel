
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @noRd
tagsInputDependencies <- function(){
  version <- "0.1.0"
  src <- normalizePath("./www")
  dep <- htmlDependency(
    name = "tagsinput",
    version = version,
    src = src,
    script = "bootstrap-tagsinput.js",
    stylesheet = "bootstrap-tagsinput.css"
    )
  list( dep )
}


#' text input specific to tags
#'
#' @param ... see \code{\link[shiny]{textInput}}
#'
#' @importFrom shiny textInput
#' @importFrom htmltools tagAppendAttributes
#' @export
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'   ui <- fluidPage(
#'     tagsTextInput("fruits", "Fruits", "apple, banana"),
#'     textOutput("out")
#'   )
#'
#'   server <- function(input, output){
#'     output$out <- renderPrint( strsplit( input$fruits, ",")[[1]] )
#'   }
#'
#'   shinyApp( ui, server )
#'
#' }
tagsTextInput <- function(...) {
  # value <- restoreInput(id = inputId, default = value)
  # res <- div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), 
  #     shiny:::shinyInputLabel(inputId, label), 
  #     if(!is.null(description)) {
  #       tags$p(description)
  #     },
  #     tags$input(id = inputId, 
  #                type = "text", class = "shiny-input-text form-control", 
  #                value = value, placeholder = placeholder))
  res <- textInput(...)
  res$children[[2]] <- tagAppendAttributes( res$children[[2]], `data-role` = "tagsinput" )
  attr(res, "html_dependencies") <- tagsInputDependencies()
  res
}
