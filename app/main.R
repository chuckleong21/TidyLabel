box::use(
  shiny[fluidPage, titlePanel, navlistPanel, tabPanel,
        icon, img, div, p, a,
        reactive,
        moduleServer, NS, renderUI, tags, uiOutput],
  tippy[tippy]
)

box::use(
  app/view/tabs
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tidylabel_tab <- tabPanel(
    # conditional panel anchor
    value = "#label", 
    title = "标签整理",
    tabs$ui(ns("label")), 
    
  )
  tidytax_tab <- tabPanel(
    title = "税费整理", 
    tabs$ui(ns("tax"))
  )
  
  fluidPage(
    tags$link(rel = "stylesheet", href = "static/css/app.min.css"),
    titlePanel(title = "TidyLabel"), 
    uiOutput(ns("header")),
    navlistPanel(
      tidylabel_tab,
      tidytax_tab
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$header <- renderUI({
      icon_heart <- icon(name = "heart", class = "fa-solid fa-heart")
      icon_github <- icon(name = "github", class = "fa-github")
      icon_wechat <- icon(name = "weixin", class  = "fa-weixin")
      tags$header(
        div(
          class = "wrapper", 
          div(class = "sign", p("Created with ", icon_heart, " by ")),
          div(
            class = "contact-detail", 
            icon_github, 
            a("@chuckleong21", href = "https://github.com/chuckleong21/TidyLabel")
          ),
          tippy(element = div(class = "contact-detail", icon_wechat, "@chuckleong21"),
                content = "<img src='/qrcode.jpg' width='190' height=259>")
        )
      )
    })
    tabs$server("label")
    tabs$server("tax", version = reactive(input$pdfVersion))
  })
}
