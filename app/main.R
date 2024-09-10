box::use(
  shiny[fluidPage, titlePanel, icon, img, div, p, a,
        moduleServer, NS, renderUI, tags, uiOutput],
  tippy[tippy]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$link(rel = "stylesheet", href = "static/css/app.min.css"),
    titlePanel(title = "TidyLabel"), 
    uiOutput(ns("header"))
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
  })
}
