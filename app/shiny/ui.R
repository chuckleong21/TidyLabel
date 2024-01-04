ui <-   fluidPage(
  tags$style(".fa-heart {color:#FF6347;} .fa-weixin {color:#7BB32E} header span {font-size:15px}"),
  titlePanel(title = "TidyLabel"), 
  tags$header(
    span("Created With ", icon(name = "heart", class = "fa-solid fa-heart"), " by "),
    span(
      icon(name = "github", class = "fa-github"), 
      a("@chuckleong21", href = "https://github.com/chuckleong21")
    ), 
    span(
      tippy(element = span(icon(name = "weixin", class = "fa-weixin"), "@chuckleong21"), 
            content = "<img src='/qrcode.jpg' width='190' height=259>") 
    )
  ), 
  navlistPanel(
    widths = c(3, 9),
    # tabPanel1
    tabPanel(tidyLabelUI("tab1"), title = "标签整理"),
    # tabPanel2
    tabPanel(tidyTaxUI("tab2"), title = "税费整理"),
    # help tab
    tabPanel(
      title = "整理帮助",
      tagList(
        h1("标签整理"), 
        img(src = "/1.png", width = 1200),
        img(src = "/2.png", width = 1200),
        img(src = "/3.png", width = 1200),
        h1("税费整理"),
        img(src = "/4.png", width = 1200),
        img(src = "/5.png", width = 1200)
      )
    )
  ) 
)
