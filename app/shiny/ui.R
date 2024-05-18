ui <-   fluidPage(
  usei18n(translator),
  tags$style(
    "
    .fa-heart {color:#FF6347;} 
    .fa-weixin {color:#7BB32E;} 
    header span {font-size:15px;}
.wrapper {
  display: grid;
  grid-template-columns: 130px 130px 130px;
  grid-template-rows: repeat(2, 35px);
  grid-auto-rows:auto;
}

.sign {
  grid-row: 1 / span 2;
  display: flex;
  justify-content: start;
  align-items: center;
}

.contact-detail {
  display: flex;
  justify-content: start;
  align-items: center;
}

.switch-lang {
  grid-row: 1 / span 2;
  grid-column: 3 / 3;
  display:flex;
  justify-content:right;
  align-items: center;
}

.opt-container {
  display:flex;
}
    "
  ),
  titlePanel(title = "TidyLabel"), 
  tags$header(
    div(
      class = "wrapper", 
      div(class = "sign", p("Created With ", icon(name = "heart", class = "fa-solid fa-heart"), " by ")),
      div(
        class = "contact-detail",
        icon(name = "github", class = "fa-github"), 
        a("@chuckleong21", href = "https://github.com/chuckleong21")
      ),
      tippy(element = div(class = "contact-detail", icon(name = "weixin", class = "fa-weixin"), "@chuckleong21"), 
            content = "<img src='/qrcode.jpg' width='190' height=259>"), 
      # div(
      #   class = "switch-lang",
      #   selectInput("switchLang", 
      #               label = NULL, 
      #               choices = setNames(translator$get_languages(), c("中文", "English")), 
      #               selected = translator$get_key_translation()) 
      # )
    ) 
  ), 
  navlistPanel(
    widths = c(3, 9),
    # tabPanel1
    tabPanel(tidyLabelUI("tab1", i18n = translator), title = translator$translate("标签整理")),
    # tabPanel2
    tabPanel(tidyTaxUI("tab2", i18n = translator), title = translator$translate("税费整理")),
    # changelog tab
    tabPanel(
      title = translator$translate("版本更新"),
      htmlOutput("changelog")
    ),
    tabPanel(
      title = translator$translate("应用设置"),
      tagList(
        fluidRow(
          column(width = 1), 
          column(
            width = 8,
            div(
              class = "opt-container", 
              span(translator$translate("应用语言")),
              div(
                class = "switch-lang",
                selectInput("switchLang", 
                            label = NULL, 
                            choices = setNames(translator$get_languages(), c("中文", "English")), 
                            selected = translator$get_key_translation()) 
              )
            ),
            div(
              class = "opt-container", 
              span(translator$translate("税费版本")),
              div(
                selectInput("pdfVersion", 
                            label = NULL, 
                            choices = setNames(1:2, c("2023-12", "2024-05")), 
                            selected = 2)
              )
            )
          )
        )
      )
    )
  ) 
)
