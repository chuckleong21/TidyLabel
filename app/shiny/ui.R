ui <-   fluidPage(
  useShinyjs(),
  usei18n(translator),
  # js script for testing
  # tags$head(
  #   tags$script(
  #     "
  #     $(document).on('shiny:value', function(event) {
  #       console.log('[output] ' + event.name + ': ' + event.value);
  #     });
  #   "
  #   )
  # ),
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

.opt-container span {
  margin-right:7px
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
    id = "main",
    widths = c(3, 9),
    # tidy_label
    tabPanel(value = "#label", tidyLabelUI("tab1", i18n = translator), title = translator$translate("标签整理")),
    # tidy_tax
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
              span(style = "padding-top:5px", translator$translate("应用语言")),
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
                            choices = version_list, 
                            selected = "auto")
              )
            ),
            div(
              class = "opt-container",
              span(translator$translate("开启对照")), 
              div(
                shinyWidgets::materialSwitch(
                  inputId = "enableValidation", 
                  value = FALSE,
                  status = "primary"
                )
              )
            ), 
            conditionalPanel(
              condition = "input.enableValidation == true", 
              tagList(
                div(
                  class = "opt-container", 
                  span(style = "padding-top:25px", translator$translate("箱单文件")), 
                  div(fileInput("boxFile", label = "", accept = ".xlsx"))
                )
              )
            )
          )
        )
      )
    )
  ) 
)
