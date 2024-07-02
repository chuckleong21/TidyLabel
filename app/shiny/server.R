server = function(input, output, session) {
  
  # IMPORTANT!
  # this is needed to terminate the R process when the!
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$switchLang, {
    update_lang(input$switchLang)
  })
  
  observe({
    if(input$enableValidation) {
      insertTab(inputId = "main", 
                tab = tabPanel(value = "#validate", 
                               validateUI("tab3", i18n = translator), 
                               title = translator$translate("标签对照")),
                target = "#label", position = "after")
    } else {
      removeTab(inputId = "main", target = "#validate")
    }
  })
  
  # inputs shared across modules
  session$userData$lang <- reactive(input$switchLang)
  session$userData$tax$version <- reactive(input$pdfVersion)
  session$userData$label <- tidyLabelServer("tab1", i18n = translator)
  session$userData$box$path <- reactive(input$boxFile$datapath)
  session$userData$box$data <- reactive(cross_tbl(session$userData$box$path(), 
                                                  session$userData$label$path(), 
                                                  session$userData$label$sheet()))
  
  tidyTaxServer("tab2", i18n = translator, version = reactive(input$pdfVersion))
  validateServer("tab3", i18n = translator)
  
  observe({
    output$changelog <- renderUI({
      switch(input$switchLang, 
             "zh" = includeMarkdown("www/changelog-zh.md"),
             "en" = includeMarkdown("www/changelog-en.md")
      )
    })
  })
  
  observe({
    if(input$enableValidation) {
      options(shiny.maxRequestSize = 500 * 1024 ** 2)
    } else {
      options(shiny.maxRequestSize = 5 * 1024 ** 2)
    }
  })
  
  session$onSessionEnded(function() {
    tmpRDS <- paste0(tempdir(), "\\tmp.rds")
    if(file.exists(tmpRDS)) file.remove(tmpRDS)
  })
}
