server = function(input, output, session) {
  
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$switchLang, {
    update_lang(input$switchLang)
  })
  
  tidyLabelServer("tab1", i18n = translator)
  tidyTaxServer("tab2", i18n = translator)
  
  observe({
    output$changelog <- renderUI({
      switch(input$switchLang, 
             "zh" = includeMarkdown("www/changelog-zh.md"),
             "en" = includeMarkdown("www/changelog-en.md")
      )
    })
  })
}
