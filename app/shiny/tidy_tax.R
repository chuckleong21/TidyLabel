tidyTaxUI <- function(id, i18n) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4, 
        h3(i18n$translate("上传")),
        div(
          style = "display:flex",
          fileInput(inputId = ns("file"), label = i18n$translate("上传税费PDF文件"), 
                    multiple = FALSE, accept = ".pdf"),
          span(
            style = "margin-top:25px;",
            actionButton(ns("pdfSuccess"), label = "", 
                         icon = icon("circle-check", 
                                     class = "fa-solid", style = "color:#198754"), style = "border:none"), 
            actionButton(ns("pdfWarning"), label = "", 
                         icon = icon("circle-exclamation", 
                                     class = "fa-solid", style = "color:#ffc107"), style = "border:none"),
            actionButton(ns("pdfError"), label = "", 
                         icon = icon("circle-xmark", 
                                     class = "fa-solid", style = "color:#dc3545"), style = "border:none")
          )
        ),
        htmlOutput(ns("tidyUI")),
        htmlOutput(ns("filterCheckboxUI")),
        htmlOutput(ns("exportUI")),
      ), 
      column(
        width = 8, 
        uiOutput(ns("table"))
      )
    )
  )
}

tidyTaxServer <- function(id, i18n, version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
      
    pdf_status <- reactive({
      
      if(is.null(input$file)) return(NULL)
      from <- get_pdf_version(input$file$datapath)$version
      
      if(version() == "auto") {
        ifelse(from %in% version_list, "success", "error")
      } else {
        ifelse(from %in% version_list, ifelse(from == version(), "success", "warning"), "error")
      }
    })
    
    pdf_status_modal <- function() {
      version <- reactive(get_pdf_version(input$file$datapath)$version)
      preset <- session$userData$tax$version() 
      status <- pdf_status()
      modalDialog(
        title = ifelse(preset == "auto", "自动检测结果", "版本检测结果"),
        footer = tagList(
          modalButton("OK")
        ), 
        tagList(
            switch(status, 
                   "success" = span("匹配", style = "color:#198754"),
                   "warning" = span("不匹配", style = "color:#ffc107"), 
                   "error" = span("缺失", style = "color:dc3545")),
            br(),
            span(paste0("检测到版本", ":" , version())),
            if(preset != "auto") div(br(), span(paste0("设置版本", ":", preset))),
            if(status == "error" && preset == "auto") div(br(), span(paste0("现有版本", ":", version_list[-1]))),
            if(status != "success") div(br(), span(paste0("版本不匹配", "/", "版本缺失", "将导致结果出错")))
        )
      )
    }
    
    observe({
      if(is.null(pdf_status())) {
        hide(id = "pdfSuccess")
        hide(id = "pdfWarning")
        hide(id = "pdfError")
      } else {
        toggle(id = "pdfSuccess", condition = pdf_status() == "success")
        toggle(id = "pdfWarning", condition = pdf_status() == "warning")
        toggle(id = "pdfError", condition = pdf_status() == "error")
      }
    })
    
    observeEvent(input$pdfSuccess, {
      showModal(
        pdf_status_modal()
      )
    })
    
    observeEvent(input$pdfWarning, showModal(pdf_status_modal()))
    observeEvent(input$pdfError, showModal(pdf_status_modal()))
    
    observe({
      req(input$file)
      
      if(grepl("\\.(pdf)$", input$file$datapath)) {
        output$table <- renderUI({
          if(!input$file$name %in% list.files("www/")) {
            file.copy(input$file$datapath, paste("www", input$file$name, sep = .Platform$file.sep))
          }
          tags$iframe(
            style = "height:600px; width:100%;", 
            src = input$file$name
          )
        })
      }
    })
    
    n <- reactive(get_n_pages(input$file$datapath))
    tidied <- eventReactive(input$tidyButton, {
      req(version())
      progress <- shiny::Progress$new()
      progress$set(message = paste0(i18n$translate("整理数据"), "："), value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 4
        }
        progress$set(value = value, detail = detail)
      }
      
      tidyup(file = input$file$datapath, page = do.call(seq, as.list(input$pagination)),
             version = ifelse(version() == "auto", get_pdf_version(input$file$datapath)$version, version()), 
             updateProgress = updateProgress) %>%
        rename(
          "Товар" = id,
          "Код товара" = hs_code,   
          "Вес брутто" = weight,
          "Вид" = tax_code,
          "Сумма" = tax,
        )
    })
    
    output$tidyUI <- renderUI({
      req(input$file)
      tagList(
        h3(i18n$translate("整理参数")), 
        hr(), 
        sliderInput(inputId = ns("pagination"), label = i18n$translate("页面范围"), 
                    min = 1, max = n(), value = c(1, n()), step = 1),
        actionButton(inputId = ns("tidyButton"), label = i18n$translate("整理"), 
                     icon = icon(name = "gears", class = "fa-solid")),
      )
    })
    
    observeEvent(input$tidyButton, {
      output$filterCheckboxUI <- renderUI({
        req(input$tidyButton)
        conditionalPanel(
          condition = "input.tidyButton != 0", ns = ns, 
          checkboxInput(inputId = ns("summ"), label = i18n$translate("汇总")), 
          checkboxGroupInput(inputId = ns("taxCode"), label = paste0(i18n$translate("忽略以下税费编码"), "："), 
                             choices = unique(tidied()[["Вид"]]), inline = TRUE), 
        )
      })
    })
    
    observeEvent(input$tidyButton, {
      output$table <- renderTable(tidied())
    })
    
    tidied_filter <- reactive({
      base_summary <- function(x) {
        x %>%
          add_count(Товар, wt = Сумма, name = "Сумма") %>%
          select(-Вид) %>%
          distinct()
      }
      
      if(is.null(input$summ)) return()
      if(!input$summ) {
        if(is.null(input$taxCode)) {
          tidied()
        } else {
          filter(tidied(), !Вид %in% as.vector(input$taxCode))
        }
      } else if(is.null(input$taxCode)) {
        base_summary(tidied())
      } else if(!is.null(input$taxCode)) {
        filter(tidied(), !Вид %in% as.vector(input$taxCode)) %>% base_summary()
      }
    })
    
    
    observe({
      req(input$tidyButton)
      output$table <- renderTable(tidied_filter())
    })
    
    observeEvent(input$tidyButton, {
      output$exportUI <- renderUI({
        tagList(
          h3(i18n$translate("导出")), 
          hr(),
          div(
            tagsTextInput(inputId = ns("header"), label = i18n$translate("重命名表头")),
            fluidRow(
              column(
                width = 4, 
                textInput(inputId = ns("filename"), 
                          label = i18n$translate("文件名"), 
                          placeholder = i18n$translate("导出")), 
              ), 
              column(
                width = 6, 
                br(),
                downloadButton(outputId = ns("export"), label = i18n$translate("导出"))
              )
            )
          )
        )
      })
    })
    
    tidied_rename <- reactive({
      header_sub(tidied_filter(), input$header)
    })
    
    observe({
      req(input$header)
      output$table <- renderTable(tidied_rename())
    })
    
    export_file_name <- reactive({
      if(nchar(input$filename) != 0) {
        gsub(paste0(id, "-"), "", input$filename)
      } else {
        i18n$translate("导出")
      }
    })
    
    output$export <- downloadHandler(
      filename = function() paste0(export_file_name(), ".xlsx"), 
      content = function(file) {
        openxlsx2::write_xlsx(x = tidied_rename(), file = file)
      }
    )
    
    # session is an environment object and treated as reactives
    session$onSessionEnded(function() {
      tax <- paste("www", isolate(session$input$file$name), sep = .Platform$file.sep)
      if(file.exists(tax)) file.remove(tax)
    })
  })
}