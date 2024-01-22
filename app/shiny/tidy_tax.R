tidyTaxUI <- function(id, i18n) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4, 
        h3(i18n$translate("上传")),
        fileInput(inputId = ns("file"), label = i18n$translate("上传税费PDF文件"), 
                  multiple = FALSE, accept = ".pdf"),
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

tidyTaxServer <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
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
    
    n <- eventReactive(input$tidyButton, {
      # implement Progress
      progress <- shiny::Progress$new()
      progress$set(message = paste0(i18n$translate("识别表格"), "："), value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 4
        }
        progress$set(value = value, detail = detail)
      }
      get_table_pages(
        file = paste("www/", isolate(input$file$name), sep = .Platform$file.sep), 
        updateProgress = updateProgress)
    })
    tidied <- eventReactive(input$tidyButton, {
      # req(input$file)
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
      
      tidyup(file = input$file$datapath, page = n(), updateProgress = updateProgress) %>%
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
        h3(i18n$translate("整理")), 
        hr(), 
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
          h3("导出"), 
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
        openxlsx::write.xlsx(x = tidied_rename(), file = file)
      }
    )
    
    # session is an environment object and treated as reactives
    session$onSessionEnded(function() {
      tax <- paste("www", isolate(session$input$file$name), sep = .Platform$file.sep)
      if(file.exists(tax)) file.remove(tax)
    })
  })
}