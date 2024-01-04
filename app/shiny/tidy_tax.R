tidyTaxUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4, 
        h3("上传"),
        fileInput(inputId = ns("file"), label = "上传税费表格Excel文件", 
                  multiple = FALSE, accept = c(".xls", ".xlsx", ".pdf")),
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

tidyTaxServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    file_upload <- reactive({
      req(input$file$datapath)
      if(!grepl("\\.(pdf)$", input$file$datapath)) {
        sheets <- readxl::excel_sheets(input$file$datapath)
        map(str_which(sheets, "Page"), \(i) read_excel(input$file$datapath, sheet = i))
      } else {
        input$file$name
      }
    })
    
    observe({
        req(input$file)
        if(grepl("\\.(xls|xlsx)$", input$file$datapath)) {
          output$table <- renderUI({
            # req(input$file)
            tagList(
              h3("文件有", strong(length(file_upload())), "个表格"),
              hr(),
              lapply(seq_along(file_upload()), function(x) {
                output[[paste0("table_", x)]] <- renderUI({
                  tagList(h6("表格", x, ":"), br(), renderTable(file_upload()[[x]]))
                })
              })
            )
          })
        } 
        
        if(grepl("\\.(pdf)$", input$file$datapath)) {
          output$table <- renderUI({
            if(!input$file$name %in% list.files("www/")) {
              file.copy(input$file$datapath, paste("www", input$file$name, sep = .Platform$file.sep))
            }
            tags$iframe(
              style = "height:600px; width:100%;", 
              src = gsub("/", "\\\\", input$file$name)
            )
          })
        }
    })
    
    n <- reactive(get_table_pages(paste("www/", isolate(input$file$name), sep = .Platform$file.sep)))
    tidied <- reactive({
      req(input$file)
      tidyup(file = paste("www/", input$file$name, sep = .Platform$file.sep), page = n()) %>%
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
        h3("整理"), 
        hr(), 
        actionButton(inputId = ns("tidyButton"), label = "整理", 
                     icon = icon(name = "gears", class = "fa-solid")),
      )
    })
    
    observeEvent(input$tidyButton, {
      output$filterCheckboxUI <- renderUI({
        req(input$tidyButton)
        conditionalPanel(
          condition = "input.tidyButton != 0", ns = ns, 
          checkboxInput(inputId = ns("summ"), label = "汇总"), 
          checkboxGroupInput(inputId = ns("taxCode"), label = "忽略以下税费编码：", 
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
            tagsTextInput(inputId = ns("header"), label = "重命名表头"),
            fluidRow(
              column(
                width = 4, 
                textInput(inputId = ns("filename"), label = "文件名", placeholder = "导出"), 
              ), 
              column(
                width = 6, 
                br(),
                downloadButton(outputId = ns("export"), label = "导出")
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
        gsub(paste0(id, "-"), "", input$filename())
      } else {
        "导出"
      }
    })
    
    output$export <- downloadHandler(
      filename = function() paste0(export_file_name(), ".xlsx"), 
      content = function(file) openxlsx::write.xlsx(x = tidied_rename(), file = file)
    )
    
    # session is an environment object and treated as reactives
    session$onSessionEnded(function() {
      file.remove(paste("www", isolate(session$input$file$name), sep = .Platform$file.sep))
    })
  })
}