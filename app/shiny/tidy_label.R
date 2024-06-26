tidyLabelUI <- function(id, i18n) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4, 
        h3(i18n$translate("上传")),
        fileInput(inputId = ns("file"), label = i18n$translate("上传箱单表格Excel文件"), 
                  multiple = FALSE, accept = c(".xls", ".xlsx")),
        uiOutput(ns("selectSheet")),
        uiOutput(ns("uploadSheet")),
        htmlOutput(ns("exportUI")),
        uiOutput(ns("exportButtonUI")),
      ), 
      column(
        width = 8, 
        uiOutput(ns("table"))
      )
    )
  )
}

tidyLabelServer <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    file_upload <- reactive({
      req(input$file$datapath)
      readxl::excel_sheets(input$file$datapath)
    })
    
    output$selectSheet <- renderUI({
      req(input$file$datapath)
      selectInput(inputId = ns("sheet"), label = i18n$translate("选择表格"), choices = file_upload())
    })
    
    output$uploadSheet <- renderUI({
      req(input$file$datapath)
      tagList(
        div(
          style = "display: flex;", 
          actionButton(inputId = ns("uploadButton"), label = i18n$translate("上传表格"),
                       icon = icon(name = "file-arrow-up", class = "fa-file-arrow-up")),
          uiOutput(ns("tidyButton"), style = "margin-left:10px;")
        )
      )
    })
    
    label <- reactive({
      req(input$file$datapath)
      label_data(input$file$datapath, input$sheet)
    })
    
    observeEvent(input$uploadButton, {
      output$table <- renderUI({
        req(input$file)
        renderTable(label())
      })
    })
    
    observeEvent(input$uploadButton, {
      output$tidyButton <- renderUI({
        actionButton(inputId = ns("tidyNow"), label = i18n$translate("整理"), 
                     icon = icon(name = "gears", class = "fa-gears"))
      })
    })
    
    output_table_list <- reactive({
      tagList(
        h3(i18n$translate("共整理"), strong(length(label())), i18n$translate("个表格")),
        hr(),
        lapply(seq_along(label()), function(x) {
          output[[paste0("table_", x)]] <- renderUI({
            tagList(
              h6(i18n$translate("表格"), x, ":"), 
              br(), renderTable(label()[x])
            )
          })
        })
      )
    })
    
    observeEvent(input$tidyNow, {
      output$table <- renderUI({
        output_table_list()
      })
      
      output$exportUI <- renderUI({
        previewDoc_cond <- "input.formatBox != null & input.exportFileType == '.docx' & input.formatBox == true"
        tagList(
          hr(), 
          h3(i18n$translate("导出选项")), 
          div(
            fluidRow(
              column(
                width = 4, 
                textInput(inputId = ns("filename"), label = i18n$translate("文件名"), placeholder = i18n$translate("导出"))
              ),
              column(
                width = 6, 
                radioButtons(inputId = ns("exportFileType"), label = paste0(i18n$translate("导出为"), ":"), 
                             choices = c(".docx", ".xlsx"), selected = ".docx", inline = TRUE)
              ), 
            ), 
            fluidRow(
              div(
                conditionalPanel(
                  condition = "input.exportFileType == '.docx'", ns = ns, style = "width:110px;",
                  checkboxInput(inputId = ns("formatBox"), label = i18n$translate("套用百世格式"), value = TRUE)
                ),
                conditionalPanel(
                  condition = previewDoc_cond, ns = ns, style = "margin-left:10px;",
                  checkboxInput(inputId = ns("previewDoc"), label = i18n$translate("预览")), 
                ),
                style = "margin-left:15px;display:flex;"
              )
            )
          )
        )
      })
    })
    
    observeEvent(input$tidyNow, {
      output$exportButtonUI <- renderUI({
        downloadButton(ns("export"), i18n$translate("导出"))
      })
    })
    
    export_document <- reactive({
      if(input$exportFileType == ".xlsx") {
        wb <- openxlsx::createWorkbook()
        
        for(i in seq_along(label())) {
          openxlsx::addWorksheet(wb = wb, sheetName = paste("Sheet", i))
          openxlsx::writeData(wb = wb, x = label()[i], sheet = i)
        }
        return(wb)
      }
      
      if(input$exportFileType == ".docx") {
        # select every two columns before converting into themed flextables and conserve
        tidy_flextbl <- imap(label(), \(x, y) {
          x %>% 
            flextable(cwidth = 3) %>% 
            set_table_properties(layout = "autofit") %>%
            set_header_labels(values = c(y, "")) %>% 
            theme_box
        })
        # inititate a docx
        doc <- read_docx()
        # do the following: 
        # 1. add a table into a page, 
        # 2. add a page break after
        # 3. if it is the last page, skip adding a page break
        progress <- shiny::Progress$new()
        progress$set(message = paste0(i18n$translate("汇编至Word"), "："), value = 0)
        iwalk(rev(tidy_flextbl), \(x, y) {
          progress$set(value = y / length(label()), detail = sprintf("%g%%", round(y / length(label()), 2) * 100))
          
          if(y != 1) {
            doc %>% 
              body_add_flextable(x) %>% 
              body_add_break()
          } else {
            doc %>% 
              body_add_flextable(x)
          }
        })
        
        if(!input$formatBox) {
          return(doc)
          progress$close()
        } else {
          # if baaksai format is checked
          print(doc, target = paste0(tempdir(), "\\tmp.docx"))
          progress$close()
          
          format_baaksai(word_file = paste0(tempdir(), "\\tmp.docx"), 
                         excel_file = input$file$datapath, 
                         excel_sheet = input$sheet, 
                         progress_bar = TRUE, 
                         preview = input$previewDoc, 
                         i18n = translator)
        }
      }
    })
    
    preview_doc <- reactive({
      if(input$formatBox && input$previewDoc) {
        if(!file.exists("www/tmp.pdf")) {
          export_document(); Sys.sleep(1.5)
        }
        
        while(!file.exists("www/tmp.pdf")) Sys.sleep(0.5)
        tags$iframe(
          style = "height:600px; width:100%;", 
          src = "tmp.pdf"
        )
      } else output_table_list()
    })
    
    observe({
      req(input$previewDoc)
      output$table <- renderUI(preview_doc())
    })
    
    export_file_name <- reactive({
      if(nchar(input$filename) != 0) {
        gsub(paste0(id, "-"), "", input$filename)
      } else {
        i18n$translate("导出")
      }
    })
    
    output$export <- downloadHandler(
      filename = function () {
        file_type <- ifelse(input$exportFileType == ".docx", ".docx", ".xlsx")
        paste0(export_file_name(), file_type)
      }, 
      content = function(file) {
        if(input$exportFileType == ".docx") {
          if(!input$formatBox) {
            print(export_document(), target = file)
          } else {
            export_document()
            file.copy(paste0(tempdir(), "\\tmp.docx"), file)
          }
        }
        if(input$exportFileType == ".xlsx") openxlsx::saveWorkbook(wb = export_document(), file = file)
      }
    )
    
    return(
      list(path = reactive(input$file$datapath), sheet = reactive(input$sheet), data = label)
    )
    
    session$onSessionEnded(function() {
      label <- paste("www", "tmp.pdf", sep = .Platform$file.sep)
      if(file.exists(label)) file.remove(label)
    })
  })
}