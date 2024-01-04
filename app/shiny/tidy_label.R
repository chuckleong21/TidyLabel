tidyLabelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4, 
        h3("上传"),
        fileInput(inputId = ns("file"), label = "上传箱单表格Excel文件", 
                  multiple = FALSE, accept = c(".xls", ".xlsx")),
        uiOutput(ns("selectSheet")),
        uiOutput(ns("uploadSheet")),
        htmlOutput(ns("tidyUI")),
        uiOutput(ns("tidyButton")),
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

tidyLabelServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    file_upload <- reactive({
      req(input$file$datapath)
      sheets <- readxl::excel_sheets(input$file$datapath)
      map(seq_along(sheets), \(i) readxl::read_excel(input$file$datapath, sheet = i)) %>% 
        setNames(nm = sheets)
    })
    
    output$selectSheet <- renderUI({
      req(input$file$datapath)
      selectInput(inputId = ns("sheet"), label = "选择表格", choices = names(file_upload()))
    })
    
    output$uploadSheet <- renderUI({
      req(input$file$datapath)
      actionButton(inputId = ns("uploadButton"), label = "上传表格",
                   icon = icon(name = "file-arrow-up", class = "fa-file-arrow-up"))
    })
    
    observeEvent(input$uploadButton, {
      output$table <- renderUI({
        req(input$file)
        renderTable(file_upload()[input$sheet])
      })
    })
    
    observeEvent(input$uploadButton, {
      output$tidyUI <- renderUI(
        tagList(hr(), h3("整理"))
      )
      
      output$tidyButton <- renderUI({
        actionButton(inputId = ns("tidyNow"), label = "整理", 
                     icon = icon(name = "gears", class = "fa-gears"))
      })
    })
    
    tidytbl <- reactive({
      input$tidyNow
      x <- file_upload()[[input$sheet]]
      purrr::map(seq(0, ncol(x) - 2, 2), \(i) x[, 1:2 + i])
    })
    
    observeEvent(input$tidyNow, {
      output$table <- renderUI({
        tagList(
          h3("共整理", strong(length(tidytbl())), "个表格"),
          hr(),
          lapply(seq_along(tidytbl()), function(x) {
            output[[paste0("table_", x)]] <- renderUI({
              tagList(h6("表格", x, ":"), br(), renderTable(tidytbl()[x]))
            })
          })
        )
      })
      
      output$exportUI <- renderUI({
        tagList(
          hr(), 
          h3("导出"), 
          div(
            fluidRow(
              column(
                width = 4, 
                textInput(inputId = ns("filename"), label = "文件名", placeholder = "导出")
              ),
              column(
                width = 6, 
                radioButtons(inputId = ns("exportFileType"), label = "导出为:", 
                             choices = c(".docx", ".xlsx"), selected = ".docx", inline = TRUE)
              ), 
            ), 
            fluidRow(
              conditionalPanel(
                condition = "input.exportFileType == '.docx'", ns = ns,
                checkboxInput(inputId = ns("formatBox"), label = "套用百世格式", value = TRUE)
              ),
            )
          )
        )
      })
    })
    
    observeEvent(input$tidyNow, {
      output$exportButtonUI <- renderUI({
        downloadButton(ns("export"), "导出")
      })
    })
    
    output$formatBox <- renderUI({
      input$exportUI
      checkboxInput(inputId = "formatBoxValue", label = "套用百世格式", value = TRUE)
    })
    
    export_document <- reactive({
      if(input$exportFileType == ".xlsx") {
        wb <- openxlsx::createWorkbook()
        
        for(i in seq_along(tidytbl())) {
          openxlsx::addWorksheet(wb = wb, sheetName = paste("Sheet", i))
          openxlsx::writeData(wb = wb, x = tidytbl()[i], sheet = i)
        }
        return(wb)
      }
      
      if(input$exportFileType == ".docx") {
        # select every two columns before converting into themed flextables and conserve
        tidy_flextbl <- imap(tidytbl(), \(x, y) {
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
        iwalk(rev(tidy_flextbl), \(x, y) {
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
        } else {
          # if baaksai format is checked
          print(doc, target = paste0(tempdir(), "\\tmp.docx")) %>% invisible()
          format_baaksai()
        }
      }
    })
    
    export_file_name <- reactive({
      if(nchar(input$filename) != 0) {
        gsub(paste0(id, "-"), "", input$filename)
      } else {
        "导出"
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
  })
}