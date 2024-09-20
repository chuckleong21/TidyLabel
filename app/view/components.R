box::use(
  shiny[NS,
        fileInput, sliderInput, actionButton,
        checkboxInput, checkboxGroupInput,
        textInput, downloadButton, downloadHandler,
        renderUI, renderTable, conditionalPanel,
        tags, tagList, h3, hr, icon, div, column, br,
        fluidRow,
        req, reactive, observe],
  tabulapdf[get_n_pages], 
  dplyr[add_count, select, distinct, filter], 
  openxlsx2[write_xlsx]
)

box::use(
  app/view/tagsTextInput[tagsTextInput], 
  app/logic/tidyup
)

#' @export
upload_view <- function(tab) {
  ns <- NS(tab)
  
  switch(
    tab, 
    "app-label" = fileInput(inputId = ns("file"), label = "上传箱单表格Excel文件", 
                            multiple = FALSE, accept = c(".xls", ".xlsx")), 
    "app-tax" = fileInput(inputId = ns("file"), label = "上传税费PDF文件", 
                          multiple = FALSE, accept = ".pdf")
  )
}

#' @export
table_view <- function(id, pdf = FALSE, input, output, ...) {
    ns <- NS(id)
    x <- list(...)
    
    if(id == "tax") {
      if(!pdf) {
        stopifnot('Check whether "table" argument present' = "table" %in% names(x))
        output$table <- renderTable(x$table())
      } else {
        # save a copy in local directory as an anchor point
        tmp <- paste("app", "static", input$file$name, sep = .Platform$file.sep)
        file.copy(input$file$datapath, tmp)
        
        output$table <- renderUI({
          tags$iframe(style = "height:600px; width:100%", src = gsub("app/", "", tmp))
        })
      }
    }
}

#' @export
param_view <- function(id, input, output, session, ...) {
  
  ns <- session$ns
  
  if(id == "label") {
    invisible()
  }
  
  if(id == "tax") {
    output$tidyUI <- renderUI({
      req(input$file)
      icon_gears <- icon(name = "gears", class = "fa-solid")
      n <- reactive(get_n_pages(input$file$datapath))
      tagList(
        h3("整理"),
        hr(), 
        sliderInput(inputId = ns("pagination"), label = "页面范围", 
                    min = 1, max = n(), value = c(1, n()), step = 1),
        actionButton(inputId = ns("tidyButton"), label = "整理", 
                     icon = icon_gears)
      )
    })
  }
}

#' @export
config_view <- function(id, choices = NULL, input, output, session) {
  ns <- session$ns
  
  if(id == "table") {
    invisible()
  }
  
  if(id == "tax") {
    output$filterCheckboxUI <- renderUI({
      req(input$tidyButton)
      conditionalPanel(
        condition = "input.tidyButton != 0", ns = ns,
        checkboxInput(inputId = ns("summ"), label = "汇总"),
        checkboxGroupInput(ns("taxCode"), label = paste0("忽略以下税费编码", "："), 
                           choices = unique(choices()[["Вид"]]), inline = TRUE)
      )
    })
    
  }
}

#' @export
filter_view <- function(input, output, session, tbl, summary = NULL, code = NULL) {
  tidied_filter <- reactive({
    base_summary <- function(x) {
      x |>
        add_count(Товар, wt = Сумма, name = "Сумма") |>
        select(-Вид) |>
        distinct()
    }
    
    if(is.null(summary())) return()
    if(!summary()) {
      if(is.null(code())) {
        tbl()
      } else {
        filter(tbl(), !Вид %in% as.vector(code()))
      }
    } else if(is.null(code())) {
      base_summary(tbl())
    } else if(!is.null(code())) {
      filter(tbl(), !Вид %in% as.vector(code())) |> base_summary()
    }
  })
  
  output$table <- renderTable(tidied_filter())
  tidied_filter
}

#' @export
export_view <- function(id, input, output, session, ...) {
  ns <- session$ns
  
  if(id == "label") {
    invisible()
  }
  
  if(id == "tax") {
    x <- list(...)
    valid_args <- c("filename", "header", "format")
    stopifnot(any(valid_args %in% names(x)))
  }
  output$export <- renderUI({
    tagList(
      h3("导出"), 
      hr(), 
      div(
        tagsTextInput(inputId = ns("header"), label = "重命名表头"),
        fluidRow(
          column(
            width = 4, 
            textInput(inputId = ns("filename"), 
                      label = "文件名", 
                      placeholder = "导出")
          ), 
          column(
            width = 6, 
            br(), 
            downloadButton(outputId = ns("download"), label = "导出")
          )
        )
      )
    )
  })
  
  tidied_rename <- reactive({
    tidyup$header_sub(x$table(), x$header())
  })
  export_file_name <- reactive({
    if(nchar(input$filename) != 0) {
      gsub(paste0(id, "-"), "", input$filename)
    } else {
      "导出"
    }
  })
  
  list(result = tidied_rename, saveslot = export_file_name)
}