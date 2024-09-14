box::use(
  shiny[NS, tagList, fluidRow, column, h3, 
        fileInput,
        selectInput,
        uiOutput, renderUI, renderTable,
        verbatimTextOutput, renderPrint,
        conditionalPanel, 
        checkboxInput, checkboxGroupInput,
        reactive, observe, req, eventReactive, observeEvent, 
        Progress,
        moduleServer],
  tabulapdf[get_n_pages], 
  dplyr[rename]
)

box::use(
  app/view/upload,
  app/view/table,
  app/view/components,
  app/logic/pdf_coord[get_pdf_version],
  app/logic/tidyup,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        h3("上传"),
        components$upload_view(tab = id),
        if(id == "app-label") uiOutput(ns("selectSheet")),
        if(id == "app-tax") uiOutput(ns("tidyUI")),
        if(id == "app-tax") uiOutput(ns("filterCheckboxUI")),
        if(id == "app-tax") uiOutput(ns("export"))
      ), 
      column(
        width = 8, 
        uiOutput(ns("table"))
      )
    )
  )
}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if(id == "label") {
      output$table <- renderPrint({
        input$file$datapath
      })
      output$selectSheet <- renderUI({
        selectInput(inputId = ns("sheet"), label = "选择表格", choices = letters[1:3])
      })
    }
    
    if(id == "tax") {
      observe({
        req(input$file)
        components$table_view(id = id, pdf = TRUE, input = input, output = output)
      })
      
      components$param_view(
        id = id, 
        input = input, 
        output = output, 
        session = session 
      )
      tidied <- eventReactive(input$tidyButton, {
        req(input$file)
        progress <- Progress$new()
        progress$set(message = "整理数据", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if(is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 4
          }
          progress$set(value = value, detail = detail)
        }
        
        tidyup$tidyup(file = input$file$datapath, page = do.call(seq, as.list(input$pagination)), 
                      version = get_pdf_version(input$file$datapath)$version, 
                      updateProgress = updateProgress) |> 
          rename(
            "Товар" = id,
            "Код товара" = hs_code,   
            "Вес брутто" = weight,
            "Вид" = tax_code,
            "Сумма" = tax,
          )
      })
      
      observeEvent(input$tidyButton, {
        components$table_view(
          id = "tax", 
          input = input, 
          output = output, 
          session = session,
          table = tidied
        )
        components$config_view(
          id = "tax", 
          choices = tidied,
          input = input, 
          output = output, 
          session = session
        )
      })
      observe({
        # must be called first for reactivity
        invisible(c(input$taxCode, input$summ))
        filtered <- components$filter_view(
          input = input, 
          output = output, 
          session = session, 
          tbl = tidied, 
          summary = reactive(input$summ),
          code = reactive(input$taxCode)
        )
        invisible(c(input$taxCode, input$summ))
        exported <- components$export_view(
          id = "tax",
          input = input,
          output = output,
          session = session,
          table = filtered,
          filename = reactive(input$filename),
          header = reactive(input$header)
        )
        
        output$table <- renderTable(exported$result())
      })
    }
  })
}