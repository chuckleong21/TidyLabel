box::use(
  shiny[NS, fileInput, moduleServer, 
        reactive]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  switch(
    id, 
    "app-label-file" = fileInput(inputId =  id, label = "上传箱单表格Excel文件", 
                            multiple = FALSE, accept = c(".xls", ".xlsx")), 
    "app-tax-file" = fileInput(inputId = id, label = "上传税费PDF文件", 
                          multiple = FALSE, accept = ".pdf")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    list(
      name = reactive(input[[id]]$name),
      path = reactive(input[[id]]$datapath)
    )
  })
}