format_baaksai <- function(word_file = paste0(tempdir(), "\\tmp.docx"), 
                           excel_file,
                           excel_sheet, 
                           label_position = "end",
                           visible = FALSE, 
                           progress_bar = NULL, 
                           preview = NULL) {
  # MS Word
  word <- COMCreate("Word.Application") # create MS Word App
  word[["Visible"]] <- visible # Make it invisible in taskbar
  doc <- word$Documents()$Open(word_file)
  
  # MS Excel
  excel <- COMCreate("Excel.Application")
  excel[["Visible"]] <- visible
  workbook <- excel$Workbooks()$Open(normalizePath(excel_file))
  worksheet <- workbook$Worksheets(excel_sheet)
  
  
  
  # Different from usual VBA that modifies properties such as Document.PageSetup.PageWidth straight away
  # The properties of the object it is belonged to must be assigned to an R object first
  page_setup <- doc$PageSetup()
  cms_to_pts <- word$Application()$CentimetersToPoints # No parentheses indicate a function
  # Then use the `[[` function to set corresponding properties
  page_setup[["PageWidth"]] = cms_to_pts(17)
  page_setup[["PageHeight"]] = cms_to_pts(10.25)
  page_setup[["TopMargin"]] = cms_to_pts(0)
  page_setup[["BottomMargin"]] = cms_to_pts(1)
  page_setup[["LeftMargin"]] = cms_to_pts(1)
  page_setup[["RightMargin"]] = cms_to_pts(1)
  
  tbl_count <- doc$Tables()$Count() # a numeric
  if(progress_bar) {
    progress <- shiny::Progress$new()
    progress$set(message = "套用格式：", value = 0)
  }
  
  for(i in seq_len(tbl_count)) {
    if(exists("progress")) progress$set(value = i / tbl_count, detail = sprintf("%g%%", round(i / tbl_count, 2) * 100))
    
    tbl <- doc$Tables(i)
    cell <- tbl$Cell(1, 1)
    cell_column <- tbl$Columns(1)
    paragraph_format <- cell$Range()$ParagraphFormat()
    cell[["Height"]] <- cms_to_pts(0.55)
    paragraph_format[["Alignment"]] = 2 # Enumerated value for AlignRight
    cell_column[["Width"]] <- cms_to_pts(3.93)
    
    cell <- tbl$Cell(1, 2)
    cell[["Height"]] <- cms_to_pts(0.55)
    cell_column <- tbl$Columns(2)
    cell_column[["Width"]] <- cms_to_pts(7.39)
    
    tbl_font <- tbl$Range()$Font()
    tbl_font[["Name"]] <- "Times New Roman"
    tbl_font[["Size"]] <- 5.5
  }
  if(exists("progress", inherits = FALSE)) progress$close()
  
  
  count <- worksheet$Shapes()$Count()
  wdConst <- DescTools::wdConst
  if(progress_bar) {
    progress <- shiny::Progress$new()
    progress$set(message = "迁移标签：", value = 0)
  }
  
  for(i in seq_len(count)) {
    if(exists("progress", inherits = F)) {
      progress$set(value = i / count, detail = sprintf("%g%%", round(i / count, 2) * 100))
    }
    shape <- worksheet$Shapes(i)
    addr <- shape$TopLeftCell()[["Address"]]
    page <- ceiling(worksheet$Range(addr)$Column() / 2)
    
    word$Selection()$GoTo(
      What = wdConst$wdGoToPage,
      Which = wdConst$wdGoToAbsolute,
      Count = page
    )
    
    if(label_position == "end") {
      word$Selection()$Tables(1)$Select()
      word$Selection()$Collapse(wdConst$wdCollapseEnd)
    }
    
    shape$Copy()
    shape$Select()
    word$Selection()$Paste()
  }
  on.exit({if(exists("progress", inherits = FALSE)) progress$close()})
  
  if(preview) doc$SaveAs(FileName = normalizePath("www/tmp.pdf"), FileFormat = 17)
  
  doc$Save(); doc$Close(); word$Quit(); workbook$Close(); excel$Quit()
}