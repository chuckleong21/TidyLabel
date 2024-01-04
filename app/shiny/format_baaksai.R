format_baaksai <- function(file = paste0(tempdir(), "\\tmp.docx")) {
  word <- COMCreate("Word.Application") # open MS Word App
  word[["Visible"]] <- FALSE # Make it invisible in taskbar
  
  doc <- word$Documents()$Open(file)
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
  
  for(i in seq_len(tbl_count)) {
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
  
  invisible(doc$Save())
  invisible(doc$Close()) 
  invisible(word$Quit())
}