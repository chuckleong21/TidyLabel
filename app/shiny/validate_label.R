filter_by_number <- function(data) {
  
  rectangularize <- function(df) {
    max_len <- df %>%
      map(unlist) %>%
      lengths() %>%
      max()
    
    df %>%
      map(unlist) %>%
      map(~ `length<-`(., max_len)) %>%
      bind_cols()
  }
  
  # browser()
  i <- data$number %>% str_split(", ") %>% map(\(x) map(x, \(x) eval(parse(text = x))))
  j <- reduce(unique(data$real), tibble)$number
  q <- map(i, 
           \(x) map(x, \(xi) map(j, \(yi) sum(!yi %in% xi)))) %>% 
    map(\(x) {
      tibble::enframe(x) %>% 
        pivot_wider(names_from = name, values_from = value) %>% 
        unnest_longer(everything())
    }) %>% 
    tibble::enframe()
  
  map(q$value, \(q) map(q, \(qq) which(qq==0))) %>% 
    map(tibble::enframe) %>% 
    map(\(q) pivot_wider(q, names_from = name, values_from = value) %>% 
          rectangularize()) %>% 
    tibble::enframe() %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    rectangularize()
}

check_real_column <- function(cross_tbl) {
  test_nm <- c("name", "model", "brand", "material")
  all(test_nm %in% colnames(reduce(cross_tbl$real, tibble))[-1])
}

check_label <- function(field, cross_tbl) {
  .real <- reduce(cross_tbl$real, tibble)
  stopifnot("One or more test columns are not available" = check_real_column(cross_tbl))
  item_fields <- unlist(cross_tbl[, 3:8]) %>% 
    set_names(c("id", "number", "name", "model", "brand", "material")) %>% 
    append(c("item_no" = cross_tbl$item_no))
  
  switch(field,
         "item_no" = sapply(cross_tbl$item_no, \(i) ifelse(item_fields["item_no"] == i, 0, 1)),
         "name" = sapply(.real$name, \(i) tidy_stringdist(tidy_comb(item_fields["name"], i))[, "jw"]),
         "model" = sapply(.real$model, \(i) ifelse(is.na(i), -1, ifelse(str_detect(i, item_fields["model"]), 0, 1))),
         "brand" = sapply(.real$brand, \(i) {
           if(is.na(i)) {
             -1
           } else {
             if(str_detect(i, "нет", negate = TRUE)) {
               if(str_detect(i, item_fields["brand"])) 0 else 1
             } else {
               if(str_detect(item_fields["brand"], "отсутствует")) 0 else 1
             }
           }
         }), 
         "material" = sapply(.real$material, \(i) {
           a <- ifelse(str_detect(i, item_fields["material"]), 0, 1)
           b <- tidy_stringdist(tidy_comb(item_fields["material"], i))[, "jw"]
           (a + b) / 2 
         }),
         stop("illegal field", call. = FALSE)
  )
}

check_labels <- function(cross_tbl, ignore.warn = TRUE) {
  fields <- c("item_no", "name", "model", "brand", "material")
  if(ignore.warn) {
    d <- sapply(fields, \(i) suppressWarnings(check_label(i, cross_tbl = cross_tbl)))
  } else  {
    d <- sapply(fields, \(i) check_label(i, cross_tbl = cross_tbl))
  }
  names(d)[1:5] <- fields 
  d %>% 
    as_tibble() %>% 
    unnest(everything()) %>% 
    mutate(dissimilarity = rowMeans(.[, seq_len(ncol(.))])) %>% 
    tibble::rowid_to_column("row")
}

highlight_tbl <- function(crosstbl, id) {
  highlight_colors <- crosstbl$disimilarity[[id]] %>% 
    select(-1) %>% 
    map(\(x) {
      case_when(
        x >= 0 & x <= 0.3 ~ "#198754", 
        x > 0.3 & x <= 0.4 ~ "#ffc107", 
        x > 0.4 ~ "#dc3545",
        x < 0 ~ "gray25"
      )
    })
  
  green <- map(highlight_colors, \(color) which(color == "#198754"))
  yellow <- map(highlight_colors, \(color) which(color == "#ffc107"))
  red <- map(highlight_colors, \(color) which(color == "#dc3545"))
  gray <- map(highlight_colors, \(color) which(color == "gray25"))
  
  crosstbl$real[[id]] %>% 
    select(-number) %>% 
    gt() %>% 
    tab_style(
      style = list(
        cell_fill(color = "gray40"), 
        cell_text(color = "#fff")
      ),
      locations = cells_body(columns = id)
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#198754"), 
        cell_text(color = "#fff")
      ), 
      locations = list(
        cells_body(column = item_no, rows = green$item_no), 
        cells_body(column = name, rows = green$name), 
        cells_body(column = brand, rows = green$brand),
        cells_body(column = model, rows = green$model),
        cells_body(column = material, rows = green$material)
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#ffc107"), 
        cell_text(color = "#000")
      ), 
      locations = list(
        cells_body(column = item_no, rows = yellow$item_no), 
        cells_body(column = name, rows = yellow$name), 
        cells_body(column = brand, rows = yellow$brand),
        cells_body(column = model, rows = yellow$model),
        cells_body(column = material, rows = yellow$material)
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#dc3545"), 
        cell_text(color = "#fff")
      ), 
      locations = list(
        cells_body(column = item_no, rows = red$item_no), 
        cells_body(column = name, rows = red$name), 
        cells_body(column = brand, rows = red$brand),
        cells_body(column = model, rows = red$model),
        cells_body(column = material, rows = red$material)
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "gray25"), 
        cell_text(color = "#fff")
      ), 
      locations = list(
        cells_body(column = item_no, rows = gray$item_no), 
        cells_body(column = name, rows = gray$name), 
        cells_body(column = brand, rows = gray$brand),
        cells_body(column = model, rows = gray$model),
        cells_body(column = material, rows = gray$material)
      )
    ) %>% 
    sub_missing(missing_text = "-") %>% 
    tab_options(column_labels.background.color = "gray40")
}

label_data <- function(file = NULL, sheet = NULL, copy = FALSE, path = paste0(tempdir(), "\\tmp.rds")) {
  if(!is.null(file) & !is.null(sheet)) {
    labels_data <- openxlsx2::read_xlsx(file = file, sheet = sheet, skip_empty_rows = TRUE)
    labels_data[1, ]
  } else {
    return(tibble::tibble())
  }
  names(labels_data)[which(is.na(names(labels_data)))] <- seq_along(names(labels_data)[which(is.na(names(labels_data)))]) + 0.1
  labels_data <- map(seq(0, ncol(labels_data) - 2, 2), \(i) labels_data[, 1:2 + i]) %>% 
    map(\(i) as_tibble(i) %>% set_names(c("name", "value")))
  
  if(copy) saveRDS(labels_data, file = path)
  labels_data
}

cross_tbl <- function(box_series_path, label_series_path, label_sheet) {
  
  test_data <- label_data(file = label_series_path, sheet = label_sheet) %>%
    map(\(i) i %>%
          pivot_wider(names_from = "name", values_from = "value")) %>%
    list_rbind() %>%
    select(1:5) %>%
    set_names(c("item_no", "name", "model", "brand", "material")) %>%
    tibble::rowid_to_column("id") %>%
    mutate(
      number = str_replace(item_no, "(.+)(\\((.+)\\))", "\\3"),
      number = ifelse(number == item_no, str_replace_all(number, "(.+)\\-(\\d+)", "1\\-\\2"), number),
      number = str_replace_all(number, "(\\d+)\\-(\\d+)", "\\1:\\2"),
      item_no = str_remove(item_no, "\\(.+\\)"),
      item_no = str_trim(item_no),
      model = str_trim(model)
    ) %>%
    relocate(number, .after = item_no)
  
  real_data <- openxlsx2::read_xlsx(file = box_series_path, sheet = "东线模板", 
                                    fill_merged_cells = TRUE, skip_empty_rows = TRUE) %>%
    select(c(1, 3, 4, 7:9)) %>%
    as_tibble() %>%
    set_names(c("item_no", "number", "name", "brand", "model", "material")) %>%
    filter(!is.na(item_no)) %>%
    tibble::rowid_to_column("id") %>%
    mutate(id = id + 1,
           item_no = str_trim(item_no),
           number = str_replace_all(number, "\\-", ":"),
           number = map(number, \(x) eval(parse(text = x)))) %>%
    nest(real = c(id, number:material)) %>% 
    mutate(real = map(seq_along(real), \(i) real[[i]] %>% 
                        mutate(item_no = item_no[i]) %>% 
                        relocate(item_no, .after = number))) %>% 
    right_join(test_data, join_by("item_no"))
  
  real_data <- real_data %>%
    left_join(real_data %>%
                group_by(item_no) %>%
                do(nest(filter_by_number(.))),
              join_by(item_no)) %>%
    group_by(item_no) %>%
    mutate(group_id = row_number(),
           real = pmap(list(real, data, group_id), \(x = real, y = data, id = group_id) {
             x[na.omit(y[, id, drop = TRUE]), ]
           })) %>%
    mutate(disimilarity = map(id, \(id = id) check_labels(.[id, ])))
  
  real_data
}

validateUI <- function(id, i18n, current_page = NULL, total_page = NULL) {
  ns <- NS(id)
  
  useShinyjs()
  # label view
  tagList(
    fluidRow(
      column(
        width = 12, 
        tableOutput(ns("singleLabel"))
      )
    ),
    # pagination
    fluidRow(
      column(width = 4,actionButton(ns("prevLabel"), label = i18n$translate("上一个"))), 
      column(width = 4, htmlOutput(ns("refreshBtn"))), 
      column(width = 4, actionButton(ns("nextLabel"), label = i18n$translate("下一个")))
    ),
    # result view
    fluidRow(
      column(width = 12, gt_output(ns("validateResult")))
    )
  )
}

validateServer <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    p <- reactiveVal(1)
    n <- reactive(length(session$userData$label$data()))
    
    output$singleLabel <- renderTable({
      session$userData$label$data()[[p()]]
    })
    
    output$refreshBtn <- renderUI({
      actionButton(ns("refreshLabel"), label = sprintf("%d/%d", p(), n()), icon = icon("refresh", style = "color:navyblue"))
    })
    
    observe({
      toggleState(id = ns("prevLabel"), condition = p() > 1, asis = TRUE)
    })
    observe({
      toggleState(id = ns("nextLabel"), condition = p() < n(), asis = TRUE)
    })
    
    observeEvent(input$refreshLabel, {
      p(1)
    })
    
    observeEvent(input$prevLabel, {
      i <- p() - 1
      p(i)
    })
    
    observeEvent(input$nextLabel, {
      i <- p() + 1
      p(i)
    })
    
    output$validateResult <- render_gt({
      req(session$userData$box$data())
      highlight_tbl(session$userData$box$data(), p())
    })
  })
}