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
  test_nm <- c("etiketki", "xing_hao", "pai_zi", "yong_tu_can_shu_cai_zhi")
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
         "name" = sapply(.real$etiketki, \(i) tidy_stringdist(tidy_comb(item_fields["name"], i))[, "jw"]),
         "model" = sapply(.real$xing_hao, \(i) ifelse(str_detect(i, item_fields["model"]), 0, 1)),
         "brand" = sapply(.real$pai_zi, \(i) {
           if(str_detect(i, "нет", negate = TRUE)) {
             if(str_detect(i, item_fields["brand"])) 0 else 1
           } else {
             if(str_detect(item_fields["brand"], "отсутствует")) 0 else 1
           }
         }), 
         "material" = sapply(.real$yong_tu_can_shu_cai_zhi, \(i) {
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
  d %>% 
    as_tibble() %>% 
    unnest(everything()) %>% 
    mutate(dissimilarity = rowMeans(.[, seq_len(ncol(.))])) %>% 
    tibble::rowid_to_column("row")
}

highlight_row <- function(check_tbl) {
  r <- check_tbl %>% 
    pull(dissimilarity) %>% 
    min_rank()
  which(r == 1)
}


box_series <- "../Emma/static/OT012.xlsx"
labels <- "../Emma/static/маркировка ОТ012.xlsx"

labels_data <- readxl::read_excel(labels, sheet = "Маркировки")
labels_data <- map(seq(0, ncol(labels_data) - 2, 2), \(i) labels_data[, 1:2 + i]) %>% 
  map(\(i) {
    set_names(i, c("name", "value")) %>% 
      pivot_wider(names_from = "name", values_from = "value")
  }) %>% 
  list_rbind()

test_data <- labels_data %>% 
  select(1:5) %>% 
  janitor::clean_names() %>% 
  tibble::rowid_to_column("id") %>% 
  rename(item_no = "nomer_gruza", 
         etietki = "naimenovanie", 
         xing_hao = "model_artikul", 
         pai_zi = "brend", 
         yong_tu_can_shu_cai_zhi = "material") %>% 
  mutate(
    number = str_replace(item_no, "(.+)(\\((.+)\\))", "\\3"),
    number = ifelse(number == item_no, str_replace_all(number, "(.+)\\-(\\d+)", "1\\-\\2"), number),
    number = str_replace_all(number, "(\\d+)\\-(\\d+)", "\\1:\\2"),
    item_no = str_remove(item_no, "\\(.+\\)"), 
    xing_hao = str_trim(xing_hao)
  ) %>% 
  relocate(number, .after = item_no)

real_data <- readxl::read_excel(box_series) %>%
  slice(seq_len(nrow(.) - 3)) %>% 
  janitor::clean_names() %>% 
  fill(c(item_no:yong_tu_can_shu_cai_zhi), .direction = "down") %>% 
  select(item_no, etiketki, number, pai_zi:yong_tu_can_shu_cai_zhi) %>% 
  tibble::rowid_to_column("id") %>% 
  mutate(id = id + 1) %>% 
  mutate(number = str_replace_all(number, "\\-", ":"), 
         number = map(number, \(x) eval(parse(text = x)))) %>% 
  nest(real = c(id, etiketki:yong_tu_can_shu_cai_zhi)) %>% 
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

# list_rbind(real_data$disimilarity) %>% 
#   pull(dissimilarity) %>% 
#   log() %>% 
#   hist(breaks = 5)

highlight_tbl <- function(id) {
  highlight_colors <- real_data$disimilarity[[id]] %>% 
    select(-1, -2) %>% 
    map(\(x, y) {
      case_when(
        x <= 0.3 ~ "#198754", 
        x > 0.3 & x <= 0.4 ~ "#ffc107", 
        .default = "#dc3545"
      )
    })
  
  green <- map(highlight_colors, \(color) which(color == "#198754"))
  yellow <- map(highlight_colors, \(color) which(color == "#ffc107"))
  red <- map(highlight_colors, \(color) which(color == "#dc3545"))
  
  real_data$real[[id]] %>% 
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
        cells_body(column = etiketki, rows = green$name), 
        cells_body(column = pai_zi, rows = green$brand),
        cells_body(column = xing_hao, rows = green$model),
        cells_body(column = yong_tu_can_shu_cai_zhi, rows = green$material)
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#ffc107"), 
        cell_text(color = "#000")
      ), 
      locations = list(
        cells_body(column = etiketki, rows = yellow$name), 
        cells_body(column = pai_zi, rows = yellow$brand),
        cells_body(column = xing_hao, rows = yellow$model),
        cells_body(column = yong_tu_can_shu_cai_zhi, rows = yellow$material)
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#dc3545"), 
        cell_text(color = "#fff")
      ), 
      locations = list(
        cells_body(column = etiketki, rows = red$name), 
        cells_body(column = pai_zi, rows = red$brand),
        cells_body(column = xing_hao, rows = red$model),
        cells_body(column = yong_tu_can_shu_cai_zhi, rows = red$material)
      )
    ) %>% 
    opt_stylize(style = 4, color = "gray")
  
}

walk(seq_len(nrow(real_data)), \(i) {
  print(highlight_tbl(i))
  Sys.sleep(2.5)
})

case_when(
  test_statistic <= 0.3 ~ "#198754", 
  test_statistic > 0.3 & test_statistic <= 0.4 ~ "#ffc107", 
  .default = "#dc3545"
)

cross_table <- function(.test, .real) {
  number <- str_remove(.test[1, 2, drop = TRUE], "(?:\\(.+\\))")
  .real <- subset(.real, str_detect(item_no, number))
  list(test = .test, real = .real)
}


cross_tbl <- cross_table(labels_data[[44]], box_data)
# cross_tbl$real <- subset(cross_tbl$real, id != 17)
r <- check_labels(cross_tbl = cross_tbl) %>% highlight_row()

cross_tbl$real %>%
  slice(r) %>%
  gt() %>%
  tab_style(
    style = list(
      cell_fill(color = ifelse(length(r) > 1, "#ffc107", "#198754")),
      cell_text(color = ifelse(length(r) > 1, "#000", "#fff"))
    ),
    locations = cells_body(rows = seq_along(r))
  ) %>%
  opt_stylize(style = 3, color = "gray")
# 
# 
# check_label("name", labels_data[[1]], box_data)
# check_labels(labels_data[[14]], box_data) %>% 
#    pull(dissimilarity) %>% 
#    min_rank()