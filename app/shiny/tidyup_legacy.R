# find the column(s) that contains the given keyword
col_search <- function(tbl, keyword) {
  x <- tbl %>% 
    map(\(x) str_detect(x, keyword)) %>% 
    map_dbl(sum, na.rm = TRUE)
  names(x) <- NULL
  which(x > 0)
}

# find the row(s) that contains the given keyword
row_search <- function(tbl, keyword) {
  tbl %>% 
    pmap(c) %>% 
    map(str_detect, pattern = keyword) %>% 
    map_dbl(sum, na.rm = TRUE) %>% 
    is_greater_than(0) %>% 
    which()
}

# extract the desired data with the keyword given
extract_search <- function(tbl, keyword) {
  if(keyword %in% c("32 Товар", "33 Код товара", "Вес брутто")) {
    cleaned <- tbl[, col_search(tbl, keyword)] %>% 
      drop_na() %>% 
      reduce(c)
    i <- which(str_detect(cleaned, keyword))
    res <- cleaned[i + 1]
  } else if(identical(keyword, "^Всего:?[первому|второму|третьему]?.+?:?$")) {
    r <- row_search(tbl, keyword)
    res <- tbl[row_search(tbl, keyword), ] %>% 
      pmap(c) %>% 
      map(na.omit) %>% 
      map(str_extract, pattern = "^\\d+,?\\d+?$") %>% 
      flatten() %>% 
      reduce(c) %>% 
      na.omit()
  } else stop("Keyword Out of Scope")
  
  if(length(res) != 0) {
    return(res)
  } else return(NA_character_)
}

# put all together and preserve it into a tidied data frame
tidyup <- function(tbl) {
  keywords <- c(
    product = "32 Товар", 
    hs_code = "33 Код товара", 
    weight = "Вес брутто",
    tax = "^Всего:?[первому|второму|третьему]?.+?:?$"
  )
  res <- lapply(keywords, function(k) extract_search(tbl = tbl, keyword = k)) %>% 
    as_tibble() %>% 
    filter(grepl("^\\d+$", product)) %>% 
    mutate(
      weight = gsub(",", ".", weight),
      tax = gsub(",", ".", tax),
    )
  res
}