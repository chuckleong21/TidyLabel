source("pdf_coord.R")
get_table_pages <- function(file, updateProgress = NULL) {
  p <- c()
  for(i in seq_len(get_n_pages(file))) {
    if(length(extract_tables(file, i)) != 0) {
      # update Progress
      if(is.function(updateProgress)) {
        text <- sprintf("%d/%d", i, i)
        updateProgress(value = i, detail = text)
      }
      p <- c(p, i)
    } else next
  }
  p
}

tidy_page <- function(file, page, version = NULL) {
  
  tax_code_dict <- paste0(c(1, 2, 5), "010")
  coordinates <- get_coordinates(file, page, version = version)
  map(coordinates, function(coor) {
    map(coor, function(l) 
      unlist(extract_tables(file = file, page = page, guess = FALSE, area = l, output = "matrix")))
  }) %>% 
    as_tibble() %>% 
    unnest(everything()) %>% 
    mutate(
      tax_code = map_chr(tax_code, \(x) {
        d <- which(str_detect(x, tax_code_dict))
        ifelse(length(d) != 0, tax_code_dict[d], x)
      }),
      tax_code = fuzzymatch(tax_code, tax_code_dict),
      across(c(weight, tax), ~gsub(",", "\\.", .x)),
      across(everything(), readr::parse_number),
      across(c(id, tax_code), as.integer)
    ) %>% 
    filter(!is.na(tax_code))
}

fuzzymatch <- function(string, dict) {
  res <- sapply(string, function(x) {
    tidystringdist::tidy_comb(x, dict) %>% 
      tidystringdist::tidy_stringdist() %>% 
      select(-hamming, -soundex) %>% 
      pivot_longer(-c(1, 2), names_to = "method", values_to = "value") %>% 
      group_by(V1) %>% 
      arrange(method) %>% 
      group_by(method) %>% 
      slice_min(value) %>% 
      ungroup() %>% 
      count(V1) %>% 
      slice_max(n) %>% 
      pull(V1)
  })
  
  map_chr(res, \(r) ifelse(length(r) != 1, NA_character_, r))
}

tidyup <- function(file, page = NULL, version = NULL, updateProgress = NULL) {
  if(!grepl("\\.pdf$", file, ignore.case = TRUE)) {
    x <- as.list(match.call())
    file_supplied <- str_extract(basename(x$file), "\\.(.+)$", group = 1)
    stop(paste("Expected pdf file, but", file_supplied, "file is supplied"), call. = FALSE)
  }
  
  n <- page %||% get_table_pages(file = file)
  imap(n, \(x, y) {
    if(is.function(updateProgress)) {
      text <- sprintf("%g%%", round(y / length(n), 2) * 100)
      updateProgress(value = y / length(n), detail = text)
    }
    tidy_page(file = file, page = x, version = version)
  }) %>% 
    list_rbind() %>% 
    filter(nchar(id) != 0)
}

header_sub <- function(tbl, x) {
  if(is_scalar_character(x)) {
    x <- strsplit(x, ",")[[1]]
  }
  y <- colnames(tbl)
  for(i in seq_along(head(x, length(y)))) {
    y[i] <- x[i]
  }
  colnames(tbl) <- y
  tbl
}