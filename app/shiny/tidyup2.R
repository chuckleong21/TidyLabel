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

get_coordinates <- function(n, version) {
  if(n == 1) {
    id <-        switch(
      version, 
      "1" = list(list(c(339.2, 347.58, 351.66, 365.78))), 
      "2" = list(list(c(358.4255, 332.9425, 377.4152, 360.3851)))
    )
    hs_code <- switch (version,
      "1" = list(list(c(336.3251, 385.9036, 351.6561, 452.9770))), 
      "2" = list(list(c(360.3413, 367.4272, 378.3732, 483.3339)))
    )
    weight <- switch (version,
      "1" = list(list(c(361.24, 452.02, 378.49, 528.67))), 
      "2" = list(list(c(382.3311, 435.4386, 402.3208, 485.2497)))
    )
    tax_code <- switch (version,
      "1" = list(list(c(507.84, 68.74, 600.79, 90.78))), 
      "2" = list(list(c(529.72241,  47.48635, 621.68146,  74.30774)))
    )
    tax <- switch (version,
      "1" = list(list(c(506.88, 233.55, 599.83, 307.33))), 
      "2" = list(list(c(528.7645, 221.8254, 621.6815, 291.7526)))
    )
  } else {
    id <- switch (version,
      "1" = map(seq_len(3) - 1, \(i) list(c(101.57+166*i, 341.74, 117.86+166*i, 364.87))), 
      "2" = map(seq_len(3) - 1, \(i) list(c(91.12742 + 161.89*i, 348.26906, 110.15927 + 161.89*i, 375.09044)))
    )
    hs_code <- switch (version,
      "1" = map(seq_len(3) - 1, \(i) list(c(102.53+166*i, 380.07, 115.94+166*i, 431.9))), 
      "2" = map(seq_len(3) - 1, \(i) list(c(91.12742 + 161.89*i, 385.6291, 110.15927 + 161.89*i, 447.9004)))
    )
    weight <- switch (version,
      "1" = map(seq_len(3) - 1, \(i) list(c(126.48+166*i, 449.14, 141.81+166*i, 526.76))), 
      "2" = map(seq_len(3) - 1, \(i) list(c(114.1274 + 161.89*i, 442.1441, 133.1593 + 161.89*i, 523.5704)))
    )
    tax_code <- switch (version,
                        "1" = map(seq_len(3), \(i) {
                          top = 604.62; left = 61.08; bottom = 695.65; right = 89.82
                          case_match(i, 
                                     1 ~ list(c(top, left, bottom, right)), 
                                     2 ~ list(c(top, left + 257.8, bottom, right + 257.8)), 
                                     3 ~ list(c(top + 119.5, left, bottom + 119.5, right)))
                        }), 
                        "2" = map(seq_len(3), \(i) {
                          top = 585.28100; left = 61.85495; bottom = 676.28214; right = 90.59215
                          x = 257.677; y = 116.8646 
                          case_match(i, 
                                     1 ~ list(c(top,  left, bottom,  right)), 
                                     2 ~ list(c(top, left + x, bottom, right + x)), 
                                     3 ~ list(c(top + y,  left, bottom + y,  right)))
                        })
    )
    tax <- switch (version,
      "1" = map(seq_len(3), \(i) {
        top = 604.62; left = 230.68; bottom = 695.65; right = 302.54
        case_match(i, 
                   1 ~ list(c(top, left, bottom, right)), 
                   2 ~ list(c(top, left + 257.8, bottom, right + 257.8)), 
                   3 ~ list(c(top + 117, left, bottom + 117, right)))
      }), 
      "2" = map(seq_len(3), \(i) {
        top = 585.28100; left = 233.3203; bottom = 676.28214; right = 306.1212
        x = 257.677; y = 116.8646
        case_match(i, 
                   1 ~ list(c(top, left, bottom, right)), 
                   2 ~ list(c(top, left + x, bottom, right + x)), 
                   3 ~ list(c(top + y, left, bottom + y, right)))
      })
    )
  }
    list(id = id, hs_code = hs_code, weight = weight, tax_code = tax_code, tax = tax)
}

tidy_page <- function(file, page, version) {
  tax_code_dict <- paste0(c(1, 2, 5), "010")
  coordinates <- get_coordinates(page, version)
  map(coordinates, function(coor) {
    map(coor, function(l) 
      unlist(extract_tables(file = file, page = page, guess = FALSE, area = l, output = "matrix")))
  }) %>% 
    as_tibble() %>% 
    unnest(everything()) %>% 
    mutate(tax_code = fuzzymatch(tax_code, tax_code_dict), 
           across(c(weight, tax), ~gsub(",", "\\.", .x)),
           across(everything(), readr::parse_number), 
           across(c(id, tax_code), as.integer))
}


fuzzymatch <- function(string, dict) {
  sapply(string, function(x) {
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
}


tidyup <- function(file, page = NULL, version, updateProgress = NULL) {
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