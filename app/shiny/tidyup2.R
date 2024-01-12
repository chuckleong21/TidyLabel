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

get_coordinates <- function(n) {
  if(n == 1) {
    id <-        list(list(c(339.2, 347.58, 351.66, 365.78)))
    hs_code <-   list(list(c(339.2, 384.95, 353.57, 439.56)))
    weight <-    list(list(c(361.24, 452.02, 378.49, 528.67)))
    tax_code  <- list(list(c(507.84, 68.74, 600.79, 90.78)))
    tax  <-      list(list(c(506.88, 233.55, 599.83, 307.33)))
  } else {
    id <- map(seq_len(3) - 1, \(i) list(c(101.57+166*i, 341.74, 117.86+166*i, 364.87)))
    hs_code <- map(seq_len(3) - 1, \(i) list(c(102.53+166*i, 380.07, 115.94+166*i, 431.9)))
    weight <- map(seq_len(3) - 1, \(i) list(c(126.48+166*i, 449.14, 141.81+166*i, 526.76)))
    tax_code <- map(seq_len(3), \(i) {
      top = 604.62; left = 61.08; bottom = 695.65; right = 89.82
     case_match(i, 
                        1 ~ list(c(top, left, bottom, right)), 
                        2 ~ list(c(top, left + 257.8, bottom, right + 257.8)), 
                        3 ~ list(c(top + 119.5, left, bottom + 119.5, right)))
    })
    tax <- map(seq_len(3), \(i) {
      top = 604.62; left = 230.68; bottom = 695.65; right = 302.54
      case_match(i, 
                        1 ~ list(c(top, left, bottom, right)), 
                        2 ~ list(c(top, left + 257.8, bottom, right + 257.8)), 
                        3 ~ list(c(top + 117, left, bottom + 117, right)))
    })
  }
    list(id = id, hs_code = hs_code, weight = weight, tax_code = tax_code, tax = tax)
}

tidy_page <- function(file, page) {
  coordinates <- get_coordinates(page)
  map(coordinates, function(coor) {
    map(coor, function(l) 
      unlist(extract_tables(file = file, page = page, guess = FALSE, area = l)))
  }) %>% 
    as_tibble() %>% 
    unnest(everything())
}


tidyup <- function(file, page = NULL, updateProgress = NULL) {
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
    tidy_page(file = file, page = x)
  }) %>% 
    list_rbind() %>% 
    filter(nchar(id) != 0) %>% 
    mutate(
      across(c(weight, tax), ~gsub(",", "\\.", .x)),
      across(everything(), readr::parse_number), 
      tax_code = case_match(tax_code, 1 ~ 1010, 2 ~ 2010, 5 ~ 5010, .default = tax_code)
    )
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