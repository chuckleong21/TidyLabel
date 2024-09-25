box::use(
  purrr[map, map_int, map_chr, imap, map_vec,
        list_rbind, is_scalar_character, pmap], 
  tabulapdf[extract_tables],
  dplyr[as_tibble, everything, filter, if_all, 
        select, arrange, group_by, slice_min, ungroup,
        mutate, across, count, slice_max, pull, case_when], 
  tidyr[unnest, pivot_wider, pivot_longer, drop_na], 
  stringr[str_detect, str_extract],
  readr[parse_number], 
  tidystringdist[tidy_comb, tidy_stringdist], 
  utils[head]
)

box::use(
  app/logic/pdf_coord[get_coordinates, 
                      load_coordinates, 
                      get_pdf_version],
)

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
  ver <- version %||% get_pdf_version(file)$version
  coordinates <- load_coordinates() |> 
    dplyr::filter(version %in% ver, first == (page == 1 ))
  
  extracts <- coordinates |>
    mutate(
      extract = pmap(.l = coordinates, \(version, first, property, type, 
                                         top, left, bottom, right) {
        extract_tables(file = file, 
                       page = page, guess = FALSE, 
                       area = list(c(top, left, bottom, right)), 
                       output = "matrix") |>
          unlist()
      }),
      extract = map(extract, \(x) if(length(x) == 0) NA_character_ else x),
      len = map(extract, `length<-`, max(length(extract))), 
    ) 
  
  if(nrow(coordinates) %% 15 == 0) {
    extracts <- extracts |> 
      mutate(position = rep(rep(1:3, each = 5), nrow(coordinates) %/% 15))
  } else {
    extracts <- extracts |> 
      mutate(position = 1)
  }
  
  extracts |> 
    pivot_wider(id_cols = c(type, position), names_from = property, values_from = len) |>
    unnest(id:tax) |> 
    drop_na(id) |> 
    mutate(tax_code = fuzzymatch(tax_code, tax_code_dict))
}

fuzzymatch <- function(string, dict) {
  res <- sapply(string, function(x) {
    tidy_comb(x, dict) |> 
      tidy_stringdist() |> 
      select(-hamming, -soundex) |> 
      pivot_longer(-c(1, 2), names_to = "method", values_to = "value") |> 
      group_by(V1) |> 
      arrange(method) |> 
      group_by(method) |> 
      slice_min(value) |> 
      ungroup() |> 
      count(V1) |> 
      slice_max(n) |> 
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
  }) |> 
    list_rbind()
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