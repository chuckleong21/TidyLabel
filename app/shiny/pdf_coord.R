get_pdf_version <- function(file) {
  version_coord <- list(
    list(c(182.05631,  60.8, 231.88225, 315.95563)), 
    list(c(214.57110,  45.57053, 261.50853, 305.16325))
  )
  
  version_meta <- map(version_coord, \(v) extract_tables(
    file = file, pages = 1, area = v, guess = FALSE, output = "matrix"
  )) %>% 
    list_flatten()
  i <- map_int(version_meta, nrow); j <- map_int(version_meta, ncol)
  jj <- which(i == 5); jj <- ifelse(length(jj) != 1, ifelse(length(j) != 1, 1L, jj), jj)
  v <- unlist(version_meta[jj])
  version <- ifelse(length(v) == 10, 
                    str_extract(v[6], "(\\d+)/", group = 1), 
                    str_extract(v[1], "(\\d+)/", group = 1))
  organisation <- str_c(v[2:5], collapse = "\n")
  list(version = version, org = organisation)
}

get_coordinates <- function(file, n, version = NULL) {
  version <- version %||% get_pdf_version(file)$version
  if(n == 1) {
    if(version %in% c("7801664214", "7805640197")) {
      id       <- list(list(c(339.2, 347.58, 351.66, 365.78)))
      hs_code  <- list(list(c(336.3251, 385.9036, 351.6561, 452.9770)))
      weight   <- list(list(c(361.24, 452.02, 378.49, 528.67)))
      tax_code <- list(list(c(507.84, 68.74, 600.79, 90.78)))
      tax      <- list(list(c(506.88, 233.55, 599.83, 307.33)))
    } else if(version %in% "2540263576") {
      id       <- list(list(c(358.4255, 332.9425, 377.4152, 360.3851)))
      hs_code  <- list(list(c(360.3413, 367.4272, 378.3732, 483.3339)))
      weight   <- list(list(c(382.3311, 435.4386, 402.3208, 485.2497)))
      tax_code <- list(list(c(529.72241, 47.48635, 621.68146, 74.30774)))
      tax      <- list(list(c(528.7645, 221.8254, 621.6815, 291.7526)))
    } else if(version %in% "7816168667") {
      id       <- list(list(c(335.3669, 340.8686, 353.5725, 360.9906)))
      hs_code  <- list(list(c(336.3251, 379.1962, 350.6980, 446.2696)))
      weight   <- list(list(c(361.2381, 445.3114, 378.4855, 524.8413)))
      tax_code <- list(list(c(507.84130, 62.03498, 601.74403, 86.94795)))
      tax      <- list(list(c(507.84130, 227.8020, 601.74403, 302.5410)))
    }
  } else {
    id <- switch (version,
                  "7801664214" = map(seq_len(3) - 1, \(i) list(c(101.57+166*i, 341.74, 117.86+166*i, 364.87))), 
                  "2540263576" = map(seq_len(3) - 1, \(i) list(c(91.12742 + 161.89*i, 348.26906, 110.15927 + 161.89*i, 375.09044))),
                  "7805640197" = map(seq_len(3) - 1, \(i) list(c(107.2754+172.0468*i, 346.5086, 120.6848+172.0468*i, 367.5805))),
                  "7816168667" = map(seq_len(3) - 1, \(i) list(c(101.57+166*i, 341.74, 117.86+166*i, 364.87)))
    )
    hs_code <- switch (version,
                       "7801664214" = map(seq_len(3) - 1, \(i) list(c(102.53+166*i, 380.07, 115.94+166*i, 431.9))), 
                       "2540263576" = map(seq_len(3) - 1, \(i) list(c(91.12742 + 161.89*i, 385.6291, 110.15927 + 161.89*i, 447.9004))),
                       "7805640197" = map(seq_len(3) - 1, \(i) list(c(108.2332+172.4068*i, 382.9056, 120.6848+172.4068*i, 450.9105))),
                       "7816168667" = map(seq_len(3) - 1, \(i) list(c(102.53+166*i, 380.07, 115.94+166*i, 431.9)))
    )
    weight <- switch (version,
                      "7801664214" = map(seq_len(3) - 1, \(i) list(c(126.48+166*i, 449.14, 141.81+166*i, 526.76))), 
                      "2540263576" = map(seq_len(3) - 1, \(i) list(c(114.1274 + 161.89*i, 442.1441, 133.1593 + 161.89*i, 523.5704))),
                      "7805640197" = map(seq_len(3) - 1, \(i) list(c(129.3051+172.0468*i, 451.8683, 146.5458+172.0468*i, 530.4092))), 
                      "7816168667" = map(seq_len(3) - 1, \(i) list(c(126.48+166*i, 449.14, 141.81+166*i, 526.76)))
    )
    tax_code <- switch (version,
                        "7801664214" = map(seq_len(3), \(i) {
                          top = 604.62; left = 61.08; bottom = 695.65; right = 89.82
                          x = 257.8; y = 119.5
                          case_match(i, 
                                     1 ~ list(c(top, left, bottom, right)), 
                                     2 ~ list(c(top, left + x, bottom, right + x)), 
                                     3 ~ list(c(top + y, left, bottom + y, right)))
                        }), 
                        "2540263576" = map(seq_len(3), \(i) {
                          top = 585.28100; left = 61.85495; bottom = 676.28214; right = 90.59215
                          x = 257.677; y = 116.8646 
                          case_match(i, 
                                     1 ~ list(c(top,  left, bottom,  right)), 
                                     2 ~ list(c(top, left + x, bottom, right + x)), 
                                     3 ~ list(c(top + y,  left, bottom + y,  right)))
                        }),
                        "7805640197" = map(seq_len(3), \(i) {
                          top = 634.07398; left = 56.29045; bottom = 706.8680; right = 86.94055
                          x = 265.315; y = 95.8
                          case_match(i, 
                                     1 ~ list(c(top,  left, bottom,  right)), 
                                     2 ~ list(c(top, left + x, bottom, right + x)), 
                                     3 ~ list(c(top + y,  left, bottom + y,  right)))
                        }), 
                        "7816168667" = map(seq_len(3), \(i) {
                          top = 604.62; left = 61.08; bottom = 695.65; right = 89.82
                          x = 257.8; y = 119.5
                          case_match(i, 
                                     1 ~ list(c(top, left, bottom, right)), 
                                     2 ~ list(c(top, left + x, bottom, right + x)), 
                                     3 ~ list(c(top + y, left, bottom + y, right)))
                        })
    )
    tax <- switch (version,
                   "7801664214" = map(seq_len(3), \(i) {
                     top = 604.62; left = 230.68; bottom = 695.65; right = 302.54
                     case_match(i, 
                                1 ~ list(c(top, left, bottom, right)), 
                                2 ~ list(c(top, left + 257.8, bottom, right + 257.8)), 
                                3 ~ list(c(top + 117, left, bottom + 117, right)))
                   }), 
                   "2540263576" = map(seq_len(3), \(i) {
                     top = 585.28100; left = 233.3203; bottom = 676.28214; right = 306.1212
                     x = 257.677; y = 116.8646
                     case_match(i, 
                                1 ~ list(c(top, left, bottom, right)), 
                                2 ~ list(c(top, left + x, bottom, right + x)), 
                                3 ~ list(c(top + y, left, bottom + y, right)))
                   }),
                   "7805640197" = map(seq_len(3), \(i) {
                     top = 634.0740; left = 228.6973; bottom = 706.8680; right = 298.6178
                     x = 265.315; y = 95.8
                     case_match(i, 
                                1 ~ list(c(top, left, bottom, right)), 
                                2 ~ list(c(top, left + x, bottom, right + x)), 
                                3 ~ list(c(top + y, left, bottom + y, right)))
                   }), 
                   "7816168667" = map(seq_len(3), \(i) {
                     top = 604.62; left = 230.68; bottom = 695.65; right = 302.54
                     case_match(i, 
                                1 ~ list(c(top, left, bottom, right)), 
                                2 ~ list(c(top, left + 257.8, bottom, right + 257.8)), 
                                3 ~ list(c(top + 117, left, bottom + 117, right)))
                   })
    )
  }
  list(id = id, hs_code = hs_code, weight = weight, tax_code = tax_code, tax = tax)
}

