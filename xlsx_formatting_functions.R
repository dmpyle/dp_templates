
# convert "hex" from tidyxl import (AARRGGBB) to R hex format (#RRGGBB or #RRGGBBAA) or RGB or HSV
convert_tidyxl_hex <- function(tidyxl_hex, output = c("hex", "rgb", "hsv"), alpha = FALSE) {
  if(alpha) {
    r_hex <- sub(tidyxl_hex, pattern = "^(..)(..)(..)(..)$", replacement = "#\\2\\3\\4\\1")
  } else {
    r_hex <- sub(tidyxl_hex, pattern = "^(..)(..)(..)(..)$", replacement = "#\\2\\3\\4")
  }
  result <- switch(match.arg(output),
                   "hex" = {result <- r_hex},
                   "rgb" = if(alpha) {
                     result <- t(cbind(t(col2rgb(r_hex)), alpha=col2rgb(r_hex, alpha = T)[4,]))
                     } else {
                       result <- col2rgb(r_hex)
                       },
                   "hsv" = if(alpha) {
                     result <- t(cbind(t(rgb2hsv(col2rgb(r_hex))), alpha=col2rgb(r_hex, alpha = T)[4,]/255))
                     } else {
                       result <- rgb2hsv(col2rgb(r_hex))
                     }
                   )
  return(result)
}

#finds nearest match of a color (RGB/HEX format or R-recognized color name string) within a colorspace (default all R-recognized colors)
nearest_color <- function(color, template = colors(distinct = TRUE), n_guesses = 1, return_dist = FALSE, method = c("rgb", "hsv")) {
  if(is.matrix(color)) {
    col_rgb <- color
  } else {
    tryCatch(col_rgb <- col2rgb(color), error = function(e) stop("arg 'color' must be an rgb matrix or convertible to one by col2rgb()"))
  }
  template_rgb <- col2rgb(setNames(template, template))
  convert_fun <- switch(match.arg(method), "rgb" = identity, "hsv" = rgb2hsv)
  rt_sq_dist <- sqrt(colSums((convert_fun(template_rgb) - as.vector(convert_fun(col_rgb)))^2))
  ranked_template <- data.frame(color = names(sort(rt_sq_dist)), distance = sort(rt_sq_dist), row.names = NULL, stringsAsFactors = F)
  best_guess <- head(ranked_template, n_guesses)
  if(!return_dist) best_guess <- best_guess[,"color"]
  return(best_guess)
}

#imports xlsx file with cells and formatting and maps colors to nearest match
map_xlsx_formatting <- function(xlsx_path, xlsx_sheet = NULL, color_template = colors(distinct = T)) {
  #installs (if needed) and loads tidyverse packages and tidyxl
  while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  while(!require("tidyxl", quietly = T)) install.packages("tidyxl")

  xlsx_sheet <- match.arg(xlsx_sheet, choices = tidyxl::xlsx_sheet_names(xlsx_path))
  
  cells <- tidyxl::xlsx_cells(xlsx_path, sheets = xlsx_sheet, include_blank_cells = F) %>%
    dplyr::select(-character_formatted, -is_array, -is_blank, -comment) %>%
    tidyr::unite("value", c(error, logical, numeric, date, character), na.rm = T, remove = F) %>%
    tidyr::nest(formula = starts_with("formula"), data = c(error, logical, numeric, date, character)) 
  
  formatting <- tidyxl::xlsx_formats(xlsx_path)$local$font %>% 
    tibble::enframe() %>% 
    tidyr::pivot_wider() %>% 
    tidyr::unnest_wider(col = color, names_sep = "_") %>% 
    tidyr::unnest(everything()) %>% 
    dplyr::rename(font = name, color_hex = color_rgb) %>% 
    tibble::rowid_to_column("local_format_id") %>%
    dplyr::mutate(color_hex = convert_tidyxl_hex(color_hex, "hex")) %>% 
    dplyr::mutate(color = map_chr(color_hex, nearest_color), .before = color_hex)
  
  result <- dplyr::left_join(cells, formatting, by = "local_format_id")
  return(result)
}

