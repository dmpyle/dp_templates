
#### Functions to map xlsx formatting and colors ####
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
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
    #tidyxl package
    while(!require("tidyxl", quietly = T)) install.packages("tidyxl")
  })
  
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
    dplyr::mutate(color = purrr::map_chr(color_hex, nearest_color), .before = color_hex)
  
  result <- dplyr::left_join(cells, formatting, by = "local_format_id")
  return(result)
}

#### RhoRand Schedule Template functions ####
#split a character vector in RhoRand Schedule format ("Stage # Visit # (Week #)" vs "Stage # DBPCFC XX Window") into stage, visit, week columns 
parse_schedule <- function(rhoRand_names) {
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
    })

  sched_tbl <- tibble::tibble(source = rhoRand_names) %>%
    dplyr::mutate(source = stringr::str_remove_all(stringr::str_to_lower(source),
                                                   c("\\s(?=\\d)" = "_", "(?<=dbpcfc)\\s|\\s(?=window)" = "_", "\\(|\\)" = ""))) %>%
    tidyr::separate(source, into = c("stage", "visit", "week"), sep = "\\s", fill = "right") %>%
    dplyr::mutate(week = readr::parse_number(week)) %>%
    dplyr::mutate(week = as.integer(week))
  
  return(sched_tbl)
}

#import rhoRand schedule template from xlsx file and parse event names, dates, and encoding
import_rhoRand_schedule <- function(xlsx_path, xlsx_sheet = NULL, id_cols = c(subject_id = "ID", xolair_freq = "Omalizumab Dose Frequency"),
                                    parse_event_names = T, parse_dates = T) {
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages, lubridate, and readxl
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
    require(readxl, quietly = T)
    require(lubridate, quietly = T)
    #tidyxl package
    while(!require("tidyxl", quietly = T)) install.packages("tidyxl")
  })


  xlsx_sheet <- match.arg(xlsx_sheet, choices = tidyxl::xlsx_sheet_names(xlsx_path))
  id_cols <- purrr::set_names(match.arg(id_cols, choices = colnames(readxl::read_excel(xlsx_path)), several.ok = T), names(id_cols))
  
  rhoRand_schedule <- readxl::read_excel(xlsx_path, sheet = xlsx_sheet) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(unname(id_cols)), ~!is.na(.x)))
  
  rhoRand_schedule_colormap <- map_xlsx_formatting(xlsx_path, xlsx_sheet) %>%
    dplyr::select(row_n = row, col_n = col, cell_color = color)
  
  schedule_tbl <- rhoRand_schedule %>% 
    dplyr::rename(all_of(id_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(names(id_cols)), stringr::str_replace_all, c("^Every\\s" = "q", "\\sweeks$" = "w"))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(names(id_cols)))) %>%
    tidyr::pivot_longer(-dplyr::group_cols(), names_to = "event_name", values_to = "date_value")  %>%
    dplyr::mutate(encoded_text = stringr::str_extract(date_value, "\\*+")) %>%
    dplyr::mutate(row_n = dplyr::cur_group_id()+1, col_n = dplyr::row_number()+2) %>% 
    dplyr::left_join(rhoRand_schedule_colormap, by = c("col_n", "row_n")) %>%
    dplyr::select(-row_n, -col_n) %>%
    dplyr::ungroup() 
  
  if(parse_dates) {
    schedule_tbl <- schedule_tbl %>% 
      dplyr::mutate(date = lubridate::dmy(date_value), .after = date_value, .keep = "unused")
  } 
  if(parse_event_names) {
    schedule_tbl <- schedule_tbl %>% 
      dplyr::mutate(parse_schedule(event_name), .after = event_name, .keep = "unused")
  } 
  
  return(schedule_tbl)
}

#map rhoRand schedule encoding (color, *, **) into meaningful values
map_rhoRand_encoding <- function(schedule_tbl_or_path, xlsx_sheet = NULL, map_colors = T, estimate_dates = T, map_dependencies = T) {
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  })

  if(is.data.frame(schedule_tbl_or_path)) {
    schedule_tbl <- tibble::as_tibble(schedule_tbl_or_path)     
  } else if(is.character(schedule_tbl_or_path)){
    if(file.exists(schedule_tbl_or_path)) {
      schedule_tbl <- import_rhoRand_schedule(xlsx_path = schedule_tbl_or_path, xlsx_sheet = xlsx_sheet)
    } else stop("file path does not exist")
  } else stop("schedule_tbl_or_path must be either a data.frame/tibble object or a file path")
  
  if(map_colors) {
    mapped_tbl <- schedule_tbl %>%
      dplyr::mutate(visit_completed = dplyr::case_when(cell_color=="black" ~ TRUE, cell_color=="blue" ~ FALSE), .keep = "unused")
  } else {
    mapped_tbl <- schedule_tbl
  }
  
  if(estimate_dates & map_dependencies) {
    mapped_tbl <- mapped_tbl %>%
      dplyr::mutate(date_estimated = if_else(is.na(date),NA,!is.na(encoded_text))) %>%
      dplyr::mutate(date_depends_on = case_when(stringr::str_detect(encoded_text, "^\\*{2}$") ~ paste("start date of stage"),
                                                stringr::str_detect(encoded_text, "^\\*{1}$") ~ "previous visit date",
                                                is.na(encoded_text) ~ NA_character_), .keep = "unused")
  } else if(estimate_dates & !map_dependencies) {
    mapped_tbl <- mapped_tbl %>%
      dplyr::mutate(date_estimated = if_else(is.na(date),NA,!is.na(encoded_text)), .keep = "unused")
  } else if(!estimate_dates & map_dependencies) {
    mapped_tbl <- mapped_tbl %>% 
      dplyr::mutate(date_depends_on = case_when(stringr::str_detect(encoded_text, "^\\*{2}$") ~ paste("start date of stage"),
                                                stringr::str_detect(encoded_text, "^\\*{1}$") ~ "previous visit date",
                                                is.na(encoded_text) ~ NA_character_), .keep = "unused")
  }
  
  return(mapped_tbl) 
}

#add artificial template for a stage not in the rhoRand schedule
add_stage_template <- function(curr_schedule, new_stage = "screening", visits = c("visit_0"),
                               completed = T, incl_dbpcfc = T, before_stage = c(curr_schedule$stage, "end"), 
                               offset_wks = (incl_dbpcfc*4)+length(visits)+1) {
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages and lubridate
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
    require(lubridate, quietly = T)
  })

  before_stage <- match.arg(before_stage, c(curr_schedule$stage, "end"))
  nested_schedule <- dplyr::nest_by(curr_schedule, subject_id, xolair_freq, .key = "subject_sched")
  
  if(before_stage=="end") {
    for(i in 1:length(visits)) {
      nested_schedule <- nested_schedule %>%
        dplyr::mutate(subject_sched = list(
          tibble::add_row(subject_sched, stage = new_stage, visit = visits[i], week = i - 1L,
                          date = dplyr::last(subject_sched$date) + lubridate::weeks(i),
                          visit_completed = T, date_estimated = T)))
    }
    if(incl_dbpcfc) {
      nested_schedule <- nested_schedule %>%
        dplyr::mutate(subject_sched = list(
          tibble::add_row(subject_sched, stage = new_stage, visit = paste0("dbpcfc_", c("start", "end"), "_window"),
                          week = NA_integer_, visit_completed = completed, date_estimated = T,
                          date = c(dplyr::last(subject_sched$date) + lubridate::weeks(1),
                                   dplyr::last(subject_sched$date) + lubridate::weeks(5)))))
    }
  } else {
    nested_schedule <- nested_schedule %>%
      dplyr::mutate(dplyr::across(subject_sched, .names = "{.fn}", 
                                  .fn = list(before_row = ~detect_index(.x$stage, ~.x == before_stage))))
    for(i in 1:length(visits)) {
      nested_schedule <- nested_schedule %>%
        dplyr::mutate(subject_sched = list(
          tibble::add_row(subject_sched, .before = before_row + i - 1, stage = new_stage, visit = visits[i], week = i - 1L,
                          date = subject_sched$date[before_row + i - 1] - lubridate::weeks(offset_wks) + lubridate::weeks(i),
                          visit_completed = completed, date_estimated = T)))
    }
    if(incl_dbpcfc) {
      nested_schedule <- nested_schedule %>%
        dplyr::mutate(subject_sched = list(
          tibble::add_row(subject_sched, .after = before_row + length(visits) - 1, stage = new_stage, 
                          visit = paste0("dbpcfc_", c("start", "end"), "_window"),
                          week = NA_integer_, visit_completed = completed, date_estimated = T,
                          date = c(subject_sched$date[before_row + length(visits) - 1] + lubridate::weeks(1),
                                   subject_sched$date[before_row + length(visits)] - lubridate::weeks(1)))))
    }
  }  
  
  new_schedule <- nested_schedule %>% 
    dplyr::select(-any_of("before_row")) %>%
    tidyr::unnest(subject_sched) %>% 
    dplyr::ungroup()
  
  return(new_schedule)
}

#add individual dbpcfc visits
interpolate_dbpcfc <- function(sched_tbl, n_dbpcfc=4, groups = c("subject_id", "xolair_freq", "stage"), 
                               visit_var = "visit", week_var = "week") {
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages, lubridate, and glue
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
    require(glue, quietly = T)
    require(lubridate, quietly = T)
    #tidyxl package
    while(!require("tidyxl", quietly = T)) install.packages("tidyxl")
  })

  stopifnot(all(c(visit_var, week_var) %in% colnames(sched_tbl)))
  initial_colnames <- colnames(sched_tbl)
  if(!all(groups %in% colnames(sched_tbl)) & dplyr::is_grouped_df(sched_tbl)) groups <- dplyr::group_vars(sched_tbl)
  initial_coltypes <- purrr::flatten(dplyr::summarize(sched_tbl, dplyr::across(dplyr::all_of(c(groups, "week", "visit")), typeof)))
  
  #rowwise grouped tibble
  sched_nested <- sched_tbl %>% 
    dplyr::mutate(dplyr::across(week, ~if(is.character(.x)) {as.integer(readr::parse_number(.x))} else {as.integer(.x)})) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(groups), ~ordered(.x, levels = unique(.x)))) %>% 
    dplyr::nest_by(dplyr::across(dplyr::all_of(groups)), .key = "schedule") %>%
    dplyr::mutate(dplyr::across(schedule, list(has_dbpcfc = ~purrr::some(.x$visit, ~stringr::str_detect(.x, "dbpcfc.*start")))))
  
  #abort if no groups have dbpcfc window
  if(purrr::every(sched_nested$schedule_has_dbpcfc, purrr::negate(identity))) {
    message("no dbpcfc windows found")
    return(sched_tbl)
  }
  
  #interpolation on groups with dbpcfc window
  result_tbl <- sched_nested %>% 
    dplyr::filter(schedule_has_dbpcfc) %>%
    dplyr::mutate(dplyr::across(schedule, 
                                list(stable_vars = ~dplyr::distinct(.x, dplyr::across(tidyselect::vars_select_helpers$where(~length(unique(.x))==1))),
                                     after_row = ~purrr::detect_index(.x$visit, ~stringr::str_detect(.x, "dbpcfc.*start")),
                                     start_week = ~as.integer(max(.x$week, na.rm = T) + 1), end_week = ~as.integer(max(.x$week, na.rm = T) + 1 + 4)
                                ))) %>%
    dplyr::mutate(schedule = list(dplyr::mutate(schedule,
                                                week = dplyr::case_when(!is.na(week) ~ week,
                                                                        !is.na(date) ~ as.integer(difftime(date, .data$date[1], units = "weeks")),
                                                                        stringr::str_detect(visit, "dbpcfc.*start") ~ schedule_start_week,
                                                                        stringr::str_detect(visit, "dbpcfc.*end") ~ schedule_end_week))
    )) %>%
    dplyr::mutate(schedule = list(tibble::add_row(schedule, .after = schedule_after_row, 
                                                  visit = paste0("dbpcfc_", 1:n_dbpcfc), schedule_stable_vars, 
                                                  week = seq.int(from = schedule_start_week, by = 1, length.out = n_dbpcfc))
    )) %>%
    dplyr::mutate(schedule = list(dplyr::mutate(schedule,
                                                date_estimated = date_estimated | (is.na(date) & stringr::str_detect(visit, "dbpcfc_\\d")),
                                                date = dplyr::if_else(is.na(date) & date_estimated, .data$date[1] + lubridate::weeks(week), date))
    )) %>%
    tidyr::unnest(schedule) %>% 
    dplyr::ungroup()
  
  #extract and re-append any groups without dbpcfc window
  if(purrr::some(sched_nested$schedule_has_dbpcfc, purrr::negate(identity))) {
    message(glue::glue("some {groups} groups have no dbpcfc window: {dplyr::filter(sched_nested, !schedule_has_dbpcfc)[[groups]]}"))
    result_tbl <- dplyr::filter(sched_nested, !schedule_has_dbpcfc) %>%
      tidyr::unnest(schedule) %>% 
      dplyr::ungroup() %>%
      dplyr::full_join(result_tbl, by = dplyr::all_of(initial_colnames))
  }
  
  result_tbl <- result_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(groups, "week", "visit")), 
                                ~as.vector(.x, mode = purrr::pluck(initial_coltypes, dplyr::cur_column())))) %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.character), 
                                ~stringr::str_replace_all(.x, "^(\\d+)$", paste0(dplyr::cur_column(), "_" , "\\1")))) %>%
    dplyr::select(dplyr::all_of(initial_colnames))
  
  return(result_tbl)
}

#add cols for visit types and hours
add_info_cols <- function(sched_tbl, incl_hours = T, incl_vtype = T) {
  #installs (if needed) and loads packages
  suppressPackageStartupMessages({
    #tidyverse packages and lubridate
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  }) 
  
  rw_sched <- dplyr::rowwise(sched_tbl) %>%
    dplyr::mutate(entry_type = factor(dplyr::case_when(stringr::str_detect(visit, "\\d$") ~ "visit", 
                                                       stringr::str_detect(visit, "dbpcfc.*window") ~ "dbpcfc_window"))) %>%
    dplyr::mutate(xolair = dplyr::case_when(entry_type != "visit" ~ NA, 
                                            stage=="screening" ~ F, T ~ week %% readr::parse_number(xolair_freq) == 0)) %>%
    dplyr::mutate(dbpcfc = dplyr::case_when(entry_type != "visit" ~ NA,
                                            T ~ stringr::str_detect(visit, "dbpcfc.*\\d+"))) %>%
    dplyr::mutate(oit = dplyr::if_else(entry_type != "visit", NA, FALSE), oit_type = NA) %>%
    dplyr::mutate(other = !(xolair | dbpcfc | oit) & !is.na(date), .after = oit_type)
  
  if(incl_hours) {
    rw_sched <- rw_sched %>%
      dplyr::mutate(hours = sum(xolair * 1.5, dbpcfc * 6, oit * 3, other * 2, 0, na.rm = T)) 
  }
  if(incl_vtype) {
    rw_sched <- rw_sched  %>%
      dplyr::mutate(visit_type = dplyr::case_when(other & !xolair & !dbpcfc & !oit ~ "other",
                                                  !other & !xolair & dbpcfc & !oit ~ "dbpcfc",
                                                  !other & !xolair & !dbpcfc & oit ~ "oit",
                                                  !other & xolair & !dbpcfc & !oit ~ "xolair",
                                                  !other & xolair & dbpcfc & !oit ~ "xolair/dbpcfc",
                                                  !other & xolair & !dbpcfc & oit ~ "xolair/oit",
                                                  TRUE ~ NA_character_)) 
  }
  
  return(dplyr::ungroup(rw_sched))
}
