
####Function definitions####
str_to_snake <- compose(~str_replace_all(.x, c('\\s' = '_')), str_to_lower)
str_to_display <- compose(str_to_title, ~str_replace_all(.x, c('_' = ' ', '^n(?!\\w)' = '# of')))

sigdig <- function(num, sig) {format(num, digits = sig, nsmall = sig)}

formulator <- function(lhs, rhs = "1", type = c("each", "sum", "product")) {
  #Makes a formula object from the first 2 arguments
  #"lhs" argument is coerced to string and forms the left side of the formula
  #"rhs" argument can be a string or list of strings (or anything coercible to string)
  #"type" argument determines return format if rhs is a list:
  #"each" returns a list of formulas with lhs ~ rhs(i) for each component
  #"sum" combines rhs list and returns lhs ~ rhs(i) + rhs(i+1) + ...
  #"product" combines rhs list and returns lhs ~ rhs(i) * rhs(i+1) * ...
  
  suppressPackageStartupMessages({  
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  }) #installs (if needed) and loads tidyverse packages
  
  type <- match.arg(type)
  collapse_type <- switch(type,
                          each = NULL,
                          sum = " + ", 
                          product = " * ")
  rhs_list <- map_depth(rhs, .depth = vec_depth(rhs)-1, ~str_c(.x, collapse = collapse_type))
  base <- paste0(as.character(lhs), " ~ ")
  
  formula_list <- paste0(base, rhs_list) %>%
    map(as.formula) %>%
    set_names(nm = names(rhs_list))
  
  if(length(formula_list) > 1) return(formula_list)
  else return(formula_list[[1]])
}

list2df <- function(list, id_col = "id") {
  #converts a list to a tibble with list names as the first column named with the "id_col" argument
  suppressPackageStartupMessages({  
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  }) #installs (if needed) and loads tidyverse packages
  
  enframe(list, name = id_col, value = "value") %>%
    unnest(cols = "value")
}

df2list <- function(df, id_col = colnames(df)[[1]]) {
  #converts a data frame/tibble to a list with the column indicated in "id_col" as names (defaults to first column)
  suppressPackageStartupMessages({  
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  }) #installs (if needed) and loads tidyverse packages
  
  nest(df, data = -id_col) %>%
    deframe()
}

glm_stats <- function(mymodel, type = c("add", "drop"), test = c("LRT", "Rao", "Chisq", "F")) {
  #univariate additions to a null model or deletions of each term from a full model
  #provides wald statistic and any of c("LRT", "Rao", "Chisq", "F") with p values
  #provides summary statistics (mean, sd) for predictor variables in total and grouped by response variable
  suppressPackageStartupMessages({  
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
    while(!require("tidymodels", quietly = T)) install.packages("tidymodels")
  }) #installs (if needed) and loads tidyverse and tidymodels packages
  
  type <- match.arg(type)
  test <- match.arg(test, several.ok = T) %>% set_names(nm = str_to_lower(.))
  response_var <- pluck(all.vars(formula(mymodel)), attr(terms(mymodel), "response"))
  term_list <- attr(terms(mymodel), "term.labels")
  
  sum_stats <- rbind(pluck(mymodel, "data"), mutate(pluck(mymodel, "data"), !!response_var := "total")) %>%
    group_by_at(.vars = response_var) %>%
    summarize_if(is.numeric, list(~mean(.), ~sd(.))) %>%
    pivot_longer(-matches(response_var), names_to = c("term", ".value"), names_sep = "[[:punct:]]") %>%
    pivot_wider(names_from = response_var, values_from = c("mean", "sd")) 
  
  result_tibble <- inner_join(tibble(term = term_list), sum_stats, by = "term") %>%
    left_join(tidy(mymodel, exponentiate = T), by = "term") %>%
    rename("coefficient" = estimate, "wald.statistic" = statistic, "wald.p.value" = p.value)
  
  if (!mymodel$converged) warning(glue("model {as_label(enexpr(mymodel))} did not converge. Use caution when interpreting statistics"))
  
  if (is_empty(term_list)) {
    warning(glue("model {as_label(enexpr(mymodel))} has no predictor variables. No statistics computed"))
    return(result_tibble)
  }
  
  test_result <- switch(type,
                        add = map(test, ~suppressWarnings(tidy(add1(update(mymodel, . ~ 1), scope = term_list, test = .x)))),
                        drop = map(test, ~suppressWarnings(tidy(drop1(mymodel, scope = term_list, test = .x))))
  )  %>% map(~select(.x, -Deviance, -df))
  
  result_tibble <- reduce2(.x = test_result, .y = names(test_result), ~nest_join(..1, ..2, name = ..3, by = "term"), .init = result_tibble) %>%
    unnest(cols = names(test_result), names_sep = ".") %>%
    mutate(term = ordered(term, levels = str_sort(term, numeric = T)))
  
  return(result_tibble)
}

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