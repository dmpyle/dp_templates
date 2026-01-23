####Function definitions####
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

model_stats <- function(mymodel, type = c("add", "drop"), test = c("LRT", "Rao", "Chisq", "F")) {
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

multi_model_stats <- function(model_list, type = c("add", "drop"), test = c("LRT", "Rao", "Chisq", "F")) {
  #univariate additions to a null model or deletions of each term from a full model
  #provides wald statistic, and either LRT, Rao, F, or Chisq with p values
  #provides summary statistics (mean, sd) for predictor variables in total and grouped by response variable
  type <- match.arg(type)
  test <- match.arg(test) %>% set_names(nm = str_to_lower(.))
  model_list <- as.list(model_list)
  response_vars <- map(model_list, ~pluck(all.vars(formula(.x)), attr(terms(.x), "response")))
  term_lists <- map(model_list, ~attr(terms(.x), "term.labels"))
  empty_list <- list_along(term_lists) %>% set_names(nm = names(term_lists))
  
  sum_stats <- model_list %>% map(~rbind(pluck(.x, "data"), mutate(pluck(.x, "data"), outcome = "total"))) %>%
    map2(.y = response_vars, ~group_by_at(.x, .vars = .y)) %>%
    map(~summarize_if(.x, is.numeric, list(~mean(.), ~sd(.)))) %>%
    map2(.y = response_vars, ~pivot_longer(.x, -matches(.y), names_to = c("term", ".value"), names_sep = "[[:punct:]]")) %>%
    map2(.y = response_vars, ~pivot_wider(.x, names_from = .y, values_from = c("mean", "sd")))
  
  result_list <- map(term_lists, ~tibble(term = .x)) %>%
    map2(.y = sum_stats, ~inner_join(.x, .y, by = "term")) %>%
    map2(.y = map(model_list, ~tidy(.x, exponentiate = T)), ~left_join(.x, .y, by = "term")) %>%
    map(~rename(.x, "coefficient" = estimate, "wald.statistic" = statistic, "wald.p.value" = p.value))
  
  iwalk(model_list, ~if(!pluck(.x, "converged")) warning(glue("model '{.y}' did not converge. Use caution when interpreting statistics")))
  iwalk(term_lists, ~if(is_empty(.x)) warning(glue("model '{.y}' has no predictor variables. Statistics not computed")))
  
  model_list <- discard(model_list, ~is_empty(attr(terms(.x), "term.labels")))
  term_lists <- compact(term_lists)
  
  test_results <- switch(type,
                         add = map2(model_list, term_lists, ~suppressWarnings(tidy(add1(update(.x, . ~1), scope = .y, test = test)))),
                         drop = map2(model_list, term_lists, ~suppressWarnings(tidy(drop1(.x, scope = .y, test = test))))
  ) %>% map(~select(.x, -Deviance, -df)) 
  
  result_list <- list_modify(empty_list, !!!test_results) %>% 
    map_if(is.null, ~tibble(term = character(0))) %>%
    map2(.y = result_list, ~nest_join(.y, .x, by = "term", name = names(test))) %>%
    map(~unnest(.x, cols = names(test), names_sep = "."))
  
  return(result_list)
}