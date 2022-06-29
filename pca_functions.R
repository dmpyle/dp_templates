
#### Misc Functions ####
sigdig <- function(num, sig) {format(num, digits = sig, nsmall = sig)}

formulator <- function(lhs, rhs = "1", type = c("each", "sum", "product")) {
  #Makes a formula object from the first 2 arguments
  #"lhs" argument is coerced to string and forms the left side of the formula
  #"rhs" argument can be a string or list of strings (or anything coercible to string)
  #"type" argument determines return format if rhs is a list:
  #"each" returns a list of formulas with lhs ~ rhs(i) for each component
  #"sum" combines rhs list and returns lhs ~ rhs(i) + rhs(i+1) + ...
  #"product" combines rhs list and returns lhs ~ rhs(i) * rhs(i+1) * ...
  
  #installs (if needed) and loads tidyverse packages
  suppressPackageStartupMessages({  
    while(!require("tidyverse", quietly = T)) install.packages("tidyverse")
  })
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

#### PCA Functions ####
pc_vec <- function(len) {
  #creates a vector of length 'len' (or length(len) if non-numeric) with sequential 'PC#' strings
  if (is.numeric(len)) return(paste0(rep("PC", len), seq_len(len)))
  else return(paste0(rep("PC", length(len)), seq_along(len)))
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

tidy_prcomp <- function(prcomp, metadata, mode = c("all", "variance", "loading", "samples")) {
  mode <- match.arg(mode)
  if (mode == "all") mode <- c("variance", "loading", "samples")  
  n_pcs <- length(pluck(prcomp, "sdev"))
  metadata <- as_tibble(metadata)
  tidy_pcs <- list(variance = NULL, loading = NULL, samples = NULL)
  
  if(has_element(mode, "variance")) {
    tidy_pcs$variance <- tidy(prcomp, "pcs") %>%
      mutate(PC = ordered(paste0("PC", PC), levels = pc_vec(n_pcs)))
  }
  if(has_element(mode, "loading")) {
    tidy_pcs$loading <- tidy(prcomp, "variables") %>%
      rename("gene" = column, "load" = value) %>%
      mutate(PC = ordered(paste0("PC", PC), levels = pc_vec(n_pcs))) 
  }
  if(has_element(mode, "samples")) {
    tidy_pcs$samples <- augment(prcomp, data = metadata) %>%
      select(-.rownames) %>% 
      rename_all(str_remove, "\\.fitted")
  }
  return(compact(tidy_pcs))
}

modular_pca <- function(dataset, mod_list, center_data = T, scale_data = T) {
  #calculates targeted pcas on specified gene subsets
  mod_list <- as.list(mod_list) #list of character vectors each containing a set of genes
  dataset <- as_tibble(dataset) #data frame containing read data with genes as variables
  #centered/scaled = logicals passed on to prcomp function
  
  gene_count <- map(mod_list, length) %>% flatten_int()
  mod_list %<>% map(~keep(.x, ~.x %in% colnames(tpm_paidup)))
  missing <- map(mod_list, length) %>% flatten_int() %>% subtract(gene_count, .)
  if(sum(missing) > 0) {
    warning(glue("{sum(missing)} genes missing from {as_label(enexpr(dataset))} have been removed from {as_label(enexpr(mod_list))}"))
  }
  mod_list %<>% map(~discard(.x, ~var(tpm_paidup[.x]) == 0))
  constant <- map(mod_list, length) %>% flatten_int() %>% subtract(gene_count-missing, .)
  if(sum(constant) > 0) {
    warning(glue("{sum(constant)} genes with 0 variance in {as_label(enexpr(dataset))} have been removed from {as_label(enexpr(mod_list))}"))
  }
  
  result_list <- map(mod_list, ~prcomp(select(dataset, .), center = center_data, scale. = scale_data))
  
  return(result_list)
  #returns a list of prcomp objects with the same names as the module list
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


multi_tidy_prcomp <- function(prcomp_list, metadata, mode = c("all", "variance", "loading", "samples")) {
  mode <- match.arg(mode)
  if (mode == "all") mode <- c("variance", "loading", "samples")
  n_pcs <- map(prcomp_list, pluck, "sdev") %>% map(length)
  metadata <- as_tibble(metadata)
  tidy_pcs <- list(variance = NULL, loading = NULL, samples = NULL)
  
  if(has_element(mode, "variance")) {
    tidy_pcs$variance <- map(prcomp_list, ~tidy(.x, "pcs")) %>%
      map2(.y = n_pcs, ~mutate(.x, PC = ordered(paste0("PC", PC), levels = pc_vec(.y))))
  }
  if(has_element(mode, "loading")) {
    tidy_pcs$loading <- map(prcomp_list, ~tidy(.x, "variables")) %>%
      map(~rename(.x, "gene" = column, "load" = value)) %>%
      map2(.y = n_pcs, ~mutate(.x, PC = ordered(paste0("PC", PC), levels = pc_vec(.y))))
  }
  if(has_element(mode, "samples")) {
    tidy_pcs$samples <- map(prcomp_list, ~augment(.x, data = metadata)) %>%
      map(~select(.x, -.rownames)) %>%
      map(~rename_all(.x, str_remove, "\\.fitted"))
  }
  return(compact(tidy_pcs))
}