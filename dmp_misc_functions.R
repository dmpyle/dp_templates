
####Function definitions####
str_to_display <- compose(str_to_title, ~str_replace_all(.x, c('_' = ' ', '^n(?!\\w)' = '# of')))

sigdig <- function(num, sig) {format(num, digits = sig, nsmall = sig)}

gcf <- function(a, b) {
  #Greatest Common Factor
  while(b != 0) {
    new_a <- b
    new_b <- a %% b
    a <- new_a
    b <- new_b
  }
  return(a)
}

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

createifnot.path <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    message("designated path does not exist, created new folder in the specified location")
  }
  return(path)
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
