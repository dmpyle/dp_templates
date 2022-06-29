#### App Info ####
# Created: 10/2021 by David Pyle
# Purpose: template shiny app to visualize effort/workload projections

#### Load Libraries ####
suppressPackageStartupMessages({
    while(!require(tidyverse, quietly = T)) install.packages("tidyverse") #loads readr, tidyr, dplyr, tibble, stringr, forcats, purrr, ggplot2
    library(magrittr, quietly = T, pos = "package:tidyverse")
    library(glue, quietly = T, pos = "package:tidyverse")
    library(lubridate, quietly = T, pos = "package:tidyverse")
}) # install (if needed) and load tidyverse package and supplements
suppressPackageStartupMessages({
    while(!require(shiny, quietly = T)) install.packages("shiny")
    while(!require(rhandsontable, quietly = T)) install.packages("rhandsontable")
    while(!require(DT, quietly = T)) install.packages("DT")
    while(!require(timevis, quietly = T)) install.packages("timevis")
    while(!require(shinyWidgets, quietly = T)) install.packages("shinyWidgets")
}) # install (if needed) and load shiny package and supplements

#### Server Function ####

shinyServer(function(input, output) {
    values <- reactiveValues()
    
    str_to_snake <- compose(~str_replace_all(.x, c('\\s' = '_')), str_to_lower)
    str_to_display <- compose(str_to_title, ~str_replace_all(.x, c('_' = ' ', '^n(?!\\w)' = '# of')))
    
    values$current_template <- tibble(hours = numeric(1))
    values$instances <- character(1)
    
    observe({
        values$currentDF <- nest_join(tibble(instance = values$instances), values$current_template, by = character(), name = "template") %>%
            unnest(template) 
        values$filter_vars <- colnames(values$current_template)[colnames(values$current_template) != "hours"]
    })
    
    output$ui_filters <- renderUI(expr = {
        wellPanel(h4("Filter Options"),
                        map(values$filter_vars,
                            ~switch(typeof(pull(values$current_template, .x)),
                                   "integer" = {sliderInput(inputId = str_to_snake(.x), label = str_to_display(.x), 
                                                            min = min(pull(values$current_template, .x), na.rm = T), 
                                                            max = max(pull(values$current_template, .x), na.rm = T),
                                                            value = range(pull(values$current_template, .x), finite = T))},
                                   "double" = {numericRangeInput(inputId = str_to_snake(.x), label = str_to_display(.x), value = numeric(),
                                                                 min = min(pull(values$current_template, .x), na.rm = T), 
                                                                 max = max(pull(values$current_template, .x), na.rm = T))}, 
                                   "character" = {checkboxGroupInput(inputId = str_to_snake(.x), label = str_to_display(.x), 
                                                                     choices = unique(pull(values$current_template, .x)),
                                                                     selected = unique(pull(values$current_template, .x)))},
                                   "factor" = {checkboxGroupInput(inputId = str_to_snake(.x), label = str_to_display(.x), 
                                                                  choices = unique(pull(values$current_template, .x)),
                                                                  selected = unique(pull(values$current_template, .x)))},
                                   "logical" = {checkboxInput(inputId = str_to_snake(.x), label = str_to_display(.x))}
                                   )
                            ))
        })
    
    ## Print hands on table
    output$hot_template <- renderRHandsontable(expr = {
        rhandsontable(data = values$current_template, useTypes = input$hot_mode, search = T) %>% 
            hot_table(stretchH = "all") %>%
            hot_rows(fixedRowsTop = 1)
        })#output$h_o_table
    
    ## Print filtered table
    output$filteredDF <- renderDataTable(expr = {
        values$filteredDF <- values$currentDF 
        }, options = list(scrollX = T)
        )#output$filteredDF
    
    ## Print summary table
    output$summaryDF <- renderDataTable(expr = {
        values$summaryDF <- values$currentDF %>% 
            group_by(across(any_of(values$filter_vars))) %>%
            summarise(total_hours = sum(hours, na.rm = T), .groups = "drop_last")
    }, options = list(scrollX = T)
    )#output$summaryDF
    
    ## Hands on table inputs
    observeEvent(input$hot_template, {
        values$previous_template <- values$current_template
        values$current_template <- hot_to_r(input$hot_template)
        
    })#input$h_o_table
    
    # output$ui_newcol <- renderUI(expr = {
    #     wellPanel(
    #         h4("Add a template column"),
    #         selectInput("newcol_type", "Column Type", choices = c("integer", "real number", "text", "checkbox")),
    #         div(class='row', 
    #             textInput(inputId = "newcol_name", label = "Name", placeholder = glue("newcol_{ncol(values$currentDF)+1}")),
    #             switch(input$newcol_type(pull(values$current_template, .x)),
    #                    "integer" = {numericInput(inputId = "newcol_default", label = "Default Value", value = integer(), step = 1)},
    #                    "real number" = {numericInput(inputId = "newcol_default", label = "Default Value", value = numeric())},
    #                    "text" = {textInput(inputId = "newcol_default", label = "Default Value", value = character())},
    #                    "checkbox" = {checkboxInput(inputId = "newcol_default", label = "Default Value")})),
    #         actionButton("add_newcol", "Add Column")
    #         )
    # })
        
})
