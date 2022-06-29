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

#### UI Definition ####
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Workload Projection Template"),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput("ui_filters"),
            # uiOutput("ui_newcol")
            ),

        mainPanel(
            wellPanel(
                h4("Summary Table"),
                dataTableOutput("summaryDF")
            ),
            wellPanel(
                h4("Filtered Table"),
                dataTableOutput("filteredDF")
            ),
            wellPanel(
                h4("Template"),
                switchInput("hot_mode", label = "Template Mode", value = F, onLabel = "TEST", offLabel = "BUILD"),
                rHandsontableOutput("hot_template")
            )
        )
    )
))
