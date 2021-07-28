rm(list = ls())

#### Loading Libraries ####
while(!requireNamespace("tidyverse", quietly = T)) install.packages("tidyverse")

# attaches tidyverse packages and commonly used supplements
library(tidyverse) #readr, tidyr, dplyr, tibble, stringr, forcats, purrr, ggplot2
library(rlang, pos = "package:tidyverse")
library(magrittr, pos = "package:tidyverse")
library(glue, pos = "package:tidyverse")
library(lubridate, pos = "package:tidyverse")
library(readxl, pos = "package:tidyverse")
library(naniar, pos = "package:tidyverse")

# attaches other needed packages
library(redcapAPI)

# print options setting
options(tibble.print_min = 20, tibble.print_max = 40)

#### Assigning file paths ####
local.docs.path <- file.path("~", "Documents") #user Documents folder
local.db.path <- file.path("~", "Dropbox (Partners HealthCare)") #usual location for dropbox in Shrefflab

#checking that local.db.path exists and if not, searching 1 layer of subdirectories to find it
if(!dir.exists(local.db.path)) {
  for (subdir in list.dirs("~", recursive = F)) {
    if(dir.exists(file.path(subdir, "Dropbox (Partners HealthCare)"))) {
      local.db.path <- file.path(subdir, "Dropbox (Partners HealthCare)")
      break
    }
  }
  rm(subdir)
}

output.path <- file.path(local.db.path, 
                         # insert directory path here
)

if (!dir.exists(output.path)) {stop("Could not set the read/write path. Please check that your designated output folder exists")}

#### User-specific API tokens ####
switch(Sys.info()[['user']],
       # add usernames and API_token paths here
       stop("No API token found. Run the next section of code to find it.")
)

if(!exists("secret")) {
  if(askYesNo(message("Do you have a Redcap API token for this project saved locally as a .txt file?"), 
              prompts = c("yes - select file in next window", "no", "cancel"))) {
    token_path <- file.choose()
    secret <- readLines(token_path, warn = FALSE)
    message(paste(sep = '\n', "Add the following line to the above switch function:", 
                  paste0(Sys.info()[['user']], " = {secret <- readLines('", token_path, "', warn = F)}")))
    rm(token_path)
  } else {
    stop("No Redcap API token found. Please obtain an API token through Redcap and save the token as a .txt file")
  }
}

#### Redcap Data Export ####
options(redcap_api_url = 'https://redcap.partners.org/redcap/api/')
rconn <- redcapConnection(token = secret)
# export metadata bundle - automatically saved in options("redcap_bundle")
exportBundle(rconn, users = F, version = F)
# to access bundle use getOption("redcap_bundle") or save to a variable with argument return_object = T in exportBundle function
redcap <- getOption("redcap_bundle") %>% 
  compact() %>% 
  map(as_tibble)
# bundle contains a list of these dataframes:
# meta_data: all variables with name, form, type, and various details
# instruments: all forms with name and label
# events: all events in project with name, arm number, and unique name
# arms: all arms with name, number
# mappings: map of associations of arms, events, and forms

# map choices from selectable choice variables and save to bundle
redcap$choices <- redcap$meta_data %>%
  mutate(choices = case_when(field_type == "calc" ~ NA_character_,
                             field_type == "yesno" ~ "1, Yes | 0, No", 
                             field_type == "truefalse" ~ "1, TRUE | 0, FALSE",
                             TRUE ~ select_choices_or_calculations)) %>%
  mutate(across(choices, ~map(.x, as_tibble))) %>%
  mutate(across(choices, ~map(.x, separate_rows, value, sep = "\\s\\|\\s"))) %>%
  mutate(across(choices, ~map(.x, separate, value, into = c("id", "value"), sep = "(?<=\\d),\\s?", fill = "right", extra = "merge"))) %>%
  mutate(across(choices, ~map(.x, deframe))) %>% 
  mutate(across(choices, ~set_names(.x, field_name))) %>%
  pull(choices)
# to see choices or use in later code: redcap$choices$[var_name]

redcap$data_dictionary <- redcap$meta_data %>%
  mutate(select_choices = if_else(field_type != "calc", select_choices_or_calculations, NA_character_),
         calculations = if_else(field_type == "calc", select_choices_or_calculations, NA_character_), 
         text_validation_type = if_else(field_type != "slider", text_validation_type_or_show_slider_number, NA_character_),
         show_slider_number = if_else(field_type == "slider", text_validation_type_or_show_slider_number, NA_character_), 
         select_choices_or_calculations = NULL, text_validation_type_or_show_slider_number = NULL,
         .after = field_note) %>%
  mutate(var_type = case_when(field_type %in% c("radio", "dropdown", "truefalse", "yesno") ~ "single_select",
                              field_type == "checkbox" ~ "multi_select",
                              TRUE ~ field_type), .after = section_header) %>%
  mutate(select_choices = case_when(!is.na(select_choices) ~ select_choices,
                                    field_type == "yesno" ~ "1, Yes | 0, No", 
                                    field_type == "truefalse" ~ "1, TRUE | 0, FALSE")) %>%
  mutate(field_type = case_when(is.na(text_validation_type) ~ field_type,
                                is.na(text_validation_min) & is.na(text_validation_max) ~ text_validation_type,
                                !is.na(text_validation_min) & is.na(text_validation_max) ~ 
                                  paste0(text_validation_type, " (> ", text_validation_min, ")"),
                                is.na(text_validation_min) & !is.na(text_validation_max) ~ 
                                  paste0(text_validation_type, " (< ", text_validation_max, ")"),
                                TRUE ~ paste0(text_validation_type, " (", text_validation_min, " - ", text_validation_max, ")"))) %>%
  separate_rows(select_choices, sep = "\\s\\|\\s") %>%
  separate(select_choices, into = c("id", "value"), sep = "(?<=\\d),\\s?", fill = "right") %>% 
  mutate(field_name = if_else(var_type == "multi_select", paste(field_name, id, sep = "___"), field_name)) %>%
  nest(select_choices = c(id, value)) %>%
  select(form = form_name, field_name, var_type, field_type, field_label, select_choices, calculations, branching_logic, 
         field_note, field_annotation)
  
raw_data_ <- exportRecords(rconn, factors = T, checkboxLabels = T, labels = F)
export.date <- Sys.Date()

# for all data, omit the forms and events arguments
# for specific forms, add the argument 'forms = c("form_1", "form_2", etc.)'
# for specific events, add the argument 'events = c("unique_event_name_1", "unique_event_name_1", etc.)'
# to view the event and form options, run > View(redcap_data_map)

#### ^^^^ Edited ^^^^ #####
