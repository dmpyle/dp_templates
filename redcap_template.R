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

# attaches other needed packages
library(naniar, pos = "package:tidyverse")
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
       # add usernames and API_token paths here in the format below (include the comma after each line)
       # user123 = {secret <- readLines('/Users/user123/Documents/.../myAPItoken.txt', warn = FALSE)},
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

#### RedcapAPI Data Export ####
options(redcap_api_url = 'https://redcap.partners.org/redcap/api/')
rconn <- redcapConnection(token = secret)
# export metadata bundle - automatically saved in options("redcap_bundle")
exportBundle(rconn, users = F, version = F, return_object = F)
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

raw_data <- exportRecords(rconn, factors = T, checkboxLabels = T, labels = F) #deprecated soon (v2.7.0)
raw_data_typed <- exportRecordsTyped(rconn) #coming soon (v2.7.0)
export.date <- Sys.Date()
# for all data, omit the forms and events arguments
# for specific forms, add the argument 'forms = c("form_1", "form_2", etc.)'
# for specific events, add the argument 'events = c("unique_event_name_1", "unique_event_name_1", etc.)'
# to view the event and form options, run > View(redcap_data_map)

#### Accessing without redcapAPI package #####
redcap_ <- list(
  meta_data = httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                       body = list(token=secret, format='json', returnFormat='json', content='metadata'))) %>% 
    enframe(name = NULL) %>% unnest_wider(value),
  instruments = httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                         body = list(token=secret, format='json', returnFormat='json', content='instrument'))) %>% 
    enframe(name = NULL) %>% unnest_wider(value),
  events = httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                    body = list(token=secret, format='json', returnFormat='json', content='event'))) %>% 
    enframe(name = NULL) %>% unnest_wider(value),
  arms = httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                  body = list(token=secret, format='json', returnFormat='json', content='arm'))) %>% 
    enframe(name = NULL) %>% unnest_wider(value),
  mappings = httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                      body = list(token=secret, format='json', returnFormat='json', content='formEventMapping'))) %>% 
    enframe(name = NULL) %>% unnest_wider(value),
  fieldnames = httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                        body = list(token=secret, format='json', returnFormat='json', content='exportFieldNames'))) %>% 
    enframe(name = NULL) %>% unnest_wider(value)
) %>%
  map(replace_with_na_all, ~.x == "")

raw_data_ <- httr::content(httr::POST(url = 'https://redcap.partners.org/redcap/api/', encode = "form", 
                                      body = list(token=secret, format='json', returnFormat='json', content='record', action='export', 
                                                  type='flat', csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', 
                                                  exportCheckboxLabel='false', exportSurveyFields='true', exportDataAccessGroups='false'))) %>% 
  enframe(name = NULL) %>% unnest_wider(value) #equivalent to exportRecords(rconn, factors = T, checkboxLabels = F, labels = F)
# changable options:
#   rawOrLabel = 'raw'(default) -or- 'label' --- raw or labeled values for select choices
#   rawOrLabelHeaders = 'raw'(default) -or- 'label' --- include labels for checkbox options in col name
#   exportCheckboxLabel = 'false'(default) -or- 'true' --- blank/1 vs "(Un)checked"
#   exportSurveyFields = 'false'(default) -or- 'true' --- include survey timestamp/identifier fields if present
#   exportDataAccessGroups = 'false'(default) -or- 'true' --- inclued data access group field if present

#### Modify Bundle and Metadata ####
redcap$data_dictionary <- redcap$meta_data %>%
  mutate(select_choices = case_when(field_type == "calc" ~ NA_character_,
                                    field_type == "yesno" ~ "1, Yes | 0, No", 
                                    field_type == "truefalse" ~ "1, TRUE | 0, FALSE",
                                    TRUE ~ select_choices_or_calculations),
         calculations = if_else(field_type == "calc", select_choices_or_calculations, NA_character_), 
         text_validation_type = if_else(field_type != "slider", text_validation_type_or_show_slider_number, NA_character_),
         show_slider_number = if_else(field_type == "slider", text_validation_type_or_show_slider_number, NA_character_), 
         select_choices_or_calculations = NULL, text_validation_type_or_show_slider_number = NULL,
         .after = field_note) %>%
  mutate(var_type = case_when(field_type %in% c("radio", "dropdown", "truefalse", "yesno") ~ "single_select",
                              field_type == "checkbox" ~ "multi_select",
                              TRUE ~ field_type), .after = section_header) %>%
  mutate() %>%
  mutate(field_type = case_when(!is.na(text_validation_min) & !is.na(text_validation_max) ~ 
                                  paste0(text_validation_type, " (", text_validation_min, " - ", text_validation_max, ")"),
                                !is.na(text_validation_min) & is.na(text_validation_max) ~
                                  paste0(text_validation_type, " (> ", text_validation_min, ")"),
                                is.na(text_validation_min) & !is.na(text_validation_max) ~
                                  paste0(text_validation_type, " (< ", text_validation_max, ")"),
                                !is.na(text_validation_type) ~ text_validation_type,
                                TRUE ~ field_type), text_validation_type = NULL, text_validation_min = NULL, text_validation_max = NULL) %>%
  separate_rows(select_choices, sep = "\\s\\|\\s") %>%
  separate(select_choices, into = c("id", "value"), sep = "(?<=\\w),\\s?", fill = "right", extra = "merge") %>% 
  mutate(field_name = if_else(var_type == "multi_select", paste(field_name, id, sep = "___"), field_name)) %>%
  nest(select_choices = c(id, value)) %>%
  select(form_name, field_name, var_type, field_type, field_label, select_choices, calculations, show_slider_number, branching_logic, field_note, field_annotation, section_header, required_field, identifier, matrix_group_name, matrix_ranking, question_number, custom_alignment)
  
redcap$choices <- redcap$data_dictionary$select_choices %>%
  map(deframe)
# map choices from selectable choice variables and save to bundle
# to see choices or use in later code: redcap$choices$[var_name]
