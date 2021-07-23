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

#### Assigning file paths and API tokens ####
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
# export metadata bundle - automatically saved in options("redcap_bundle") - to access use getOption("redcap_bundle")
exportBundle(rconn, return_object = F, users = F)
# bundle contains a list of these dataframes:
# meta_data: all variables with name, form, type, and various details
# instruments: all forms with name and label
# events: all events in project with name, arm number, and unique name
# arms: all arms with name, number
# mappings: map of arm numbers, unique events, and forms
redcap_meta_data <- getOption("redcap_bundle")$meta_data 
redcap_data_map <- getOption("redcap_bundle")$mappings %>%
  left_join(select(getOption("redcap_bundle")$meta_data, form = form_name, field_name), by = "form")

select(getOption("redcap_bundle")$meta_data, form = form_name, field_name)

as_tibble(getOption("redcap_bundle")$meta_data) %>%
  select(form_name, field_name, field_type, select_choices_or_calculations) %>%
  mutate(single_select_choices = if_else(field_type %in% c("radio", "dropdown"), select_choices_or_calculations, NA_character_),
         multiple_select_choices = if_else(field_type == "checkbox", select_choices_or_calculations, NA_character_),
         calculation = if_else(field_type %in% c("calc"), select_choices_or_calculations, NA_character_), 
         select_choices_or_calculations = NULL) %>%
  separate_rows(choices, sep = "\\s\\|\\s") %>%  
  separate(choices, into = c("choice_num", "choice_value"), sep = "(?<=\\d),\\s?", fill = "right", extra = "merge")
  

as_tibble(getOption("redcap_bundle")$meta_data) %>%
  mutate(checkbox_choices = if_else(field_type == "checkbox", select_choices_or_calculations, NA_character_)) %>% 
  separate_rows(checkbox_choices, sep = "\\s\\|\\s") %>%
  separate(checkbox_choices, c("choice_id", NULL), sep = "(?<=\\d),\\s?", fill = "right", extra = "merge") %>%
  unite(col = field_name, field_name, choice_id, sep = "___", na.rm = T) %>%
  select(form = form_name, variable = field_name) %>% 
  chop(variable) %>%
  inner_join(x = getOption("redcap_bundle")$mappings)
mutate(across(vars, ~map(.x, append, paste0(form_name, "_complete"))))


as_tibble(getOption("redcap_bundle")$meta_data) %>%
  mutate(select_choices = if_else(field_type %in% c("radio", "checkbox", "dropdown"), select_choices_or_calculations, NA_character_),
         calculation = if_else(field_type %in% c("calc"), select_choices_or_calculations, NA_character_)) %>%
  separate_rows(select_choices, sep = "\\s\\|\\s") %>%  
  separate(select_choices, into = c("id", "value"), sep = "(?<=\\d),\\s?", fill = "right", extra = "merge") %>%
  mutate(field_name = if_else(field_type == "checkbox", paste(field_name, id, sep = "___"), field_name)) %>%
  nest(select_choices = c(id, value)) %>%
  mutate(select_choices = set_names(select_choices, field_name))

raw_data_ <- exportRecords(rconn, factors = T, checkboxLabels = T, labels = F)
export.date <- Sys.Date()

# for all data, omit the forms and events arguments
# for specific forms, add the argument 'forms = c("form_1", "form_2", etc.)'
# for specific events, add the argument 'events = c("unique_event_name_1", "unique_event_name_1", etc.)'
# to view the event and form options, run > View(redcap_data_map)

#### ^^^^ Edited ^^^^ #####
