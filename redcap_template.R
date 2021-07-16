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
library(redcapAPI)

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
redcap_data_map <- getOption("redcap_bundle")$mappings

raw_data_ <- exportRecords(rconn, factors = T, checkboxLabels = T, labels = F)
export.date <- Sys.Date()

# for all data, omit the forms and events arguments
# for specific forms, use the argument 'forms = c("form_1", "form_2", etc.)'
# for specific events, use the argument 'events = c("unique_event_name_1", "unique_event_name_1", etc.)'
# to view the event and form options, run > View(redcap_data_map)

#### ^^^^ Edited ^^^^ #####
