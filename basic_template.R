rm(list = ls())

source("https://raw.githubusercontent.com/dmpyle/dp_templates/master/dmp_tidyverse.R")

#### Designate filepaths and load files####
user.path <- Sys.getenv("HOME")

#checking that local dropbox folder is in user directory or searching 1 layer of subdirectories to find it
if(!dir.exists(local.db.path <- file.path(user.path, "Dropbox (Partners HealthCare)"))) {
  for (subdir in list.dirs("~", recursive = F)) {
    if(dir.exists(file.path(subdir, "Dropbox (Partners HealthCare)"))) {
      local.db.path <- file.path(subdir, "Dropbox (Partners HealthCare)")
      break
    }
  }
  rm(subdir)
}

#### *** ####
