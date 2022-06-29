rm(list = ls())

source("dmp_tidyverse.R")
library(rdrop2)

#### Designate filepaths and load files####
user.path <- switch(.Platform$OS.type, unix = {"~"}, 
                    windows = {file.path("C:/Users", Sys.info()[["user"]])},
                    Sys.getenv("HOME"))

local.db.path <- file.path(user.path, "Dropbox (Partners HealthCare)")
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

dbpath.projects <- file.path("Projects")

#### Connect to dropbox and download/update source file ####

#auth if first time or load user's db token
if(file.exists(file.path(getwd(), paste0(Sys.info()[["user"]], "_db_token.rds")))) {
  db_token <- readRDS(file.path(getwd(), paste0(Sys.info()[["user"]], "_db_token.rds")))
} else {
  #enter dropbox credentials into browser and allow sharing to rdrop2
  db_token <- rdrop2::drop_auth(new_user = T, cache = F)  
  #save dropbox token to project folder for future use
  saveRDS(db_token, file.path(getwd(), paste0(Sys.info()[["user"]], "_db_token.rds")))
}

# drop_download(file.path( ), dtoken = db_token)