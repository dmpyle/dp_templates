#### Script to install and load Tidyverse packages ####

#loads tidyverse packages and installs if not found
while(!requireNamespace("tidyverse", quietly = T)) install.packages("tidyverse")

#attaches tidyverse packages and commonly used supplements
library(tidyverse) #readr, tidyr, dplyr, tibble, stringr, forcats, purrr, ggplot2
library(magrittr)
library(rlang)
library(modelr, pos = "package:tidyverse")
library(glue, pos = "package:tidyverse")
library(lubridate, pos = "package:tidyverse")
library(readxl, pos = "package:tidyverse")
library(broom, pos = "package:tidyverse")
library(reprex, pos = "package:tidyverse")

message(
"Installed (if necessary) and loaded tidyverse packages (ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats), 
 and supplements (magrittr, glue, readxl, broom, modelr, lubridate, reprex, rlang)"
)

