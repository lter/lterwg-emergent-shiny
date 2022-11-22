## ------------------------------------------ ##
        # NEON Chemistry Data Wrangling
## ------------------------------------------ ##
# Authors: Dvir Blander, Katrina Newcomer, & Nick Lyon

# Housekeeping ----

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)

# Load data
data_v1 <- read.csv(file = file.path("data", "soilFieldChem.csv"))

# Wrangling ----

# Subset data to desired entries
data_v2 <- data_v1 %>%
  dplyr::filter(nlcdClass %in% c("mixedForest", "deciduousForest",
                "evergreenForest", "grasslandHerbaceous"))



# End ----
