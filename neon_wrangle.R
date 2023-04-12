## ------------------------------------------ ##
        # NEON Chemistry Data Wrangling
## ------------------------------------------ ##
# Authors: Dvir Blander, Katrina Newcomer, & Nick Lyon

# Housekeeping ----

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("tidyverse")
library(tidyverse)

# Load data
data_v1 <- read.csv(file = file.path("data", "soilFieldChem.csv"))

# Wrangling ----

# Streamline data to reduce processing needs of app
data_v2 <- data_v1 %>%
  # Change format of NLCD Classes
  dplyr::mutate(nlcdClass = dplyr::case_when(
    nlcdClass == "cultivatedCrops" ~ "Cultivated Crops",
    nlcdClass == "deciduousForest" ~ "Deciduous Forest",
    nlcdClass == "dwarfScrub" ~ "Dwarf Scrub",
    nlcdClass == "emergentHerbaceousWetlands" ~ "Emergent Herbaceous Wetlands",
    nlcdClass == "evergreenForest" ~ "Evergreen Forest",
    nlcdClass == "grasslandHerbaceous" ~ "Grassland Herbaceous",
    nlcdClass == "mixedForest" ~ "Mixed Forest",
    nlcdClass == "pastureHay" ~ "Pasture Hay",
    nlcdClass == "sedgeHerbaceous" ~ "Sedge Herbaceous",
    nlcdClass == "shrubScrub" ~ "Shrub Scrub",
    nlcdClass == "woodyWetlands" ~ "Woody Wetlands",
    TRUE ~ nlcdClass)) %>%
  # Drop impossible coordinates
  dplyr::filter(abs(Longitude) <= 180 & abs(Latitude) <= 90) %>%
  # Fill NAs in other wanted columns with something more descriptive
  dplyr::mutate(
    siteID = ifelse(is.na(siteID), yes = "Not Recorded", no = siteID),
    biophysicalCriteria = ifelse(is.na(biophysicalCriteria), 
                                 yes = "Not Recorded", 
                                 no = biophysicalCriteria),
    sampleTiming = ifelse(is.na(sampleTiming), 
                          yes = "Not Recorded", 
                          no = sampleTiming)) %>%
  # Coalesce seemingly duplicate columns
  dplyr::mutate(
    soilMoisture = dplyr::coalesce(soilMoisture, soilMoisture.1),
    dryMassFraction = dplyr::coalesce(dryMassFraction.x,
                                      dryMassFraction.y),
    collectDate = dplyr::coalesce(collectDate.x, collectDate.y)
  ) %>%
  # Move these columns around
  dplyr::relocate(dryMassFraction, .after = dryMassFraction.x) %>%
  dplyr::relocate(collectDate, .after = collectDate.x) %>%
  # Drop unwanted columns as well
  dplyr::select(-X, -plotID.x, -namedLocation,
                -dplyr::starts_with("coreCoordinate"),
                -subplotID, -geodeticDatum, -collectDate.x,
                -dplyr::ends_with("Uncertainty"),
                -samplingProtocolVersion.x, -collectedBy,
                -nTransBoutType.x, -eventID,
                -sampleID, -sampleCode, -toxicodendronPossible,
                -horizonDetails, -biomassID, -remarks,
                -dataQF, -publicationDate.x,
                -moistureSampleID, -dplyr::starts_with("oven"),
                -soilMoisture.1, -smRemarks, -smMeasuredBy,
                -smDataQF, -publicationDate.y, -cnSampleID,
                -dplyr::starts_with("bgc"), -cnDataQF,
                -cnLaboratoryName, -cnRemarks, -cnTestMethod,
                -isoRemarks, -isoInstrument, -plotID.y,
                -dryMassFraction.x, -dryMassFraction.y,
                -collectDate.y,
                -dplyr::starts_with("genetic")) %>%
  # Strip collection year from collection date character
  dplyr::mutate(collectYear = as.numeric(stringr::str_extract(string = collectDate, 
                                                              pattern = "[:digit:]{4}")),
                .after = collectDate)

# Check which columns were dropped
setdiff(x = names(data_v1), y = names(data_v2))

# Glimpse the data
dplyr::glimpse(data_v2)

# Export ----

# Export broadly pared down data
write.csv(x = data_v2, na = '', row.names = F,
          file = file.path("data", "app_data.csv"))

# End ----
