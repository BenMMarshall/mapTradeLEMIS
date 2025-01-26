library(here)
library(dplyr)
library(stringr)
library(rgbif)
library(ggplot2)
library(sf)
library(CoordinateCleaner)
targets::tar_load("worldDataList")

world_sf <- worldDataList$worldData %>%
  st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("epsg:4326")) %>%
  group_by(group, region, iso2) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

grpVector <- c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles")

grpLower <- str_to_lower(ifelse(grpVector == "Terrestrial Mammals", "mammals", grpVector))

for(grp in grpVector){

  print(grp)
  # grp <- "Reptiles"
  # grp <- "Amphibians"
  # grp <- "Birds"
  # grp <- "Terrestrial Mammals"
  # grp <- "Arachnids"
  grpL <- str_to_lower(ifelse(grp == "Terrestrial Mammals", "mammals", grp))

  load(here("data", paste0("retryGBIF_", grpL, ".rds")))

  gbifDistIso2List <- vector("list", length = length(retryGBIF))
  names(gbifDistIso2List) <- retryGBIF
  totSpp <- length(retryGBIF)
  for(sp in retryGBIF){
    # sp <- retryGBIF[1]

    print(sp)
    if(paste0(sp, "_gbifSummary2.csv") %in% list.files(here("data", "gbifDistData", grpL),
                                                       pattern = paste0(sp, "_gbifSummary2.csv"))){
      print("File exists")
      {next}
    }

    print("--- Occ. Searching...")
    occOUT <- try(
      occ_search(scientificName = sp,
                 limit = 10000,
                 hasCoordinate = TRUE,
                 hasGeospatialIssue = FALSE)
    )
    print("--- Occ. Searched")

    if(class(occOUT) == "try-error"){
      gbifLocCounts <- data.frame(
        corrected = sp,
        nRecords = NA,
        nExclusions = NA,
        nClean = NA
      )
      print("--- Occ. NA")
      write.csv(gbifLocCounts, file = here("data", "gbifDistData", grpL, paste0(sp, "_gbifSummary.csv")))
      {next}
    }

    occData <- try(
      occOUT$data %>%
        dplyr::select(species,
                      decimalLongitude, decimalLatitude,
                      countryCode, individualCount,
                      # gbifID, family, taxonRank, coordinateUncertaintyInMeters,
                      # year,
                      basisOfRecord) %>%
        filter(!is.na(decimalLongitude)) %>%
        filter(!is.na(decimalLatitude)) %>%
        filter(individualCount > 0 | is.na(individualCount)) %>%
        filter(basisOfRecord == "HUMAN_OBSERVATION" |
                 basisOfRecord == "OBSERVATION" |
                 basisOfRecord == "PRESERVED_SPECIMEN")
    )

    if(class(occData)[1] == "try-error"){
      gbifLocCounts <- data.frame(
        corrected = sp,
        nRecords = 0,
        nExclusions = 0,
        nClean = 0
      )
      print("--- Occ. 0")
      write.csv(gbifLocCounts, file = here("data", "gbifDistData", grpL, paste0(sp, "_gbifSummary2.csv")))
      {next}
    } else if(occOUT$meta$count == 0 | nrow(occData) == 0){
      gbifLocCounts <- data.frame(
        corrected = sp,
        nRecords = 0,
        nExclusions = 0,
        nClean = 0
      )
      print("--- Occ. 0")
      write.csv(gbifLocCounts, file = here("data", "gbifDistData", grpL, paste0(sp, "_gbifSummary2.csv")))
      {next}
    }

    flags <- clean_coordinates(x = occData,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               tests = c("capitals", "centroids",
                                         "equal", "zeros",
                                         "outliers"),
                               capitals_rad = 10000,
                               centroids_rad = 1000,
                               outliers_mtp = 5,
                               outliers_method = "quantile",
                               outliers_size = 7
    )

    # flags[!flags$.summary,]
    # summary(flags)
    # plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

    #Exclude problematic records
    occCleaned <- occData[flags$.summary,]

    gbifLocCounts <- data.frame(
      corrected = sp,
      nRecords = occOUT$meta$count,
      nExclusions = sum(!flags$.summary),
      nClean = nrow(occCleaned)
    )

    occCleaned$sampled <- FALSE
    if(nrow(occCleaned) > 1000){
      occCleaned[sample(1:nrow(occCleaned), 1000),]$sampled <- TRUE
    } else {
      occCleaned$sampled <- TRUE
    }

    occ_sf <- occCleaned %>%
      filter(sampled) %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = sf::st_crs("epsg:4326"))

    print("--- Occ. Intersection...")
    interOUT <- st_intersection(occ_sf, world_sf)
    iso2OUT <- paste(sort(unique(interOUT$iso2)), collapse = ", ")

    gbifCountries <- data.frame(
      corrected = sp,
      gbifDistIso2 = iso2OUT
    )

    write.csv(occCleaned, file = here("data", "gbifDistData", grpL, paste0(sp, "_gbifClean.csv")))
    write.csv(gbifCountries, file = here("data", "gbifDistData", grpL, paste0(sp, "_gbifCountries.csv")))
    write.csv(gbifLocCounts, file = here("data", "gbifDistData", grpL, paste0(sp, "_gbifSummary2.csv")))

    print("--- Occ. Saved")
  }

}
