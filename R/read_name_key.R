#' Read in keys
#'
#' @name read_keys
#' @description A number of cunctiont aht read in the required files to correct and update the names in the LEMIS dataset.
#' @return The file as a dataframe, ready for downstream functions.
#'
#' @export
read_lemis_key <- function(){

  lemisCodes <- read.csv(here("data", "figshareDownload", "lemis_codes.csv"))
  return(lemisCodes)

}

#' @export
read_port_key <- function(){

  portLocations <- read.csv(here("data", "port_latlong.csv"))
  return(portLocations)

}

#' @export
read_iso_key <- function(){

  lemisISOconversion <- read.csv(here("data", "lemisISO2conversion.csv"),
                                 na.strings = "<NA>")
  return(lemisISOconversion)

}

#' @export
read_world_data <- function(){

  worldData <- map_data("world")
  worldData[worldData$region == "Micronesia",]$region <- "Federated States of Micronesia"
  worldData[worldData$region == "Saint Martin",]$region <- "Sint Maarten"

  worldData <- worldData %>%
    mutate(iso2 = countrycode(sourcevar = region,
                              origin = "country.name",
                              destination = "iso2c"))

  # Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, Chagos
  # Archipelago, Grenadines, Heard Island, Kosovo, Madeira Islands,
  # Saba, Siachen Glacier, Sint Eustatius, Virgin Islands

  wrappedData <- worldData %>%
    group_by(region) %>%
    mutate(cross100lat = any(long >= 70)) %>%
    filter(cross100lat)

  wrappedData <- wrappedData %>%
    mutate(long = -180-(180-long))

  worldDataCountryCentres <- worldData %>%
    group_by(iso2) %>%
    summarise(centre_y_origin = mean(lat),
              centre_x_origin = mean(long))

  worldDataCountryCentres <- rbind(worldDataCountryCentres,
                                   data.frame(iso2 = "XX",
                                              centre_y_origin = -48.628486,
                                              centre_x_origin = -126.671752))

  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "US"),]$centre_y_origin <- 42.946011
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "US"),]$centre_x_origin <- -114.936032

  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "NZ"),]$centre_y_origin <- -41.053320
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "NZ"),]$centre_x_origin <- 175.091887

  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "AQ"),]$centre_y_origin <- -73.770226
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "AQ"),]$centre_x_origin <- -16.716902
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "CA"),]$centre_y_origin <- 54.553243
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "CA"),]$centre_x_origin <- -69.431648
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "GB"),]$centre_y_origin <- 52.857452
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "GB"),]$centre_x_origin <- -1.222984
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "NO"),]$centre_y_origin <- 60.921710
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "NO"),]$centre_x_origin <- 8.657099
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "LA"),]$centre_y_origin <- 19.361919
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "LA"),]$centre_x_origin <- 102.586823
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "VN"),]$centre_y_origin <- 12.830124
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "VN"),]$centre_x_origin <- 108.921331
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "MM"),]$centre_y_origin <- 20.454361
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "MM"),]$centre_x_origin <- 95.042594
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "IN"),]$centre_y_origin <- 24.444591
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "IN"),]$centre_x_origin <- 78.095019
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "MY"),]$centre_y_origin <- 5.096628
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "MY"),]$centre_x_origin <- 101.609000
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "EC"),]$centre_y_origin <- -1.015691
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "EC"),]$centre_x_origin <- -79.211533
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "PE"),]$centre_y_origin <- -10.831143
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "PE"),]$centre_x_origin <- -75.720452
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "CL"),]$centre_y_origin <- -33.387840
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "CL"),]$centre_x_origin <- -70.933604
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "FR"),]$centre_y_origin <- 47.850808
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "FR"),]$centre_x_origin <- 1.815731
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "SE"),]$centre_y_origin <- 59.697018
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "SE"),]$centre_x_origin <- 16.848155
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "FI"),]$centre_y_origin <- 61.457978
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "FI"),]$centre_x_origin <- 25.265780
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "ID"),]$centre_y_origin <- -6.508481
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "ID"),]$centre_x_origin <- 107.015925
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "ZM"),]$centre_y_origin <- 2.911146
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "ZM"),]$centre_x_origin <- 45.656586
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "SO"),]$centre_y_origin <- 2.911146
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "SO"),]$centre_x_origin <- 45.656586
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "OM"),]$centre_y_origin <- 1.886139
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "OM"),]$centre_x_origin <- -157.415742
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "FJ"),]$centre_y_origin <- -17.777980
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "FJ"),]$centre_x_origin <- 177.901936
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "KI"),]$centre_y_origin <- 1.886139
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "KI"),]$centre_x_origin <- -157.415742
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "UA"),]$centre_y_origin <- 50.126941
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "UA"),]$centre_x_origin <- 30.845575
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "AZ"),]$centre_y_origin <- 40.342731
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "AZ"),]$centre_x_origin <- 48.996315
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "JP"),]$centre_y_origin <- 36.344776
  worldDataCountryCentres[which(worldDataCountryCentres$iso2 == "JP"),]$centre_x_origin <- 138.899800

  worldDataCountryCentres <- rbind(worldDataCountryCentres,
                                   data.frame(iso2 = "HK", centre_y_origin = 22.320215, centre_x_origin = 114.174598))

  wrappedData <- worldData %>%
    group_by(region) %>%
    mutate(cross100lat = any(long >= 70)) %>%
    filter(cross100lat)

  wrappedData <- wrappedData %>%
    mutate(long = -180-(180-long))

  return(list("worldData" = worldData,
              "wrappedData" = wrappedData,
              "worldDataCountryCentres" = worldDataCountryCentres))

}

#' @export
iucn_distribution_data <- function(){

  IUCNAllList <- vector("list", length = 4)
  names(IUCNAllList) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(IUCNAllList)){
    # n <- names(IUCNAllList)[1]
    if(n == "birds"){
      iDistData1  <- read.csv(here("data", "iucnData",
                                   paste0(n, "_countries1.csv")),
                              na.strings = "<NA>")
      iDistData2  <- read.csv(here("data", "iucnData",
                                   paste0(n, "_countries2.csv")),
                              na.strings = "<NA>")
      iDistData <- rbind(iDistData1, iDistData2)
    } else {
      iDistData  <- read.csv(here("data", "iucnData",
                                  paste0(n, "_countries.csv")),
                             na.strings = "<NA>")
    }

    iNamesData  <- read.csv(here("data", "iucnData",
                                 paste0(n, "_gbifNames.csv")),
                            na.strings = "<NA>")
    iSynsData  <- read.csv(here("data", "iucnData",
                                paste0(n, "_synonyms.csv")),
                           na.strings = "<NA>")

    iDistData_present <- iDistData %>%
      # filter(presence == "Extant" | presence == "Possibly Extant") %>%
      group_by(scientificName) %>%
      summarise(iucnDistIso2 = paste0(code, collapse = ", "))

    iSynsData <- iSynsData %>%
      select(scientificName, synonym) %>%
      group_by(scientificName) %>%
      summarise(iucnSyns = paste0(synonym, collapse = ", "))

    iDataAll <- iNamesData %>%
      rename("scientificName" = user_supplied_name) %>%
      left_join(iDistData_present, by = "scientificName") %>%
      left_join(iSynsData, by = c("scientificName"))

    iDataAll <- iDataAll %>%
      group_by(corrected) %>%
      summarise(iucnDistIso2 = paste0(iucnDistIso2, collapse = ", "),
                iucnSyns = paste0(iucnSyns, collapse = ", "))

    IUCNAllList[[n]] <- iDataAll
  }
  return(IUCNAllList)

}


#' @export
read_extraManualDistribution_data <- function(){

  extraManualDistributionDataList <- vector("list", length = 4)
  names(extraManualDistributionDataList) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(extraManualDistributionDataList)){
    emdRead <- read.csv(here("data",
                             paste0(n, "_gbifDistMissing_filled.csv")))
    emdRead$corrected <- trimws(emdRead$corrected)
    emdRead$correctedSpelling <- trimws(emdRead$correctedSpelling)
    extraManualDistributionDataList[[n]] <- emdRead
  }

  return(extraManualDistributionDataList)

}


#' @export
combine_distribution_data <- function(extraManualDistributionDataList){

  distributionDataList <- vector("list", length = 4)
  names(distributionDataList) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(distributionDataList)){
    distributionDataList[[n]] <- read.csv(here("data", "speciesDistributions",
                                               paste0(n, "_distributionData_gbifNames.csv")))
  }

  extraGBIFDistributionDataList <- vector("list", length = 4)
  names(extraGBIFDistributionDataList) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(extraGBIFDistributionDataList)){
    extraGBIFDistributionDataList[[n]] <- read.csv(here("data",
                                                        paste0(n, "_gbifDistFill.csv")))
  }

  extraGBIFDistribution_2DataList <- vector("list", length = 4)
  names(extraGBIFDistribution_2DataList) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(extraGBIFDistribution_2DataList)){
    extraGBIFDistribution_2DataList[[n]] <- read.csv(here("data",
                                                          paste0(n, "_gbifDistFill_2.csv")))
  }

  iucnDistributionDataList <- vector("list", length = 4)
  names(iucnDistributionDataList) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(iucnDistributionDataList)){
    iucnDistributionDataList[[n]] <- read.csv(here("data", "iucnData",
                                                   paste0(n, "_synonymDist.csv")))
  }
  iucnDistributionDataList[["mammals"]][iucnDistributionDataList[["mammals"]]$species == "Bison bison",]

  countryNamesRegex <- countrycode::codelist[,c("country.name.en.regex", "iso2c")]

  eMdistData <- extraManualDistributionDataList
  names(extraManualDistributionDataList)
  eMdistDataList <- list()
  for(grp in names(extraManualDistributionDataList)){
    print(grp)
    eMdistData <- extraManualDistributionDataList[[grp]]
    for(r in 1:nrow(eMdistData)){
      # r <- 1
      if(is.na(eMdistData[r,]$distISO2) | eMdistData[r,]$distISO2 == "<NA>"){
        eMdistData[r,]$distISO2 <- paste(countryNamesRegex$iso2c[str_detect(str_to_lower(eMdistData[r,]$text),
                                                                            countryNamesRegex$country.name.en.regex)],
                                         collapse = ", ")
      }
    }
    eMdistDataList[[grp]] <- eMdistData
  }

  ddList <- lapply(distributionDataList, function(x){
    x %>%
      select(corrected, distISO2)
  })
  egList <- lapply(extraGBIFDistributionDataList, function(x){
    x %>%
      select(corrected, distISO2)
  })
  eg2List <- lapply(extraGBIFDistribution_2DataList, function(x){
    x %>%
      select(corrected, distISO2)
  })
  emList <- lapply(eMdistDataList, function(x){
    x %>%
      dplyr::select("corrected" = correctedSpelling, distISO2)
  })
  iuList <- lapply(iucnDistributionDataList, function(x){
    x %>%
      select("corrected" = species, distISO2)
  })

  allDistList <- list()
  for(grp in names(ddList)){
    df <- rbind(ddList[[grp]],
                egList[[grp]],
                eg2List[[grp]],
                emList[[grp]],
                iuList[[grp]])
    df <- df %>%
      filter(!is.na(corrected)) %>%
      mutate(group_ = grp)
    allDistList[[grp]] <- df

  }

  # loop for making sure duplicates are corrected into a single entry
  for(n in names(allDistList)){
    distData <- allDistList[[n]]
    for(c in unique(distData$corrected)){
      # c <- "Ablepharus pannonicus"
      # c <- "Uma inornata"
      # c <- unique(distData$corrected)[1]
      if(nrow(distData[distData$corrected == c,]) > 1){
        distISO2_combined <- unique(unlist(str_split(distData[distData$corrected == c,]$distISO2, ", ")))
        distData[distData$corrected == c,]$distISO2 <- paste0(distISO2_combined, collapse = ", ")
      } else {
        {next}
      }
    }
    allDistList[[n]] <- distData %>%
      filter(!duplicated(corrected))
  }

  return(allDistList)

}
