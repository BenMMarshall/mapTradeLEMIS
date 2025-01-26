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
read_extraManualDistribution_data <- function(){

  extraManualDistributionDataList <- vector("list", length = 5)
  names(extraManualDistributionDataList) <- c("amphibians", "birds", "mammals", "reptiles", "arachnids")
  for(n in names(extraManualDistributionDataList)){
    if(n == "arachnids"){
      {next}
    }
    emdRead <- read.csv(here("data",
                             paste0(n, "_nameFixes.csv")))
    emdRead$corrected <- trimws(emdRead$corrected)
    emdRead$correctedSpelling <- trimws(emdRead$correctedSpelling)
    emdRead <- emdRead %>%
      filter(!correctedSpelling == "")
    extraManualDistributionDataList[[n]] <- emdRead
  }

  return(extraManualDistributionDataList)

}


#' @export
combine_distribution_data <- function(extraManualDistributionDataList){

  distributionDataList <- vector("list", length = 5)
  names(distributionDataList) <- c("amphibians", "birds", "mammals", "reptiles", "arachnids")
  for(n in names(distributionDataList)){
    extData <- read.csv(here("data", "externalDistributionData",
                             paste0(n, "_distributionData_gbifNames.csv")),
                        na.strings = c("", "<NA>"))
    if(n == "arachnids"){
      extData_corr <- extData %>%
        group_by(corrected, group_) %>%
        summarise(extDistISO2 = paste0(distISO2, collapse = ", "))
      extData_alt <- extData %>%
        group_by(inputName, group_) %>%
        summarise(extDistISO2 = paste0(distISO2, collapse = ", "))
    } else if(n == "birds"){
      extData_corr <- extData %>%
        group_by(corrected, group_) %>%
        summarise(extDistISO2 = paste0(iso2, collapse = ", "))
      extData_alt <- extData %>%
        group_by(inputName, group_) %>%
        summarise(extDistISO2 = paste0(iso2, collapse = ", "))
    } else if(n == "mammals"){
      extData_corr <- extData %>%
        group_by(corrected, group_) %>%
        summarise(extDistISO2 = paste0(distISO2, collapse = ", "))
      extData_alt <- extData %>%
        group_by(inputName, group_) %>%
        summarise(extDistISO2 = paste0(distISO2, collapse = ", "))
    } else if(n == "reptiles"){
      extData_corr <- extData %>%
        group_by(corrected, group_) %>%
        summarise(extDistISO2 = paste0(distISO2, collapse = ", "))
      extData_alt <- extData %>%
        group_by(inputName, group_) %>%
        summarise(extDistISO2 = paste0(distISO2, collapse = ", "))
    } else if(n == "amphibians"){
      extData_corr <- extData %>%
        group_by(corrected, group_) %>%
        summarise(extDistISO2 = paste0(iso2, collapse = ", "))
      extData_alt <- extData %>%
        group_by(inputName, group_) %>%
        summarise(extDistISO2 = paste0(iso2, collapse = ", "))
    }
    extData <- rbind(extData_corr, extData_alt %>%
                       rename(corrected = inputName))

    distributionDataList[[n]] <- extData

  }

  iucnDistributionDataList <- vector("list", length = 5)
  names(iucnDistributionDataList) <- c("amphibians", "birds", "mammals", "reptiles", "arachnids")
  for(n in names(iucnDistributionDataList)){
    iucnData <- read.csv(here("data", "iucnData",
                              paste0(n, "_countries_gbifNames.csv")),
                         na.strings = c("", "<NA>")) %>%
      filter(presence %in% c("Extant", "Possibly Extant")) %>%
      select(scientificName, corrected, code, group_)

    iucnSynData <- read.csv(here("data", "iucnData", paste0(n, "_synonyms.csv")))
    iucnSynData <- iucnSynData %>%
      mutate(synonym = paste(genusName, speciesName)) %>%
      select(scientificName, synonym)

    iucnData_corr <- iucnData %>%
      group_by(corrected, group_) %>%
      summarise(iucnDistISO2 = paste0(code, collapse = ", "))

    iucnData_alt <- iucnData %>%
      group_by(scientificName, group_) %>%
      summarise(iucnDistISO2 = paste0(code, collapse = ", "))

    iucnData <- rbind(iucnData_corr, iucnData_alt %>%
                        rename(corrected = scientificName))

    iucnDataSynDist <- vector("list", length = length(unique(iucnSynData$scientificName)))
    names(iucnDataSynDist) <- unique(iucnSynData$scientificName)
    for(sci in unique(iucnSynData$scientificName)){

      if(nrow(iucnData[iucnData$corrected == sci,]) > 0){
        iucnDataSynDist[[sci]] <- data.frame(corrected = iucnSynData[iucnSynData$scientificName == sci,]$synonym,
                                             iucnDistISO2 = iucnData[iucnData$corrected == sci,]$iucnDistISO2[1],
                                             group_ = iucnData[iucnData$corrected == sci,]$group_[1])
      } else {
        iucnDataSynDist[[sci]] <- NULL
      }

    }
    iucnDataSynDist <- do.call(rbind, iucnDataSynDist)

    iucnData <- rbind(iucnData, iucnDataSynDist)

    iucnDistributionDataList[[n]] <- iucnData
  }

  gbifDistributionDataList <- vector("list", length = 5)
  names(gbifDistributionDataList) <- c("amphibians", "birds", "mammals", "reptiles", "arachnids")
  for(n in names(gbifDistributionDataList)){
    gbifDistributionDataList[[n]] <- read.csv(here("data", "gbifDistData",
                                                   paste0(n, "_gbifCountries.csv")),
                                              na.strings = c("", "<NA>")) %>%
      select(corrected, "gbifDistISO2" = gbifDistIso2, group_)
  }

  # allDistList <- list()
  # for(grp in names(gbifDistributionDataList)){
  #   allDistList[[grp]] <- distributionDataList[[grp]] %>%
  #     full_join(iucnDistributionDataList[[grp]]) %>%
  #     full_join(gbifDistributionDataList[[grp]]) %>%
  #     filter(!is.na(corrected) | corrected == "NA")
  # }
  # allDistributionData <- do.call(rbind, allDistList)

  allDistributionDataList <- vector("list", length = 5)
  names(allDistributionDataList) <- names(distributionDataList)
  for(n in names(distributionDataList)){
    print(n)
    uniNames <- unique(c(distributionDataList[[n]]$corrected,
                         iucnDistributionDataList[[n]]$corrected,
                         gbifDistributionDataList[[n]]$corrected))
    withinGroupList <- vector("list", length = length(uniNames))
    names(withinGroupList) <- uniNames
    for(sp in uniNames){
      print(sp)
      # sp<- "Nhandu carapoensis"

      uniqueISO2 <- unique(c(str_split(distributionDataList[[n]][distributionDataList[[n]]$corrected == sp,]$extDistISO2,
                                       ", ", simplify = TRUE),
                             str_split(iucnDistributionDataList[[n]][iucnDistributionDataList[[n]]$corrected == sp,]$iucnDistISO2,
                                       ", ", simplify = TRUE),
                             str_split(gbifDistributionDataList[[n]][gbifDistributionDataList[[n]]$corrected == sp,]$gbifDistISO2,
                                       ", ", simplify = TRUE)))

      withinGroupList[[sp]] <- data.frame(corrected = sp,
                                          allDistISO2 = paste0(uniqueISO2, collapse = ", "),
                                          group_ = n)

    }
    allDistributionDataList[[n]] <- do.call(rbind, withinGroupList)

  }
  allDistributionData <- do.call(rbind, allDistributionDataList)

  # allDistributionData$allDistISO2 <- NA
  # for(r in 1:nrow(allDistributionData)){
  #   # r <- 2
  #   print(r)
  #   uniqueISO2 <- unique(c(str_split(allDistributionData$extDistISO2[r], ", ", simplify = TRUE),
  #                          str_split(allDistributionData$iucnDistISO2[r], ", ", simplify = TRUE),
  #                          str_split(allDistributionData$gbifDistISO2[r], ", ", simplify = TRUE)))
  #   uniqueISO2 <- uniqueISO2[!uniqueISO2 == "" & !is.na(uniqueISO2)]
  #   allDistributionData[r,]$allDistISO2 <- paste0(uniqueISO2, collapse = ", ")
  # }

  allDistributionData <- allDistributionData %>%
    filter(!is.na(corrected) & !corrected == "" & !corrected == "NA")

  return(allDistributionData)

}
