#' Read in renamed lemis data
#'
#' @name read_trade_data
#' @description abc.
#' @return The file as a dataframe, ready for downstream functions.
#'
#' @export
read_renamed_data <- function(extraManualDistributionDataList, lemisISOconversion, ...){
  lemisDataCorrected <- read_csv(here("data", "figshareDownload", "lemisDataCorrected_2023-11-11.csv.gz"))

  # updating a few misnamed species
  for(grp in names(extraManualDistributionDataList)){
    # grp <- "reptiles"
    print(grp)
    extraDistDF <- extraManualDistributionDataList[[grp]]
    for(corr in extraDistDF$corrected){
      # corr <- extraDistDF$corrected[1]
      if(length(extraDistDF[extraDistDF$corrected == corr,]$corrected) >0){
        print(corr)
        lemisDataCorrected[lemisDataCorrected$corrected == corr,]$corrected <-
          ifelse(is.null(extraDistDF$correctedSpelling[extraDistDF$corrected == corr]),
                 extraDistDF$corrected[extraDistDF$corrected == corr],
                 extraDistDF$correctedSpelling[extraDistDF$corrected == corr])
      }
    }
  }

  lemisDataCorrected <- lemisDataCorrected %>%
    filter(import_export == "I") %>%
    filter(!group_ == "Miscellaneous" & !is.na(group_)) %>%
    mutate(code_origin = sub("Ctry_", "", country_origin),
           code_imp = sub("Ctry_", "", country_imp_exp)) %>%
    left_join(lemisISOconversion)  %>%
    mutate(country_name = case_when(
      iso2 == "XX" | is.na(iso2) ~ "Unknown",
      TRUE ~ country_name
    )) %>%
    mutate(iso2 = case_when(
      is.na(iso2) ~ "XX",
      TRUE ~ iso2
    )) %>%
    mutate(vert = case_when(group_ %in% c("Terrestrial Mammals",
                                          "Reptiles",
                                          "Birds",
                                          "Amphibians",
                                          "Fish",
                                          "Marine Mammals") ~ "Vertebrates",
                            group_ %in% c("Crustaceans and Molluscs",
                                          "Arachnids",
                                          "Insecta and Myriapoda",
                                          "Other Invertebrates",
                                          "Lepidoptera",
                                          "Echinoderms and Cnidaria",
                                          "Porifera Sponges, Bryozoa, and Squirts",
                                          "Plants",
                                          "Miscellaneous") ~ "Invertebrates"),
           vert = factor(vert, levels = c("Vertebrates", "Invertebrates"))) %>%
    mutate(
      originCapWild = case_when(
        source %in% c("A", "C", "D", "F") ~ "Captive",
        source == "R" ~ "Ranched",
        source == "W" ~ "Wild",
        TRUE ~ "Other"
      )
    )

  return(lemisDataCorrected)
}

#' @export
read_filtered_data <- function(extraManualDistributionDataList, lemisISOconversion, ...){
  lemisDataFiltered <- read_csv(here("data", "figshareDownload", "lemisDataOutliersRemoved_2024-01-09_mag2_tenK.csv.gz"))

  # updating a few misnamed species
  for(grp in names(extraManualDistributionDataList)){
    # grp <- "reptiles"
    print(grp)
    extraDistDF <- extraManualDistributionDataList[[grp]]
    for(corr in extraDistDF$corrected){
      print(corr)
      # corr <- extraDistDF$corrected[1]
      if(length(extraDistDF[extraDistDF$corrected == corr,]$corrected) >0){
        lemisDataFiltered[lemisDataFiltered$corrected == corr,]$corrected <-
          ifelse(is.null(extraDistDF$correctedSpelling[extraDistDF$corrected == corr]),
                 extraDistDF$corrected[extraDistDF$corrected == corr],
                 extraDistDF$correctedSpelling[extraDistDF$corrected == corr])
      }
    }
  }

  lemisDataFiltered <- lemisDataFiltered %>%
    filter(import_export == "I") %>%
    filter(!group_ == "Miscellaneous" & !is.na(group_)) %>%
    mutate(code_origin = sub("Ctry_", "", country_origin),
           code_imp = sub("Ctry_", "", country_imp_exp)) %>%
    left_join(lemisISOconversion) %>%
    mutate(country_name = case_when(
      iso2 == "XX" | is.na(iso2) ~ "Unknown",
      TRUE ~ country_name
    )) %>%
    mutate(iso2 = case_when(
      is.na(iso2) ~ "XX",
      TRUE ~ iso2
    )) %>%
    mutate(vert = case_when(group_ %in% c("Terrestrial Mammals",
                                          "Reptiles",
                                          "Birds",
                                          "Amphibians",
                                          "Fish",
                                          "Marine Mammals") ~ "Vertebrates",
                            group_ %in% c("Crustaceans and Molluscs",
                                          "Arachnids",
                                          "Insecta and Myriapoda",
                                          "Other Invertebrates",
                                          "Lepidoptera",
                                          "Echinoderms and Cnidaria",
                                          "Porifera Sponges, Bryozoa, and Squirts",
                                          "Plants",
                                          "Miscellaneous") ~ "Invertebrates"),
           vert = factor(vert, levels = c("Vertebrates", "Invertebrates"))) %>%
    mutate(
      originCapWild = case_when(
        source %in% c("A", "C", "D", "F") ~ "Captive",
        source == "R" ~ "Ranched",
        source == "W" ~ "Wild",
        TRUE ~ "Other"
      )
    )

  return(lemisDataFiltered)
}


