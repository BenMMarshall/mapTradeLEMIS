#' table_quantity_residence
#'
#' @name table_quantity_residence
#' @description Table for residency quantity
#' @return csvs
#'
#' @export
table_quantity_residence <- function(lemisDataFiltered, lemisISOconversion,
                                     worldDataList, portLocations,
                                     allDistributionDataList){

  # targets::tar_load("lemisDataFiltered")
  # targets::tar_load("lemisISOconversion")
  # targets::tar_load("worldDataList")
  # targets::tar_load("portLocations")
  # targets::tar_load("paletteList")
  # targets::tar_load("allDistributionDataList")

  lemisData <- lemisDataFiltered %>%
    filter(import_export == "I") %>%
    filter(!group_ == "Miscellaneous" & !is.na(group_)) %>%
    mutate(code_origin = sub("Ctry_", "", country_origin),
           code_imp = sub("Ctry_", "", country_imp_exp)) %>%
    left_join(lemisISOconversion) %>%
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

  lemisData[which(is.na(lemisData$iso2) & lemisData$country_origin == "Ctry_NA"),]$iso2 <- "NA"

  grpVector <- c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles")

  grpLower <- str_to_lower(ifelse(grpVector == "Terrestrial Mammals", "mammals", grpVector))
  stillMissingList <- vector("list", length = length(grpLower))
  names(stillMissingList) <- grpLower

  tableList <- vector("list", length = length(grpVector))
  names(tableList) <- grpVector
  for(grp in grpVector){
    print(grp)
    # grp <- "Reptiles"
    # grp <- "Amphibians"
    # grp <- "Birds"
    # grp <- "Terrestrial Mammals"
    grpL <- str_to_lower(ifelse(grp == "Terrestrial Mammals", "mammals", grp))

    lemisGroup <- lemisData %>%
      filter(group_ == grp)

    if(grp == "Birds"){
      distData <- allDistributionDataList$birds
    } else if(grp == "Terrestrial Mammals"){
      distData <- allDistributionDataList$mammals
    } else if(grp == "Amphibians"){
      distData <- allDistributionDataList$amphibians
    } else if(grp == "Reptiles"){
      distData <- allDistributionDataList$reptiles
    }

    lemisGroup_dist <- lemisGroup %>%
      filter(!corrected == "") %>%
      left_join(distData %>%
                  filter(!is.na(distISO2) & !distISO2 == "<NA>"),
                by = "corrected")

    lemisGroup_dist <- lemisGroup_dist %>%
      rowwise() %>%
      mutate(nativeOrigin = str_detect(distISO2, iso2))

    lemisGroup_long <- lemisGroup_dist %>%
      group_by(iso2, nativeOrigin) %>%
      summarise(nWhole = sum(quantity, na.rm = TRUE)) %>%
      left_join(worldDataList$worldDataCountryCentres,
                by = "iso2")

    lemisGroup_long <- lemisGroup_long %>%
      ungroup() %>%
      tidyr::complete(iso2, nativeOrigin, fill = list(nWhole = 0)) %>%
      group_by(iso2) %>%
      mutate(centre_y_origin = centre_y_origin[!is.na(centre_y_origin)][1],
             centre_x_origin = centre_x_origin[!is.na(centre_x_origin)][1])

    lemisGroup_tabledata <- lemisGroup_long %>%
      mutate(nativeOrigin = case_when(
        nativeOrigin == TRUE ~ "Resident",
        nativeOrigin == FALSE ~ "NonResident",
        is.na(nativeOrigin) ~ "Unknown"
      )) %>%
      pivot_wider(names_from = nativeOrigin, values_from = nWhole)

    lemisGroup_tabledata <- lemisGroup_tabledata %>%
      left_join(lemisGroup_dist %>%
                  group_by(iso2) %>%
                  summarise(nWhole_total = sum(quantity, na.rm = TRUE)))

    write.csv(lemisGroup_tabledata, here("tables", paste0("residenceQuantity_", grp, ".csv")),
              row.names = FALSE)

    lemisGroup_mismatches <- lemisGroup_dist %>%
      group_by(iso2, nativeOrigin, originCapWild) %>%
      summarise(nWhole = sum(quantity, na.rm = TRUE)) %>%
      ungroup() %>%
      tidyr::complete(iso2, nativeOrigin, originCapWild, fill = list(nWhole = 0)) %>%
      mutate(nativeOrigin = case_when(
        nativeOrigin == TRUE ~ "Resident",
        nativeOrigin == FALSE ~ "NonResident",
        is.na(nativeOrigin) ~ "Unknown"
      )) %>%
      filter(nativeOrigin == "NonResident" & originCapWild == "Wild") %>%
      rename("nWholeNonResidentWildMismatch" = nWhole) %>%
      left_join(lemisGroup_dist %>%
                  group_by(iso2) %>%
                  summarise(nWhole = sum(quantity, na.rm = TRUE))) %>%
      select(-nativeOrigin, -originCapWild)

    write.csv(lemisGroup_mismatches, here("tables", paste0("residenceWildMismatchQuantity_", grp, ".csv")),
              row.names = FALSE)

    tableList[[grp]] <- list(
      "lemisGroup_tabledata" = lemisGroup_tabledata,
      "lemisGroup_mismatches" = lemisGroup_mismatches
    )

  }
  return(tableList)
}
