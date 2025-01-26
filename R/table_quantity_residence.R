#' table_quantity_residence
#'
#' @name table_quantity_residence
#' @description Table for residency quantity
#' @return csvs
#'
#' @export
table_quantity_residence <- function(lemisDataFiltered, lemisISOconversion,
                                     worldDataList, portLocations,
                                     allDistributionData){

  # targets::tar_load("lemisDataFiltered")
  # targets::tar_load("lemisISOconversion")
  # targets::tar_load("worldDataList")
  # targets::tar_load("portLocations")
  # targets::tar_load("paletteList")
  # targets::tar_load("allDistributionData")

  lemisData <- lemisDataFiltered  %>%
    filter(rank == "Species" & !corrected == "")

  lemisData[which(is.na(lemisData$iso2) & lemisData$country_origin == "Ctry_NA"),]$iso2 <- "NA"

  grpVector <- c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles", "Arachnids")

  grpLower <- str_to_lower(ifelse(grpVector == "Terrestrial Mammals", "mammals", grpVector))
  retryGBIF <- vector("list", length = length(grpLower))
  names(retryGBIF) <- grpLower

  tableList <- vector("list", length = length(grpVector))
  names(tableList) <- grpVector
  for(grp in grpVector){
    print(grp)
    # grp <- "Reptiles"
    # grp <- "Amphibians"
    # grp <- "Birds"
    # grp <- "Terrestrial Mammals"
    # grp <- "Arachnids"
    grpL <- str_to_lower(ifelse(grp == "Terrestrial Mammals", "mammals", grp))

    lemisGroup <- lemisData %>%
      filter(group_ == grp)

    distData <- allDistributionData %>%
      filter(group_ == grpL)

    lemisGroup_dist <- lemisGroup %>%
      filter(!corrected == "") %>%
      left_join(distData,
                by = "corrected")

    retryGBIF <- unique(lemisGroup_dist[is.na(lemisGroup_dist$allDistISO2),]$corrected)

    save(retryGBIF, file = here("data", paste0("retryGBIF_", grpL, ".rds")))

    lemisGroup_dist <- lemisGroup_dist %>%
      rowwise() %>%
      mutate(nativeOrigin = str_detect(allDistISO2, iso2))

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
