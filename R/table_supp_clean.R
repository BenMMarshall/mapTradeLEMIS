#' table_supp_clean
#'
#' @name table_supp_clean
#' @description Combine some tables to make easier to work with supplements
#' @return csvs
#'
#' @export
table_supp_clean <- function(){

  for(grp in c("Reptiles", "Amphibians", "Terrestrial Mammals", "Birds")){

    resiCapWildQuant <- read.csv(here::here("tables", paste0("originQuantity_capWildRanch_", grp, ".csv")))
    resiQuant <- read.csv(here::here("tables", paste0("residenceQuantity_", grp, ".csv")))
    resiMismatchQuant <- read.csv(here::here("tables", paste0("residenceWildMismatchQuantity_", grp, ".csv")))

    quantOriginResidenceTable <- resiCapWildQuant %>%
      left_join(resiQuant) %>%
      left_join(resiMismatchQuant) %>%
      select(
        iso2, country_name, centre_y_origin, centre_x_origin,
        nWhole_total,
        "nWhole_Wild" = Wild, "nWhole_Captive" = Captive, "nWhole_Ranched" = Ranched, "nWhole_Other" = Other,
        "nWhole_NonResident" = NonResident, "nWhole_Resident" = Resident, "nWhole_UnknownResident" = Unknown,
        "nWhole_NonResidentWildMismatch" = nWholeNonResidentWildMismatch
      )

    quantOriginResidenceTable[is.na(quantOriginResidenceTable)] <- 0
    quantOriginResidenceTable$group_ <- grp
    write.csv(quantOriginResidenceTable, here::here("tables", paste0("SuppTable - QuantOriginResidence_", grp, ".csv")),
              row.names = FALSE)

    resiCapWildSpecies <- read.csv(here::here("tables", paste0("originSpecies_capWildRanch_", grp, ".csv")))
    resiSpecies <- read.csv(here::here("tables", paste0("residenceSpecies_", grp, ".csv")))
    resiMismatchSpecies <- read.csv(here::here("tables", paste0("residenceWildMismatchSpecies_", grp, ".csv")))

    speciesOriginResidenceTable <- resiCapWildSpecies %>%
      left_join(resiSpecies) %>%
      left_join(resiMismatchSpecies) %>%
      select(
        iso2, country_name, centre_y_origin, centre_x_origin,
        nSpp_total,
        "nSpp_Wild" = Wild, "nSpp_Captive" = Captive, "nSpp_Ranched" = Ranched, "nSpp_Other" = Other,
        "nSpp_NonResident" = NonResident, "nSpp_Resident" = Resident, "nSpp_UnknownResident" = Unknown,
        "nSpp_NonResidentWildMismatch" = nSppNonResidentWildMismatch
      )
    speciesOriginResidenceTable$group_ <- grp

    write.csv(speciesOriginResidenceTable, here::here("tables", paste0("SuppTable - SpeciesOriginResidence_", grp, ".csv")),
              row.names = FALSE)
  }

}
