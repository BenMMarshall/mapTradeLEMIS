#' table_species_origin
#'
#' @name table_species_origin
#' @description a
#' @return a
#'
#' @export
table_species_origin <- function(lemisDataCorrected,
                                 portLocations,
                                 lemisISOconversion,
                                 worldDataList){

  # targets::tar_load("lemisDataRenamed")
  # targets::tar_load("portLocations")
  # targets::tar_load("worldDataList")
  # targets::tar_load("lemisISOconversion")
  # lemisDataCorrected <- lemisDataRenamed

  mapDataCorrected <- lemisDataCorrected %>%
    filter(rank == "Species")

  # plotList <- vector("list", length = length(unique(mapDataCorrected$group_)))
  # names(plotList) <- unique(mapDataCorrected$group_)
  for(grp in unique(mapDataCorrected$group_)){
    # grp <- "Amphibians"
    print(grp)

    routeSummarySpecies_total <- mapDataCorrected %>%
      filter(group_ == grp) %>%
      group_by(port, iso2, country_name) %>%
      summarise(nSpp = n_distinct(corrected))

    routeSummarySpecies_capWild <- mapDataCorrected %>%
      filter(group_ == grp) %>%
      group_by(port, iso2, country_name, originCapWild) %>%
      summarise(nSpp = n_distinct(corrected))

    routesSpeciesWide <- routeSummarySpecies_capWild %>%
      pivot_wider(values_from = nSpp, names_from = originCapWild) %>%
      mutate_if(is.integer, ~replace(., is.na(.), 0))

    routesSpeciesWide <- routesSpeciesWide %>%
      left_join(routeSummarySpecies_total) %>%
      rename("nSppTotal" = nSpp)

    worldRouteSummarySpeciesWide <- routesSpeciesWide %>%
      ungroup() %>%
      left_join(worldDataList$worldDataCountryCentres) %>%
      left_join(portLocations)

    write.csv(worldRouteSummarySpeciesWide, file = here("tables", paste0("routesSpecies_capWildRanch_", grp, ".csv")),
              row.names = FALSE)

    originSpecies_total <- mapDataCorrected %>%
      filter(group_ == grp) %>%
      group_by(iso2, country_name) %>%
      summarise(nSpp = n_distinct(corrected))

    originSpeciesWide <- mapDataCorrected %>%
      filter(group_ == grp) %>%
      group_by(iso2, country_name, originCapWild, group_) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      pivot_wider(values_from = nSpp, names_from = originCapWild) %>%
      mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      left_join(originSpecies_total) %>%
      left_join(worldDataList$worldDataCountryCentres)

    write.csv(originSpeciesWide, file = here("tables", paste0("originSpecies_capWildRanch_", grp, ".csv")),
              row.names = FALSE)

  }

}

