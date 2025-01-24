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

  # targets::tar_load("portLocations")
  # targets::tar_load("worldDataList")
  # targets::tar_load("lemisISOconversion")
  # lemisDataCorrected <- lemisDataRenamed

  mapDataCorrected <- lemisDataCorrected %>%
    filter(import_export == "I") %>%
    filter(rank == "Species") %>%
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
      group_by(iso2, country_name, originCapWild) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      pivot_wider(values_from = nSpp, names_from = originCapWild) %>%
      mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
      left_join(originSpecies_total) %>%
      left_join(worldDataList$worldDataCountryCentres)

    write.csv(originSpeciesWide, file = here("tables", paste0("originSpecies_capWildRanch_", grp, ".csv")),
              row.names = FALSE)

  }

}

