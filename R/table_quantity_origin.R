#' table_quantity_origin
#'
#' @name table_quantity_origin
#' @description a
#' @return a
#'
#' @export
table_quantity_origin <- function(lemisDataFiltered,
                                 portLocations,
                                 lemisISOconversion,
                                 worldDataList){

  # targets::tar_load("lemisDataFiltered")

  mapDataFiltered <- lemisDataFiltered

  # plotList <- vector("list", length = length(unique(mapDataCorrected$group_)))
  # names(plotList) <- unique(mapDataCorrected$group_)
  for(grp in unique(mapDataFiltered$group_)){
    # grp <- "Amphibians"

    routeSummaryQuantity_total <- mapDataFiltered %>%
      filter(group_ == grp) %>%
      group_by(port, iso2, country_name) %>%
      summarise(nWhole = sum(quantity))

    routeSummaryQuantity_capWild <- mapDataFiltered %>%
      filter(group_ == grp) %>%
      group_by(port, iso2, country_name, originCapWild) %>%
      summarise(nWhole = sum(quantity))

    routesQuantityWide <- routeSummaryQuantity_capWild %>%
      pivot_wider(values_from = nWhole, names_from = originCapWild) %>%
      mutate_if(is.integer, ~replace(., is.na(.), 0))

    routesQuantityWide <- routesQuantityWide %>%
      left_join(routeSummaryQuantity_total) %>%
      rename("nWholeTotal" = nWhole)

    worldRouteSummaryQuantityWide <- routesQuantityWide %>%
      ungroup() %>%
      left_join(worldDataList$worldDataCountryCentres) %>%
      left_join(portLocations)

    write.csv(worldRouteSummaryQuantityWide, file = here("tables", paste0("routesQuantity_capWildRanch_", grp, ".csv")),
              row.names = FALSE)

    originQuantity_total <- mapDataFiltered %>%
      filter(group_ == grp) %>%
      group_by(iso2, country_name) %>%
      summarise(nWhole = sum(quantity))

    originQuantityWide <- mapDataFiltered %>%
      filter(group_ == grp) %>%
      group_by(iso2, country_name, originCapWild, group_) %>%
      summarise(nWhole = sum(quantity)) %>%
      pivot_wider(values_from = nWhole, names_from = originCapWild) %>%
      mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      left_join(originQuantity_total) %>%
      left_join(worldDataList$worldDataCountryCentres)

    write.csv(originQuantityWide, file = here("tables", paste0("originQuantity_capWildRanch_", grp, ".csv")),
              row.names = FALSE)

  }

}

