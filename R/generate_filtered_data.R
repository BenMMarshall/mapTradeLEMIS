#' Filter out to the core reliable data
#'
#' @name generate_filtered_data
#' @description A function to do some basic tidying of the shipment years, and recategorise the origin column.
#' @return A dataframe in the same format as the LEMIS data input, just filtered.
#'
#' @export
generate_filtered_data <- function(lemisDataRenamed, lemisCodes){

  # simplify wild captive cats and shipment year
  lemisDataCorrected <- lemisDataRenamed %>%
    filter(import_export == "I") %>%
    mutate(
      originCapWild = case_when(
        source %in% c("A", "C", "D", "F") ~ "Captive",
        source == "R" ~ "Ranched",
        source == "W" ~ "Wild",
        TRUE ~ "Other"
      )
    ) %>%
    mutate(sYear = as.numeric(substr(shipment_date, 1, 4))) %>%
    mutate(sYear = if_else(is.na(sYear),
                           as.numeric(substr(disposition_date, 1, 4)),
                           sYear)
    )

  lemisDataCorrected$quantity <- round(abs(as.numeric(lemisDataCorrected$quantity)),
                                       digits = 0)

  exclusions <- lemisDataRenamed[which(!lemisDataRenamed$entryID %in%
                                         lemisDataCorrected$entryID),]

  write.csv(exclusions,
            here::here("data", "exclusionsFiltered_2023-11-11.csv"))

  return(lemisDataCorrected)
}
