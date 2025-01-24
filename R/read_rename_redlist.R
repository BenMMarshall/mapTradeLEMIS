#' Read in IUCN data and synonyms
#'
#' @name read_rename_redlist
#' @description Will read in IUCN data and check if species are traded or not
#' @return A dataframe for summary plotting as well as a summary table as csv.
#'   Also IUCN data is reexported with a traded/not present class added.
#'
#' @export
read_rename_redlist <- function(lemisDataRenamed){

  # targets::tar_load("lemisDataRenamed")

  iucnDataList_corrected <- vector("list", length = 4)
  names(iucnDataList_corrected) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(iucnDataList_corrected)){
    iucnDataList_corrected[[n]] <- read.csv(here("data", "iucnData",
                                            paste0(n, "_gbifNames.csv")))
  }

  iucnDataList_syns <- vector("list", length = 4)
  names(iucnDataList_syns) <- c("amphibians", "birds", "mammals", "reptiles")
  for(n in names(iucnDataList_syns)){
    iucnDataList_syns[[n]] <- read.csv(here("data", "iucnData",
                                            paste0(n, "_synonyms.csv")))
  }

  lemisSpeciesData <- lemisDataRenamed %>%
    filter(import_export == "I") %>%
    filter(rank == "Species") %>%
    filter(!group_ == "Miscellaneous" & !is.na(group_)) %>%
    mutate(code_origin = sub("Ctry_", "", country_origin),
           code_imp = sub("Ctry_", "", country_imp_exp)) %>%
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
           vert = factor(vert, levels = c("Vertebrates", "Invertebrates")))

  iucnRedlistSummary_list <- iucnDataList_corrected
  for(grp in names(iucnDataList_corrected)){
    # grp <- names(iucnDataList_corrected)[3]
    Grp <- stringr::str_to_sentence(grp)

    if(Grp == "Mammals"){
      Grp <- c("Terrestrial Mammals", "Marine Mammals")
    }

    tempData <- lemisSpeciesData %>%
      filter(group_ %in% Grp)

    tempIUCNsynData <- iucnDataList_syns[[grp]] %>%
      mutate(traded = ifelse(synonym %in% tempData$corrected, "Traded", "Not present")) %>%
      group_by(scientificName) %>%
      summarise(traded = any(traded == "Traded")) %>%
      filter(traded)

    tempIUCNData <- iucnDataList_corrected[[grp]] %>%
      mutate(traded = ifelse(corrected %in% tempData$corrected, "Traded",
                             ifelse(corrected %in% tempIUCNsynData$scientificName,
                                    "Traded", "Not present")))

    write.csv(tempIUCNData, here("data", "iucnData", paste0("fullIUCNdata_traded_",
                                                            stringr::str_to_lower(grp),
                                                            ".csv")), row.names = FALSE)

    extinctTraded <- tempIUCNData %>%
      filter(traded == "Traded") %>%
      filter(redlistCategory %in% c("Extinct in the Wild", "Extinct"))

    write.csv(extinctTraded, here("data", "iucnData", paste0("extinctTradedIUCNdata_",
                                                            stringr::str_to_lower(grp),
                                                            ".csv")), row.names = FALSE)

    iucnRedlistSummary_list[[grp]] <- tempIUCNData %>%
      mutate(redlistCategory = case_when(
        redlistCategory %in% c("Lower Risk/conservation dependent", "Lower Risk/least concern") ~ "Least Concern",
        redlistCategory %in% c("Lower Risk/near threatened") ~ "Near Threatened",
        TRUE ~ redlistCategory
      )) %>%
      mutate(redlistCategory = factor(
        redlistCategory, levels = c(
          "Data Deficient",
          "Extinct",
          "Extinct in the Wild",
          "Critically Endangered",
          "Endangered",
          "Vulnerable",
          "Near Threatened",
          "Least Concern"
        )
      )) %>%
      group_by(redlistCategory) %>%
      mutate(nSpp = n_distinct(corrected)) %>%
      group_by(redlistCategory, traded, nSpp) %>%
      summarise(count = n_distinct(corrected)) %>%
      mutate(group_ = stringr::str_to_sentence(grp))

  }
  iucnRedlistSummary_df <- do.call(rbind, iucnRedlistSummary_list)

  iucnRedlistSummary <- iucnRedlistSummary_df %>%
    # select(-iucnCol, -colour, -nSpecies) %>%
    rename("nSpp_group_" = nSpp, "nSpp_trade" = count)

  write.csv(iucnRedlistSummary, here("tables", "iucnRedlistSummary.csv"),
            row.names = FALSE)

  return(iucnRedlistSummary_df)

}
