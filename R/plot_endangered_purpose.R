#' plot_critendangered_purpose
#'
#' @name plot_critendangered_purpose
#' @description Mapping plot
#' @return Ggplot2 object and file
#'
#' @export
plot_critendangered_purpose <- function(lemisDataRenamed, paletteList){

  # targets::tar_load("lemisDataRenamed")
  # targets::tar_load("lemisCodes")
  # lemisCodes %>%
  #   filter(field == "description")

  critendangeredFiles <- list.files(here("data", "iucnData"), pattern = "critendangeredTradedIUCNdata_",
                             full.names = TRUE)

  critendangeredTraded <- do.call(rbind, lapply(critendangeredFiles, read.csv))

  lemisData <- lemisDataRenamed %>%
    filter(rank == "Species" & !corrected == "")

  critendangeredTradedData <- lemisData %>%
    filter(corrected %in% critendangeredTraded$corrected |
             lemisName %in% critendangeredTraded$corrected)


  critendangeredTradedData <- critendangeredTradedData %>%
    mutate(group_ = case_when(
      group_ %in% c("Marine Mammals", "Terrestrial Mammals") ~ "Mammals",
      TRUE ~ group_
    )) %>%
    # filter(!is.na(purpose) & !purpose == "non-standard value", !purpose == "*") %>%
    mutate(purpose = factor(case_when(
      purpose == "B" ~  "Breeding in captivity\nor artificial propagation",
      purpose == "E" ~                                      "Educational",
      purpose == "G" ~                                  "Botanic gardens",
      purpose == "H" ~                                 "Hunting Trophies",
      purpose == "L" ~   "Law Enforcement / Judicial / Forensic use only",
      # purpose == "M" ~                              "Biomedical research",
      purpose == "P" ~                                         "Personal",
      # purpose == "Q" ~                   "Circuses/traveling exhibitions",
      purpose %in% c("S", "M") ~                  "Scientific/Research",
      purpose == "T" ~                                       "Commercial",
      purpose == "Y" ~        "Reintroduction/introduction\ninto the wild",
      purpose %in% c("Z", "Q") ~                        "Zoos/Circuses",
      TRUE ~ "Other"))) %>%
    mutate(description = factor(case_when(
      description %in% c("BOD", "EGL", "DEA", "SPE", "SKI") ~ "Whole Individual\n(Non-trophy)",
      description %in% c("LIV") ~ "Live",
      description %in% c("TRO") ~ "Trophy",
      description %in% c("BOC", "BON", "BOP") ~ "Bones/Bone product",
      description %in% c("HAI", "HAP", "HOC", "HOP", "HOR") ~ "Horn/Hair product",
      description %in% c("MEA") ~ "Meat",
      TRUE ~ "Other animal part"
      # TRUE ~ description
    )))

  write.csv(critendangeredTradedData, file = here("tables", "critendangeredTradedData.csv"), row.names = FALSE)

  critendangeredTradedData %>%
    ggplot() +
    geom_bar(aes(x = purpose, fill = description)) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
    scale_fill_manual(values = paletteList$purposePalVector) +
    facet_wrap(vars(group_), scales = "free") +
    coord_flip() +
    labs(x = "Listed trade\npurpose", y = "Number of records", fill = "Description\nof item") +
    theme_bw() +
    theme(
      line = element_line(colour = "#080808"),
      text = element_text(colour = "#080808"),
      axis.title = element_text(face = 2, hjust = 0.5),
      axis.title.y = element_text(hjust = 0, vjust = 1, angle = 0),
      axis.title.x = element_text(margin = margin(5,0,5,0)),
      axis.title.x.top = element_text(margin = margin(5,0,5,0)),
      legend.title = element_text(face = 2),
      legend.position = "bottom",
      # axis.text.y = element_markdown(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 0),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      panel.grid = element_blank())

  ggsave(filename = here("figures", "critendangeredTraded_purposeDescriptionPlot.png"),
         width = 300, height = 200, units = "mm", dpi = 300)
  ggsave(filename = here("figures", "critendangeredTraded_purposeDescriptionPlot.pdf"),
         width = 300, height = 200, units = "mm")

  return(critendangeredTradedData)

}
