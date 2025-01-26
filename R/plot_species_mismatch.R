#' plot_species_mismatch
#'
#' @name plot_species_mismatch
#' @description abc
#' @return acb
#'
#' @export
plot_species_mismatch <- function(paletteList, ...){
  # library(ggplot2)
  # library(ggrepel)

  # paletteList <- generate_palette()

  # grp <- "Amphibians"
  groups <- c("Reptiles", "Amphibians", "Terrestrial Mammals", "Birds", "Arachnids")
  allOriginList <- list()
  i <- 0
  for(grp in groups){
    i <- i +1
    speciesOriginResidenceTable <- read.csv(here::here("tables", paste0("SuppTable - SpeciesOriginResidence_", grp, ".csv")))
    quantOriginResidenceTable <- read.csv(here::here("tables", paste0("SuppTable - QuantOriginResidence_", grp, ".csv")))
    originResidenceData <- speciesOriginResidenceTable %>%
      left_join(quantOriginResidenceTable)
    originResidenceData$group_ <- grp
    allOriginList[[i]] <- originResidenceData
  }
  allOriginResidenceData <- do.call(rbind, allOriginList)

  groupDF <- paletteList$groupPaletteDF %>%
    filter(group_ %in% groups)
  groupVec <- groupDF$group_colour
  names(groupVec) <- glue::glue("<b><i style = 'font-size:16pt; color:{groupVec}'>{groupDF$group_}</i></b>")

  isoLabels <- allOriginResidenceData %>%
    left_join(paletteList$groupPaletteDF) %>%
    mutate(group_col = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
    filter(!iso2 == "XX") %>%
    group_by(group_) %>%
    filter(
      (group_ == "Amphibians" & (nSpp_NonResidentWildMismatch > 18 | nSpp_total > 115)) |
        (group_ == "Birds" & (nSpp_NonResidentWildMismatch > 100 | nSpp_total > 700)) |
        (group_ == "Reptiles" & (nSpp_NonResidentWildMismatch > 100 | nSpp_total > 400)) |
        (group_ == "Arachnids" & (nSpp_NonResidentWildMismatch > 20 | nSpp_total > 110)) |
        (group_ == "Terrestrial Mammals" & (nSpp_NonResidentWildMismatch > 75 | nSpp_total > 250))
    )

  mismatchPlot <- allOriginResidenceData %>%
    left_join(paletteList$groupPaletteDF) %>%
    mutate(group_col = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
    filter(!iso2 == "XX") %>%
    # mutate(tropical = case_when(
    #   centre_y_origin < 23.5 & centre_y_origin > -23.5 ~ "Tropical",
    #   TRUE ~ "NotTropical"),
    #   continent = countrycode(iso2, origin = "iso2c", destination = "continent")
    #   ) %>%
    ggplot() +
    geom_point(aes(x = nSpp_total, y = nSpp_NonResidentWildMismatch,
                   size = nWhole_total, colour = group_col), alpha = 0.75) +
    geom_text_repel(data = isoLabels,
                    aes(x = nSpp_total, y = nSpp_NonResidentWildMismatch,
                        label = iso2, colour = group_col),
                    fontface = 2) +
    facet_wrap(.~group_col, scales = "free") +
    scale_colour_manual(values = groupVec) +
    scale_size_continuous(range = c(1.5, 9)) +
    labs(x = "Total Number of Traded Species", y = "Number of Traded\nNon-Resident Species") +
    theme_bw() +
    theme(
      line = element_line(colour = "#080808"),
      text = element_text(colour = "#080808"),
      axis.title = element_text(face = 2, hjust = 0.5),
      axis.title.y = element_text(hjust = 0, vjust = 1),
      axis.title.x = element_text(margin = margin(5,0,5,0)),
      axis.title.x.top = element_text(margin = margin(5,0,5,0)),
      legend.title = element_text(face = 2),
      legend.position = "none",
      axis.text.y = element_markdown(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_markdown(hjust = 0),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      panel.grid = element_blank())

  ggsave(plot = mismatchPlot, filename = here("figures", "mismatchPlot.png"),
         width = 280, height = 220,
         dpi = 300, units = "mm")
  ggsave(plot = mismatchPlot, filename = here("figures", "mismatchPlot.pdf"),
         width = 280, height = 220,
         units = "mm")

  return(mismatchPlot)

}
