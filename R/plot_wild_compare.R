#' plot_wild_compare
#'
#' @name plot_wild_compare
#' @description abc
#' @return acb
#'
#' @export
plot_wild_compare <- function(paletteList, ...){

  # library(dplyr)
  # library(ggplot2)
  # library(here)
  # library(stringr)
  # library(colorspace)
  # library(tidyr)
  # library(scatterpie)
  # library(ggtext)
  # library(ggrepel)
  # library(patchwork)

  groups <- c("Reptiles", "Amphibians", "Terrestrial Mammals", "Birds")
  # grp <- "Birds"
  # targets::tar_source()
  paletteList <- generate_palette()

  groupDF <- paletteList$groupPaletteDF %>%
    filter(group_ %in% groups)
  groupVec <- groupDF$group_colour
  names(groupVec) <- glue::glue("<b><i style = 'font-size:16pt; color:{groupVec}'>{groupDF$group_}</i></b>")
  i <- 0
  allPlotList <- vector("list", length = length(groups))
  names(allPlotList) <- groups
  for(grp in groups){
    i <- i+1
    # originSpeciesData <- read.csv(here("tables", paste0("originSpecies_capWildRanch_", grp, ".csv")))
    # originQuantityData <- read.csv(here("tables", paste0("originQuantity_capWildRanch_", grp, ".csv")))
    originSpeciesData <- read.csv(here::here("tables", paste0("SuppTable - SpeciesOriginResidence_", grp, ".csv")))
    originQuantityData <- read.csv(here::here("tables", paste0("SuppTable - QuantOriginResidence_", grp, ".csv")))

    # names(originSpeciesData)[3:6] <- paste0("spp_", names(originSpeciesData)[3:6])
    # names(originQuantityData)[3:6] <- paste0("quant_", names(originQuantityData)[3:6])

    fullOriginData <- originSpeciesData %>%
      left_join(originQuantityData) %>%
      replace(is.na(.), 0) %>%
      mutate(nWhole_percent_Wild = nWhole_Wild / nWhole_total * 100,
             spp_percent_Wild = nSpp_Wild / nSpp_total * 100)

    fullOriginData$group_ <- grp

    isoLabels <- fullOriginData %>%
      left_join(paletteList$groupPaletteDF) %>%
      mutate(group_col = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      group_by(group_) %>%
      filter(
        (group_ == "Amphibians" & (nSpp_Wild > 75 | nWhole_Wild > 110000)) |
          (group_ == "Birds" & (nSpp_Wild > 400 | nWhole_Wild > 110000)) |
          (group_ == "Reptiles" & (nSpp_Wild > 200 | nWhole_Wild > 110000)) |
          (group_ == "Terrestrial Mammals" & (nSpp_Wild > 150 | nWhole_Wild > 110000))
      )

    # colour settings
    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == grp) %>%
      pull(group_colour)

    groupColourDark <- darken(groupColour,
                              amount = 0.5)

    plotWild <- fullOriginData %>%
      left_join(paletteList$groupPaletteDF) %>%
      mutate(group_col = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      filter(nWhole_Wild > 0, nSpp_Wild > 0) %>%
      ggplot() +
      # geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_point(aes(x = nSpp_Wild, y = nWhole_Wild,
                     size = spp_percent_Wild, colour = nWhole_percent_Wild), alpha = 0.75) +
      geom_text_repel(data = isoLabels,
                      aes(x = nSpp_Wild, y = nWhole_Wild,
                          label = iso2, colour = nWhole_percent_Wild),
                      fontface = 2) +
      scale_y_log10(labels = scales::comma) +
      scale_colour_gradient(low = groupColour, high = groupColourDark) +
      scale_size_continuous(range = c(0.25, 5)) +
      labs(x = "Number of species\nlisted as wild sourced", y = "Number of whole individuals\nlisted as wild sourced",
           colour = str_wrap("Percentage of total traded whole individuals listed as wild sourced", width = 25),
           size = str_wrap("Percentage of total traded species some level of wild capture listed", width = 25)) +
      facet_wrap(.~group_col, scales = "free") +
      theme_bw() +
      theme(
        line = element_line(colour = "#080808"),
        text = element_text(colour = "#080808"),
        # plot.title = element_text(face = 4, hjust = 0, colour = groupColour, size = 18),
        axis.title = element_text(face = 2, hjust = 0.5),
        axis.title.y = element_text(hjust = 0, vjust = 1),
        axis.title.x = element_text(margin = margin(5,0,5,0)),
        axis.title.x.top = element_text(margin = margin(5,0,5,0)),
        legend.title = element_text(face = 2, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.box.just = "right",
        axis.text.y = element_markdown(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_markdown(hjust = 0),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        panel.grid = element_blank())

    if(grp == "Terrestrial Mammals"){
      plotWild <- plotWild +
        theme(axis.title.x = element_blank())
    } else if(grp == "Birds"){
      plotWild <- plotWild +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank())
    } else if(grp == "Reptiles"){
      plotWild <- plotWild +
        theme(axis.title.y = element_blank())
    } else {
      plotWild <- plotWild +
        theme(axis.title.y = element_blank())
    }

    allPlotList[[grp]] <- plotWild
  }

  allFacets <- patchwork::wrap_plots(
    allPlotList[c("Terrestrial Mammals", "Birds", "Reptiles", "Amphibians")],
    ncol = 2)

  ggsave(plot = allFacets, filename = here("figures", "wildComparePlot.png"),
         width = 360, height = 280,
         dpi = 300, units = "mm")
  ggsave(plot = allFacets, filename = here("figures", "wildComparePlot.pdf"),
         width = 360, height = 280,
         units = "mm")

  return(allFacets)
  # most concerning would be in top, far right, dark colour, and large in size
}
