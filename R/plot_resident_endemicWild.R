#' plot_resident_endemicWild
#'
#' @name plot_resident_endemicWild
#' @description Mapping plot
#' @return Ggplot2 object and file
#'
#' @export
plot_resident_endemicWild <- function(lemisDataRenamed,
                                      worldDataList,
                                      portLocations,
                                      paletteList,
                                      allDistributionData){

  # library(dplyr)
  # library(ggplot2)
  # library(here)
  # library(stringr)
  # library(colorspace)
  # library(tidyr)
  # library(scatterpie)
  # library(ggtext)
  # targets::tar_load("lemisDataRenamed")
  # targets::tar_load("lemisISOconversion")
  # targets::tar_load("worldDataList")
  # targets::tar_load("portLocations")
  # targets::tar_load("paletteList")
  # targets::tar_load("allDistributionData")
  # targets::tar_load("extraManualDistributionDataList")

  lemisData <- lemisDataRenamed %>%
    filter(rank == "Species" & !corrected == "")

  lemisData[which(is.na(lemisData$iso2) & lemisData$country_origin == "Ctry_NA"),]$iso2 <- "NA"

  grpVector <- c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles", "Arachnids")

  grpLower <- str_to_lower(ifelse(grpVector == "Terrestrial Mammals", "mammals", grpVector))
  stillMissingList <- vector("list", length = length(grpLower))
  names(stillMissingList) <- grpLower

  plotList <- vector("list", length = length(grpVector))
  names(plotList) <- grpVector
  endemicDataList <- vector("list", length = length(grpVector))
  names(endemicDataList) <- grpVector
  for(grp in grpVector){
    print(grp)
    # grp <- "Reptiles"
    # grp <- "Amphibians"
    # grp <- "Birds"
    # grp <- "Terrestrial Mammals"
    grpL <- str_to_lower(ifelse(grp == "Terrestrial Mammals", "mammals", grp))

    lemisGroup <- lemisData %>%
      filter(group_ == grp)

    distData <- allDistributionData %>%
      filter(group_ == grpL)

    lemisGroup_dist <- lemisGroup %>%
      filter(!corrected == "") %>%
      left_join(distData,
                by = "corrected")

    lemisGroup_dist <- lemisGroup_dist %>%
      rowwise() %>%
      mutate(nativeOrigin = str_detect(allDistISO2, iso2)) %>%
      mutate(endemic = ifelse(str_count(allDistISO2, ", ") == 1, "Endemic", "NonEndemic"))

    lemisGroup_endemic <- lemisGroup_dist %>%
      group_by(corrected, nativeOrigin, iso2, originCapWild, endemic) %>%
      slice_head(n = 1) %>%
      group_by(iso2, nativeOrigin, originCapWild, endemic) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      ungroup() %>%
      tidyr::complete(iso2, nativeOrigin, originCapWild, endemic, fill = list(nSpp = 0)) %>%
      mutate(nativeOrigin = case_when(
        nativeOrigin == TRUE ~ "Resident",
        nativeOrigin == FALSE ~ "NonResident",
        is.na(nativeOrigin) ~ "Unknown"
      ))

    lemisGroup_endemic <- lemisGroup_endemic %>%
      filter(nativeOrigin == "Resident" &
               originCapWild == "Wild" &
               endemic == "Endemic") %>%
      rename("nSppResidentWildEndemic" = nSpp) %>%
      select(-nativeOrigin, -originCapWild) %>%
      group_by(iso2) %>%
      summarise(nSppResidentWildEndemic = sum(nSppResidentWildEndemic))

    # colour settings
    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == grp) %>%
      pull(group_colour)

    groupColourLight <- lighten(groupColour,
                                amount = 0.35)
    groupColourDark <- darken(groupColour,
                              amount = 0.35)

    # nativePalette <- c(colorspace::lighten(groupColour, amount = 0.5),
    #                    colorspace::darken(groupColour, amount = 0.5),
    #                    colorspace::lighten("#808080", amount = 0.25)
    # )
    # names(nativePalette) <- c("Resident", "NonResident", "Unknown")

    titleColoured <- paletteList$groupPaletteDF %>%
      mutate(title = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      filter(group_ == grp) %>%
      pull(title)

    lemisGroup_endemicPlotData <- lemisGroup_endemic %>%
      left_join(worldDataList$worldDataCountryCentres,
                by = "iso2") %>%
      mutate(nSppResidentWildEndemic = ifelse(nSppResidentWildEndemic == 0, NA, nSppResidentWildEndemic),
             group_ = grp)

    endemicDataList[[grp]] <- lemisGroup_endemicPlotData

    worldData_end <- worldDataList$worldData %>%
      left_join(lemisGroup_endemicPlotData,
                by = c("iso2" = "iso2"))
    wrappedData_end <- worldDataList$wrappedData %>%
      left_join(lemisGroup_endemicPlotData,
                by = c("iso2" = "iso2"))

    world_gg <- ggplot() +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey85") +
      geom_polygon(data = worldData_end, aes(x = long, y = lat, group = group,
                                             fill = nSppResidentWildEndemic),
                   # fill = "grey50",
                   colour = "grey35", linewidth = 0.025) +
      geom_polygon(data = wrappedData_end, aes(x = long, y = lat, group = group,
                                               fill = nSppResidentWildEndemic),
                   # fill = "grey50",
                   colour = "grey35", linewidth = 0.025)

    completePlot <- world_gg +
      # geom_point(data = lemisGroup_endemicPlotData,
      #            aes(x = centre_x_origin, y = centre_y_origin, size = nSppResidentWildEndemic,
      #                fill = nSppResidentWildEndemic),
      #            shape = 21,
      #            colour = groupColour
      # ) +
      coord_cartesian(xlim = c(-180-90, 90),
                      ylim = c(-84, 84),
                      expand = 0) +
      # scale_fill_viridis_d() +
      scale_fill_gradient(low = groupColourDark, high = groupColourLight) +
      # scale_linewidth_continuous(range = c(0.15, 4), breaks = routeRange) +
      scale_size_continuous(range = c(1, 5)) +
      # scale_alpha_continuous(range = c(0.1, 0.5), breaks = scales::pretty_breaks(n = 5)) +
      labs(title = titleColoured,
           size = "<b style = 'font-size:8pt'># Species</b><br><i style = 'font-size:6pt'>Origin and Port</i>",
           linewidth = "<i style = 'font-size:6pt'>Origin to Port Route</i>",
           fill = "<i style = 'font-size:6pt'>Number of traded endemics</i>",
           alpha = "<i style = 'font-size:6pt'>Origin to Port Route</i>") +
      theme_bw() +
      theme(
        aspect.ratio = 0.5,
        plot.title = element_markdown(size = 12),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = 4, hjust = 0, vjust = 0),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(1, 0),
        legend.background = element_rect(fill = "white"),
        # legend.margin = margin(0,0,0,0),
        # legend.key.size = unit(0.5, "mm"),
        legend.key.height = unit(1, "mm"),
        # legend.key.width = unit(0.5, "mm"),
        # legend.spacing = unit(0.1, "mm"),
        legend.box.margin = margin(0,0,0,0),
        legend.text = element_text(margin = margin(0,0,0,0), size = 5),
        legend.box.background = element_rect(fill = "white", colour = "white", linewidth = 2),
        legend.justification = c(1,0),
        legend.direction = "horizontal",
        legend.title.align = 0,
        legend.title = element_markdown(lineheight = 0.85, margin = margin(0,0,0,0))
      )

    plotList[[grp]] <- completePlot

  } # end of group loop

  allFacets <- patchwork::wrap_plots(
    plotList[c("Terrestrial Mammals",
               "Birds",
               "Reptiles",
               "Amphibians",
               "Arachnids"
    )],
    ncol = 2)

  ggsave(plot = allFacets, filename = here("figures", "residentWildEndemicPlot.png"),
         width = 400, height = 200, units = "mm", dpi = 300)
  ggsave(plot = allFacets, filename = here("figures", "residentWildEndemicPlot.pdf"),
         width = 400, height = 200, units = "mm")

  endemicDataAll <- do.call(rbind, endemicDataList)

  endemicDataAll %>%
    group_by(group_) %>%
    slice_max(nSppResidentWildEndemic, n = 5)

  write.csv(endemicDataAll, file = here("tables", "endemicDataCountries.csv"), row.names = FALSE)

  # combo map ---------------------------------------------------------------

  # endemicDataAll <- endemicDataAll %>%
  #   mutate(centre_y_origin = case_when(
  #     group_ == "Birds" ~ centre_y_origin + 1,
  #     group_ == "Terrestrial Mammals" ~ centre_y_origin + 1,
  #     group_ == "Amphibians" ~ centre_y_origin - 1,
  #     group_ == "Reptiles" ~ centre_y_origin -1),
  #     centre_x_origin = case_when(
  #       group_ == "Birds" ~ centre_x_origin - 1,
  #       group_ == "Terrestrial Mammals" ~ centre_x_origin + 1,
  #       group_ == "Amphibians" ~ centre_x_origin + 1,
  #       group_ == "Reptiles" ~ centre_x_origin -1
  #     ))
  #
  # groupColVec <- paletteList$groupPaletteDF$group_colour
  # names(groupColVec) <- paletteList$groupPaletteDF$group_
  #
  # groupColVecDark <- darken(groupColVec,
  #                           amount = 0.35)
  # names(groupColVecDark) <- paletteList$groupPaletteDF$group_
  #
  # titleColoured <- paletteList$groupPaletteDF %>%
  #   mutate(title = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
  #   filter(group_ %in% c("Reptiles", "Terrestrial Mammals", "Amphibians", "Birds")) %>%
  #   pull(title) %>% paste0(collapse = ", ")
  # titleColoured <- paste0("Traded Endemic Species of Originating Country and Classed as Wild: ", titleColoured)
  #
  # world_gg +
  #   geom_point(data = endemicDataAll,
  #              aes(x = centre_x_origin, y = centre_y_origin, size = nSppResidentWildEndemic,
  #                  fill = group_, colour = group_),
  #              shape = 21
  #   ) +
  #   coord_cartesian(xlim = c(-180-90, 90),
  #                   ylim = c(-84, 84),
  #                   expand = 0) +
  #   scale_fill_manual(values = groupColVec) +
  #   scale_colour_manual(values = groupColVecDark) +
  #   scale_size_continuous(range = c(2, 8)) +
  #   labs(title = titleColoured,
  #        size = "<b style = 'font-size:8pt'># Species</b><br><i style = 'font-size:6pt'>Origin and Port</i>",
  #        linewidth = "<i style = 'font-size:6pt'>Origin to Port Route</i>",
  #        fill = "<i style = 'font-size:6pt'>Resident in Orignating Country</i>",
  #        alpha = "<i style = 'font-size:6pt'>Origin to Port Route</i>") +
  #   theme_bw() +
  #   theme(
  #     aspect.ratio = 0.5,
  #     plot.title = element_markdown(size = 12, face = 2),
  #     panel.border = element_blank(),
  #     panel.grid = element_blank(),
  #     strip.background = element_blank(),
  #     strip.text = element_text(face = 4, hjust = 0, vjust = 0),
  #     axis.ticks = element_blank(),
  #     axis.title = element_blank(),
  #     axis.text = element_blank(),
  #     legend.position = c(1, 0),
  #     legend.background = element_rect(fill = "white"),
  #     legend.margin = margin(0,0,0,0),
  #     legend.key.size = unit(0.5, "mm"),
  #     legend.key.height = unit(0.25, "mm"),
  #     legend.key.width = unit(0.5, "mm"),
  #     legend.spacing = unit(0.1, "mm"),
  #     legend.box.margin = margin(0,0,0,0),
  #     legend.text = element_text(margin = margin(0,0,0,0), size = 5),
  #     legend.box.background = element_rect(fill = "white", colour = "white", linewidth = 2),
  #     legend.justification = c(1,0),
  #     legend.direction = "horizontal",
  #     legend.title.align = 0,
  #     legend.title = element_markdown(lineheight = 0.85, margin = margin(0,0,0,0))
  #   )

  return(endemicDataAll)

}
