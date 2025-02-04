#' plot_species_residence
#'
#' @name plot_species_residence
#' @description Mapping plot
#' @return Ggplot2 object and file
#'
#' @export
plot_species_residence <- function(lemisDataRenamed, lemisISOconversion,
                                   worldDataList, portLocations,
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
  # library(ggnewscale)
  # library(patchwork)
  # targets::tar_load("lemisDataRenamed")
  # targets::tar_load("lemisISOconversion")
  # targets::tar_load("worldDataList")
  # targets::tar_load("portLocations")
  # targets::tar_load("paletteList")
  # targets::tar_load("allDistributionData")

  lemisData <- lemisDataRenamed %>%
    filter(rank == "Species" & !corrected == "")

  lemisData[which(is.na(lemisData$iso2) & lemisData$country_origin == "Ctry_NA"),]$iso2 <- "NA"

  grpVector <- c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles", "Arachnids")

  grpLower <- str_to_lower(ifelse(grpVector == "Terrestrial Mammals", "mammals", grpVector))
  stillMissingList <- vector("list", length = length(grpLower))
  names(stillMissingList) <- grpLower

  plotList <- vector("list", length = length(grpVector))
  names(plotList) <- grpVector
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

  #   noDistSpp <- lemisGroup_dist %>%
  #       filter(is.na(allDistISO2) | allDistISO2 == "<NA>") %>%
  #       pull(corrected) %>% unique()
  #   distSpp <- lemisGroup_dist %>%
  #     pull(corrected) %>% unique()
  #   print(grp)
  #   print(100 - length(noDistSpp) / length(distSpp) *100)
  # }

    # noDistSpp <- lemisGroup_dist %>%
    #   filter(is.na(distISO2) | distISO2 == "<NA>") %>%
    #   pull(corrected) %>% unique()
    # noDistSpp <- noDistSpp[!noDistSpp == ""]
    # stillMissingList[[grpL]] <- noDistSpp
    # {next}

    # colour settings
    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == grp) %>%
      pull(group_colour)

    groupColourDark <- darken(groupColour,
                              amount = 0.35)

    nativePalette <- c(colorspace::lighten(groupColour, amount = 0.5),
                       colorspace::darken(groupColour, amount = 0.5),
                       colorspace::lighten("#808080", amount = 0.25)
    )
    names(nativePalette) <- c("Resident", "NonResident", "Unknown")

    titleColoured <- paletteList$groupPaletteDF %>%
      mutate(title = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      filter(group_ == grp) %>%
      pull(title)
    # colour setting end

    routeLineData <- lemisGroup_dist %>%
      group_by(port, iso2) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      ungroup() %>%
      left_join(worldDataList$worldDataCountryCentres) %>%
      left_join(portLocations, by = "port") %>%
      filter(!is.na(portName))

    routeRange <- range(routeLineData$nSpp)

    originTop <- lemisGroup_dist %>%
      group_by(iso2) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      filter(!is.na(iso2)) %>%
      ungroup() %>%
      mutate(max = max(nSpp),
             percent2top = nSpp/max) %>%
      filter(percent2top > 0.5) %>%
      mutate(lab = nSpp) %>%
      left_join(worldDataList$worldDataCountryCentres)

    lemisGroup_dist <- lemisGroup_dist %>%
      rowwise() %>%
      mutate(nativeOrigin = str_detect(allDistISO2, iso2))

    write.csv(lemisGroup_dist, here("tables", paste0("LEMIS_distributionsAdded_", grp, ".csv")),
              row.names = FALSE)

    lemisGroup_long <- lemisGroup_dist %>%
      group_by(corrected, nativeOrigin, iso2) %>%
      slice_head(n = 1) %>%
      group_by(iso2, nativeOrigin) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      left_join(worldDataList$worldDataCountryCentres,
                by = "iso2")

    lemisGroup_long <- lemisGroup_long %>%
      ungroup() %>%
      tidyr::complete(iso2, nativeOrigin, fill = list(nSpp = 0)) %>%
      group_by(iso2) %>%
      mutate(centre_y_origin = centre_y_origin[!is.na(centre_y_origin)][1],
             centre_x_origin = centre_x_origin[!is.na(centre_x_origin)][1])

    lemisGroup_plotdata <- lemisGroup_long %>%
      mutate(nativeOrigin = case_when(
        nativeOrigin == TRUE ~ "Resident",
        nativeOrigin == FALSE ~ "NonResident",
        is.na(nativeOrigin) ~ "Unknown"
      )) %>%
      pivot_wider(names_from = nativeOrigin, values_from = nSpp)

    lemisGroup_plotdata <- lemisGroup_plotdata %>%
      left_join(lemisGroup_dist %>%
                  group_by(corrected, iso2) %>%
                  slice_head(n = 1) %>%
                  group_by(iso2) %>%
                  summarise(nSpp_total = n_distinct(corrected)))

    write.csv(lemisGroup_plotdata, here("tables", paste0("residenceSpecies_", grp, ".csv")),
              row.names = FALSE)

    lemisGroup_mismatches <- lemisGroup_dist %>%
      group_by(corrected, nativeOrigin, iso2, originCapWild) %>%
      slice_head(n = 1) %>%
      group_by(iso2, nativeOrigin, originCapWild) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      ungroup() %>%
      tidyr::complete(iso2, nativeOrigin, originCapWild, fill = list(nSpp = 0)) %>%
      mutate(nativeOrigin = case_when(
        nativeOrigin == TRUE ~ "Resident",
        nativeOrigin == FALSE ~ "NonResident",
        is.na(nativeOrigin) ~ "Unknown"
      )) %>%
      filter(nativeOrigin == "NonResident" & originCapWild == "Wild") %>%
      rename("nSppNonResidentWildMismatch" = nSpp) %>%
      left_join(lemisGroup_dist %>%
                  group_by(corrected, iso2) %>%
                  slice_head(n = 1) %>%
                  group_by(iso2) %>%
                  summarise(nSpp_total = n_distinct(corrected))) %>%
      select(-nativeOrigin, -originCapWild)

    write.csv(lemisGroup_mismatches, here("tables", paste0("residenceWildMismatchSpecies_", grp, ".csv")),
              row.names = FALSE)

    worldData_nSpp <- worldDataList$worldData %>%
      left_join(lemisGroup_plotdata %>%
                  select(iso2, nSpp_total))
    wrappedData_nSpp <- worldDataList$wrappedData %>%
      left_join(lemisGroup_plotdata %>%
                  select(iso2, nSpp_total))

    # colour settings
    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == grp) %>%
      pull(group_colour)

    groupColourLight_nSpp <- lighten(groupColour,
                                amount = 0.5)
    groupColourDark_nSpp <- darken(groupColour,
                              amount = 0.35)

    world_gg <- ggplot() +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey85") +
      geom_polygon(data = worldData_nSpp, aes(x = long, y = lat, group = group,
                                                       fill = nSpp_total),
                   # fill = "grey50",
                   colour = "grey35", linewidth = 0.025) +
      geom_polygon(data = wrappedData_nSpp, aes(x = long, y = lat, group = group,
                                                         fill = nSpp_total),
                   # fill = "grey50",
                   colour = "grey35", linewidth = 0.025) +
      scale_fill_gradient(high = groupColourDark_nSpp, low = groupColourLight_nSpp,
                          na.value = "grey75")

    # radDF <- data.frame("group" = c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles"),
    #                     radModLeg = c(150, 80, 20, 150),
    #                     radModCir = c(500, 150, 20, 150)
    # ) %>%
    #   filter(group == grp)

    # radiusModifierLegend <- radDF$radModLeg
    # radiusModifierCircle <- radDF$radModCir

    # radiusModifierCircle <- round(max(lemisGroup_plotdata$nSpp_total)/10, digits = 0)
    # lemisGroup_plotdata$nSpp_total/radiusModifierCircle

    radiusModifier <- 2.5
    radiusModifierLegend <- 2.25
    radiusScaled <- scale(lemisGroup_plotdata$nSpp_total, center = FALSE)* radiusModifier
    # radiusScaled <- radiusScaled
    # radiusScaled * attr(radiusScaled, 'scaled:scale') / 2

    lemisGroup_plotdata$scaledSpp <- scale(lemisGroup_plotdata$nSpp_total, center = FALSE)[,1]

    lemisGroup_plotdata <- lemisGroup_plotdata %>%
      mutate(scaledSpp = scaledSpp* radiusModifier)

    completePlot <-
      world_gg +
      new_scale_fill() +
      # geom_curve(data = routeLineData,
      #            aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
      #                xend = if_else(port_lon > 90, -180-(180-port_lon), port_lon),
      geom_curve(data = lemisGroup_plotdata,
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     xend = -100,
                     y = centre_y_origin,
                     yend = 40,
                     # linewidth = nSpp,
                     # alpha = nSpp),
                     linewidth = nSpp_total,
                     alpha = nSpp_total),
                 position = position_jitter(width = 0.5, height = 0.5),
                 curvature = -0.5,
                 angle = 140,
                 colour = groupColour
      ) +

      geom_polygon(data = worldData_nSpp %>%
                     filter(iso2 == "US"), aes(x = long, y = lat, group = group),
                   colour = nativePalette[2], fill = NA, linewidth = 0.25) +

      # geom_point(data = portLocations,
      #            aes(x = port_lon, y = port_lat),
      #            size = 0.5,
      #            shape = 21,
      #            colour = "white",
      #            fill = groupColourDark
      #            # fill = "black"
      # ) +
      geom_point(data = lemisGroup_plotdata %>%
                   filter(scaledSpp < 1),
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     y = centre_y_origin),
                 size = 0.35,
                 shape = 21,
                 colour = "white",
                 fill = groupColour
      ) +
      geom_scatterpie(data = lemisGroup_plotdata,
                      aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                          y = centre_y_origin, group = iso2,
                          r = scaledSpp),
                      sorted_by_radius = TRUE, linewidth = 0.15,
                      cols = c("Resident", "NonResident", "Unknown"), color = "white", alpha = 0.8) +
      geom_scatterpie_legend(scale(lemisGroup_plotdata$nSpp_total, center = FALSE)* radiusModifierLegend,
                             n = 4,
                             x = -250, y = -65,
                             labeller = function(x){round(x* attr(radiusScaled, 'scaled:scale') / radiusModifier,
                                                          digits = 0)},
                             colour = groupColourDark) +
      geom_text(data = originTop,
                aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                    y = centre_y_origin, label = iso2
                ),
                colour = "white", size = 1.75, fontface = 2, vjust = 1) +
      coord_cartesian(xlim = c(-180-90, 90),
                      ylim = c(-84, 84),
                      expand = 0) +
      # scale_fill_viridis_d() +
      scale_fill_manual(values = nativePalette) +
      scale_linewidth_continuous(range = c(0.15, 4), breaks = routeRange) +
      # scale_size_continuous(range = c(0.15, 5), breaks = pointRange) +
      scale_alpha_continuous(range = c(0.1, 0.5), breaks = scales::pretty_breaks(n = 5)) +
      labs(title = titleColoured,
           size = "<b style = 'font-size:8pt'># Species</b><br><i style = 'font-size:6pt'>Origin and Port</i>",
           linewidth = "<i style = 'font-size:6pt'>Origin to Port Route</i>",
           fill = "<i style = 'font-size:6pt'>Resident in Orignating Country</i>",
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
        legend.position.inside = c(1, 0),
        legend.background = element_rect(fill = "white"),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(0.5, "mm"),
        legend.key.height = unit(0.25, "mm"),
        legend.key.width = unit(0.5, "mm"),
        legend.spacing = unit(0.1, "mm"),
        legend.box.margin = margin(0,0,0,0),
        legend.text = element_text(margin = margin(0,0,0,0), size = 5),
        legend.box.background = element_rect(fill = "white", colour = "white", linewidth = 2),
        legend.justification = c(1,0),
        legend.direction = "horizontal",
        legend.title = element_markdown(lineheight = 0.85, margin = margin(0,0,0,0), hjust = 0)
      ) +
      guides(
        linewidth = guide_legend(override.aes = list(linewidth = c(0.5,4)),
                                 order = 2, nrow = 1,
                                 direction = "horizontal",
                                 title.position = "top",
                                 margin = margin(0,0,0,0)),
        fill = guide_legend(order = 3, nrow = 1,
                            direction = "horizontal",
                            title.position = "top",
                            margin = margin(0,0,0,0)),
        alpha = guide_none(),
        size = guide_legend(override.aes = list(size = c(1,5)),
                            order = 1, nrow = 1,
                            direction = "horizontal",
                            title.position = "top",
                            margin = margin(0,0,0,0))
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

  ggsave(plot = allFacets, filename = here("figures", "distributionOriginPlot.png"),
         width = 400, height = 200, units = "mm", dpi = 300)
  ggsave(plot = allFacets, filename = here("figures", "distributionOriginPlot.pdf"),
         width = 400, height = 200, units = "mm")
  # targets::tar_load("plotSpeciesResidence")
  # ggsave(plot = plotSpeciesResidence, filename = here("figures", "distributionOriginPlot.png"),
  #        width = 400, height = 200, units = "mm", dpi = 300)

  return(list(allFacets,
              plotList))

}
