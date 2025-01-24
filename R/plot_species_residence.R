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
                                   allDistributionDataList){

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
  # targets::tar_load("allDistributionDataList")
  # targets::tar_load("extraManualDistributionDataList")

  lemisData <- lemisDataRenamed %>%
    filter(import_export == "I") %>%
    filter(rank == "Species" & !corrected == "") %>%
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

  lemisData[which(is.na(lemisData$iso2) & lemisData$country_origin == "Ctry_NA"),]$iso2 <- "NA"

  grpVector <- c("Birds", "Terrestrial Mammals", "Amphibians", "Reptiles")

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

    if(grp == "Birds"){
      distData <- allDistributionDataList$birds
    } else if(grp == "Terrestrial Mammals"){
      distData <- allDistributionDataList$mammals
    } else if(grp == "Amphibians"){
      distData <- allDistributionDataList$amphibians
    } else if(grp == "Reptiles"){
      distData <- allDistributionDataList$reptiles
    }

    lemisGroup_dist <- lemisGroup %>%
      filter(!corrected == "") %>%
      left_join(distData %>%
                  filter(!is.na(distISO2) & !distISO2 == "<NA>"),
                by = "corrected")

    noDistSpp <- lemisGroup_dist %>%
      filter(is.na(distISO2) | distISO2 == "<NA>") %>%
      pull(corrected) %>% unique()
    noDistSpp <- noDistSpp[!noDistSpp == ""]
    stillMissingList[[grpL]] <- noDistSpp
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
      mutate(nativeOrigin = str_detect(distISO2, iso2))

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

    world_gg <- ggplot() +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey85") +
      geom_polygon(data = worldDataList$worldData, aes(x = long, y = lat, group = group),
                   fill = "grey50", colour = "grey35", linewidth = 0.025) +
      geom_polygon(data = worldDataList$wrappedData, aes(x = long, y = lat, group = group),
                   fill = "grey50", colour = "grey35", linewidth = 0.025)

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

    completePlot <-
      world_gg +
      geom_curve(data = routeLineData,
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     xend = if_else(port_lon > 90, -180-(180-port_lon), port_lon),
                     y = centre_y_origin,
                     yend = port_lat,
                     linewidth = nSpp,
                     alpha = nSpp),
                 position = position_jitter(width = 0.5, height = 0.5),
                 curvature = -0.5,
                 angle = 140,
                 colour = groupColour
      ) +

      geom_point(data = portLocations,
                 aes(x = port_lon, y = port_lat),
                 size = 0.5,
                 shape = 21,
                 colour = "white",
                 fill = groupColourDark
                 # fill = "black"
      ) +
      geom_point(data = lemisGroup_plotdata %>%
                   mutate(scaledSpp = scale(nSpp_total, center = FALSE)* radiusModifier) %>%
                   filter(scaledSpp < 5),
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     y = centre_y_origin),
                 size = 0.5,
                 shape = 21,
                 colour = "white",
                 fill = groupColour
      ) +
      geom_scatterpie(data = lemisGroup_plotdata,
                      aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                          y = centre_y_origin, group = iso2,
                          r = scale(nSpp_total, center = FALSE)* radiusModifier),
                      cols = c("Resident", "NonResident", "Unknown"), color = NA, alpha = 0.8) +
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
        legend.position = c(1, 0),
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
        legend.title.align = 0,
        legend.title = element_markdown(lineheight = 0.85, margin = margin(0,0,0,0))
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
               "Amphibians"
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
