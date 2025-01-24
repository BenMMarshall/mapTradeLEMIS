#' plot_species_origin
#'
#' @name plot_species_origin
#' @description A number of cunctiont aht read in the required files to correct and update the names in the LEMIS dataset.
#' @return The file as a dataframe, ready for downstream functions.
#'
#' @export
plot_species_origin <- function(lemisDataCorrected,
                                portLocations,
                                paletteList,
                                lemisISOconversion,
                                worldDataList){
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
           vert = factor(vert, levels = c("Vertebrates", "Invertebrates")))

  plotList <- vector("list", length = length(unique(mapDataCorrected$group_)))
  names(plotList) <- unique(mapDataCorrected$group_)
  for(grp in unique(mapDataCorrected$group_)){
    # grp <- unique(mapDataCorrected$group_)[1]
    routeSummarySpecies <- mapDataCorrected %>%
      filter(group_ == grp) %>%
      group_by(port, iso2, country_name) %>%
      summarise(nSpp = n_distinct(corrected))

    worldRouteSummarySpecies <- routeSummarySpecies %>%
      ungroup() %>%
      left_join(worldDataList$worldDataCountryCentres) %>%
      left_join(portLocations)

    # write.csv(worldRouteSummarySpecies, file = here("tables", paste0("routesSpecies_", grp, ".csv")),
    #           row.names = FALSE)

    routeRange <- range(worldRouteSummarySpecies$nSpp)

    originSpecies <- mapDataCorrected %>%
      filter(group_ == grp) %>%
      group_by(iso2, country_name) %>%
      summarise(nSpp = n_distinct(corrected)) %>%
      ungroup() %>%
      left_join(worldDataList$worldDataCountryCentres)

    # write.csv(originSpecies, file = here("tables", paste0("originSpecies_", grp, ".csv")),
    #           row.names = FALSE)

    routesTop <- worldRouteSummarySpecies %>%
      filter(!is.na(iso2)) %>%
      ungroup() %>%
      slice_max(nSpp, n = 5) %>%
      mutate(routeSummaryLab = paste0(country_name, " to ", portName, ": ",
                                      nSpp))


    portSpecies <- worldRouteSummarySpecies %>%
      group_by(port, port_lon, port_lat) %>%
      summarise(nSpp = sum(nSpp))

    originTop <- originSpecies %>%
      filter(!is.na(iso2)) %>%
      ungroup() %>%
      mutate(max = max(nSpp),
             percent2top = nSpp/max) %>%
      filter(percent2top > 0.5) %>%
      mutate(lab = nSpp) %>%
      left_join(worldDataList$worldDataCountryCentres)

    # portTop <- portSpecies %>%
    #   ungroup() %>%
    #   slice_max(nSpp, n = 3)

    pointRange <- range(c(originSpecies$nSpp, portSpecies$nSpp))

    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == grp) %>%
      pull(group_colour)

    groupColourDark <- darken(groupColour,
                              amount = 0.35)

    titleColoured <- paletteList$groupPaletteDF %>%
      mutate(title = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      filter(group_ == grp) %>%
      pull(title)

    # imageLocation <- data.frame(x = -150, y = -50,
    #                             # image = here("data", "publicDomainAnimals", paste0(grp, ".svg")))
    #                             image = here("data", "publicDomainAnimals", paste0("Frog", ".svg")))
    # worldRouteSummarySpecies_NAPort <- worldRouteSummarySpecies %>%
    #   filter(!is.na(portName))

    worldRouteSummarySpecies <- worldRouteSummarySpecies %>%
      filter(!is.na(portName))

    routePlot <- ggplot() +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey85") +
      geom_polygon(data = worldDataList$worldData, aes(x = long, y = lat, group = group),
                   fill = "grey50", colour = "grey35", linewidth = 0.025) +
      geom_polygon(data = worldDataList$wrappedData, aes(x = long, y = lat, group = group),
                   fill = "grey50", colour = "grey35", linewidth = 0.025) +
      geom_curve(data = worldRouteSummarySpecies,
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     xend = if_else(port_lon > 90, -180-(180-port_lon), port_lon),
                     y = centre_y_origin,
                     yend = port_lat,
                     linewidth = nSpp,
                     alpha = nSpp),
                 position = position_jitter(width = 0.5, height = 0.5),
                 curvature = -0.5,
                 angle = 140,
                 colour = groupColour) +
      geom_point(data = originSpecies,
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     y = centre_y_origin,
                     size = nSpp
                 ),
                 shape = 21,
                 colour = "white", fill = groupColour) +
      geom_text(data = originTop,
                aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                    y = centre_y_origin, label = iso2
                ),
                colour = "white", size = 1.75, fontface = 2) +
      geom_point(data = portSpecies,
                 aes(x = if_else(port_lon > 90, -180-(180-port_lon), port_lon),
                     y = port_lat,
                     size = nSpp),
                 shape = 21,
                 colour = "white", fill = groupColourDark) +
      geom_text_repel(data = originTop,
                      aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                          y = centre_y_origin, label = lab
                      ),
                      colour = "grey15", size = 1.75, fontface = 2) +
      annotate("richtext", x = 89, y = -83,
               label = paste0("<b style = 'font-size:8pt'>Top 5 Routes</b><br><i style = 'font-size:6pt'>",
                              paste0(routesTop$routeSummaryLab, collapse = "<br>"),
                              "</i>"),
               hjust = 1, vjust = 0, lineheight = 0.92) +
      coord_cartesian(xlim = c(-180-90, 90),
                      ylim = c(-84, 84),
                      expand = 0) +
      scale_linewidth_continuous(range = c(0.15, 4), breaks = routeRange) +
      scale_size_continuous(range = c(0.15, 5), breaks = pointRange) +
      scale_alpha_continuous(range = c(0.1, 0.5), breaks = scales::pretty_breaks(n = 5)) +
      labs(title = titleColoured,
           size = "<b style = 'font-size:8pt'># Species</b><br><i style = 'font-size:6pt'>Origin and Port</i>",
           linewidth = "<i style = 'font-size:6pt'>Origin to Port Route</i>",
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
        legend.position = c(0, 0),
        legend.background = element_rect(fill = "white"),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(0.5, "mm"),
        legend.key.height = unit(0.25, "mm"),
        legend.key.width = unit(0.5, "mm"),
        legend.spacing = unit(0.1, "mm"),
        legend.box.margin = margin(0,0,0,0),
        legend.text = element_text(margin = margin(0,0,0,0), size = 5),
        legend.box.background = element_rect(fill = "white", colour = "white", linewidth = 2),
        legend.justification = c(0,0),
        legend.direction = "horizontal",
        # legend.title.align = 0,
        legend.title = element_markdown(lineheight = 0.85, margin = margin(0,0,0,0),
                                        hjust = 0)
      ) +
      # geom_image(data = imageLocation,
      #            aes(x = x, y = y,
      #                image = image), size = 0.15) +
      guides(
        linewidth = guide_legend(override.aes = list(linewidth = c(0.5,4)),
                                 order = 2, nrow = 1,
                                 direction = "horizontal",
                                 title.position = "top",
                                 margin = margin(0,0,0,0)),
        alpha = guide_none(),
        # alpha = guide_legend(order = 2, nrow = 1,
        #                      direction = "horizontal",
        #                      title.position = "top",
        #                      margin = margin(0,0,0,0)),
        size = guide_legend(override.aes = list(size = c(1,5)),
                            order = 1, nrow = 1,
                            direction = "horizontal",
                            title.position = "top",
                            margin = margin(0,0,0,0))
      )

    routePlot

    plotList[[grp]] <- routePlot

  }

  allFacets <- patchwork::wrap_plots(
    plotList[c("Terrestrial Mammals",
               "Marine Mammals",
               "Birds",
               "Reptiles",
               "Amphibians",
               "Fish",
               "Arachnids",
               "Lepidoptera",
               "Insecta and Myriapoda",
               "Crustaceans and Molluscs",
               "Other Invertebrates",
               "Echinoderms and Cnidaria",
               "Porifera Sponges, Bryozoa, and Squirts",
               "Plants"
    )],
    ncol = 3)

  ggsave(filename = here("figures", "routePlot_species.png"),
         width = 600, height = 600, units = "mm", dpi = 300)
  ggsave(filename = here("figures", "routePlot_species.pdf"),
         width = 600, height = 600, units = "mm")

  return(allFacets)
}
