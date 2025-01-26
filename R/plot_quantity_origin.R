#' plot_quantity_origin
#'
#' @name plot_quantity_origin
#' @description A number of cunctiont aht read in the required files to correct and update the names in the LEMIS dataset.
#' @return The file as a dataframe, ready for downstream functions.
#'
#' @export
plot_quantity_origin <- function(lemisDataFiltered,
                                portLocations,
                                paletteList,
                                lemisISOconversion,
                                worldDataList){

  # targets::tar_load("lemisDataFiltered")

  mapDataFiltered <- lemisDataFiltered

  plotList <- vector("list", length = length(unique(mapDataFiltered$group_)))
  names(plotList) <- unique(mapDataFiltered$group_)
  for(grp in unique(mapDataFiltered$group_)){
    # grp <- unique(mapDataFiltered$group_)[1]
    print(grp)

    routeSummaryQuantity <- mapDataFiltered %>%
      filter(group_ == grp) %>%
      group_by(port, iso2, country_name) %>%
      summarise(nWhole = sum(quantity))

    worldRouteSummaryQuantity <- routeSummaryQuantity %>%
      left_join(worldDataList$worldDataCountryCentres) %>%
      left_join(portLocations)

    # write.csv(worldRouteSummaryQuantity, file = here("tables", paste0("routesQuantity_", grp, ".csv")),
    #           row.names = FALSE)

    routeRange <- range(worldRouteSummaryQuantity$nWhole)

    originQuantity <- mapDataFiltered %>%
      filter(group_ == grp) %>%
      group_by(iso2, country_name) %>%
      summarise(nWhole = sum(quantity)) %>%
      ungroup() %>%
      left_join(worldDataList$worldDataCountryCentres)

    # write.csv(originQuantity, file = here("tables", paste0("originQuantity_", grp, ".csv")),
    #           row.names = FALSE)

    routesTop <- worldRouteSummaryQuantity %>%
      ungroup() %>%
      slice_max(nWhole, n = 5) %>%
      mutate(routeSummaryLab = paste0(country_name, " to ", portName, ": ",
                                      paste(format(round(nWhole / 1e6, 1), trim = TRUE), "M")))

    portQuantity <- worldRouteSummaryQuantity %>%
      group_by(port, port_lon, port_lat) %>%
      summarise(nWhole = sum(nWhole))

    originTop <- originQuantity %>%
      filter(!is.na(iso2)) %>%
      ungroup() %>%
      mutate(max = max(nWhole),
             percent2top = nWhole/max) %>%
      filter(percent2top > 0.5) %>%
      mutate(labMillion = paste(format(round(nWhole / 1e6, 1), trim = TRUE), "M"))

    # portTop <- portSpecies %>%
    #   ungroup() %>%
    #   slice_max(nSpp, n = 3)

    pointRange <- range(c(originQuantity$nWhole, portQuantity$nWhole))

    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == grp) %>%
      pull(group_colour)

    groupColourDark <- darken(groupColour,
                              amount = 0.35)

    titleColoured <- paletteList$groupPaletteDF %>%
      mutate(title = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      filter(group_ == grp) %>%
      pull(title)

    imageLocation <- data.frame(x = -150, y = -50,
                                # image = here("data", "publicDomainAnimals", paste0(grp, ".svg")))
                                image = here("data", "publicDomainAnimals", paste0("Frog", ".svg")))

    worldRouteSummaryQuantity <- worldRouteSummaryQuantity %>%
      filter(!is.na(portName))

    routePlot <- ggplot() +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey85") +
      geom_polygon(data = worldDataList$worldData, aes(x = long, y = lat, group = group),
                   fill = "grey50", colour = "grey35", linewidth = 0.025) +
      geom_polygon(data = worldDataList$wrappedData, aes(x = long, y = lat, group = group),
                   fill = "grey50", colour = "grey35", linewidth = 0.025) +
      geom_curve(data = worldRouteSummaryQuantity,
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     xend = if_else(port_lon > 90, -180-(180-port_lon), port_lon),
                     y = centre_y_origin,
                     yend = port_lat,
                     linewidth = nWhole,
                     alpha = nWhole),
                 position = position_jitter(width = 0.5, height = 0.5),
                 curvature = -0.5,
                 angle = 140,
                 colour = groupColour) +
      geom_point(data = originQuantity,
                 aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                     y = centre_y_origin,
                     size = nWhole
                 ),
                 shape = 21,
                 colour = "white", fill = groupColour) +
      geom_text(data = originTop,
                aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                    y = centre_y_origin, label = iso2
                ),
                colour = "white", size = 1.75, fontface = 2) +
      geom_point(data = portQuantity,
                 aes(x = port_lon, y = port_lat,
                     size = nWhole),
                 shape = 21,
                 colour = "white", fill = groupColourDark) +
      geom_text_repel(data = originTop,
                      aes(x = if_else(centre_x_origin > 90, -180-(180-centre_x_origin), centre_x_origin),
                          y = centre_y_origin, label = labMillion
                      ),
                      colour = "grey15", size = 1.75, fontface = 2) +
      annotate("richtext", x = 89, y = -83,
               label = paste0("<b style = 'font-size:8pt'>Top 5 Routes</b><br><i style = 'font-size:6pt'>",
                              paste0(routesTop$routeSummaryLab, collapse = "<br>"),
                              "</i>"),
               hjust = 1, vjust = 0, linehieght = 0.92) +
      coord_cartesian(xlim = c(-180-90, 90),
                      ylim = c(-84, 84),
                      expand = 0) +
      scale_linewidth_continuous(range = c(0.15, 4), breaks = routeRange) +
      scale_size_continuous(range = c(0.15, 5), breaks = pointRange) +
      scale_alpha_continuous(range = c(0.1, 0.5), breaks = scales::pretty_breaks(n = 5)) +
      labs(title = titleColoured,
           size = "<b style = 'font-size:8pt'># Whole Individuals</b><br><i style = 'font-size:6pt'>Origin and Port</i>",
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
        legend.title.align = 0,
        legend.title = element_markdown(lineheight = 0.85, margin = margin(0,0,0,0))
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

  ggsave(filename = here("figures", "routePlot_wholeIndividuals.png"),
         width = 600, height = 600, units = "mm", dpi = 300)
  ggsave(filename = here("figures", "routePlot_wholeIndividuals.pdf"),
         width = 600, height = 600, units = "mm")

  return(allFacets)
}
