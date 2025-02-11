#' plot_iucn_summary
#'
#' @name plot_iucn_summary
#' @description Plots IUCN summary data
#' @return ggplot2 object
#'
#' @export
plot_iucn_summary <- function(iucnSummary, paletteList, allDistributionData, worldDataList){

  # targets::tar_load("iucnSummary")
  # targets::tar_load("paletteList")
  # targets::tar_load("allDistributionData")
  # targets::tar_load("worldDataList")
  # library(ggplot2)
  # library(ggtext)
  # library(ggimage)
  # library(stringr)
  # library(colorspace)
  # library(dplyr)
  # library(here)
  mapList <- vector("list", length = 4)
  names(mapList) <- unique(iucnSummary$group_)
  for(grp in unique(iucnSummary$group_)){
    # grp <- "Mammals"

    # colour settings
    groupColour <- paletteList$groupPaletteDF %>%
      filter(group_ == ifelse(grp == "Mammals", "Terrestrial Mammals", grp)) %>%
      pull(group_colour)

    groupColourLight <- lighten(groupColour,
                                amount = 0.35)
    groupColourDark <- darken(groupColour,
                              amount = 0.35)
    titleColoured <- paletteList$groupPaletteDF %>%
      mutate(title = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
      filter(group_ == ifelse(grp == "Mammals", "Terrestrial Mammals", grp)) %>%
      pull(title)

    grpL <- str_to_lower(ifelse(grp == "Terrestrial Mammals", "mammals", grp))
    iucnFull <- read.csv(here("data", "iucnData", paste0("fullIUCNdata_traded_",
                                                         stringr::str_to_lower(grpL),
                                                         ".csv")), na.strings = c("<NA>"))

    iucnDist <- iucnFull %>%
      select(redlistCategory, corrected, group_, traded) %>%
      left_join(allDistributionData %>% select(-group_), by = "corrected") %>%
      rename("iucn" = redlistCategory) %>%
      mutate(
        # iucnSimp = sub("\\).*", "", sub(".*\\(", "", iucn)),
        # iucn = sub(".{5}$", "", iucn),
        colour = dplyr::case_when(
          iucn == "Least Concern" ~ "#309706",
          iucn == "Near Threatened" ~ "#A8DB06",
          iucn == "Vulnerable" ~ "#F5D800",
          iucn == "Endangered" ~ "#DC7000",
          iucn == "Critically Endangered" ~ "#CB1500",
          iucn == "Extinct in the Wild" ~ "#701E08",
          iucn == "Extinct" ~ "#000000",
          iucn == "Data Deficient" ~ "#717171",
          is.na(iucn) ~ "#BFBFBF"),
        iucnSimp = dplyr::case_when(
          iucn == "Least Concern" ~ "LC",
          iucn == "Near Threatened" ~ "NT",
          iucn == "Vulnerable" ~ "VU",
          iucn == "Endangered" ~ "EN",
          iucn == "Critically Endangered" ~ "CR",
          iucn == "Extinct in the Wild" ~ "EW",
          iucn == "Extinct" ~ "EX",
          iucn == "Data Deficient" ~ "DD",
          is.na(iucn) ~ "NA")) %>%
      mutate(iucnCol = glue::glue("{iucn} <b style='color:{colour}'>({iucnSimp})</b>")) %>%
      mutate(threat = case_when(
        iucnCol %in% c(
          "Extinct in the Wild <b style='color:#701E08'>(EW)</b>",
          "Critically Endangered <b style='color:#CB1500'>(CR)</b>",
          "Endangered <b style='color:#DC7000'>(EN)</b>",
          "Vulnerable <b style='color:#F5D800'>(VU)</b>") ~ "Threatened"
      )) %>%
      filter(threat == "Threatened", traded == "Traded")

    tableCountryCounts <- as.data.frame(table(unlist(str_split(iucnDist$allDistISO2, ", ")))) %>%
      filter(!Var1 == "") %>%
      rename("iso2" = Var1)
    tableCountryCounts$group_ <- grp

    worldData_end <- worldDataList$worldData %>%
      left_join(tableCountryCounts,
                by = c("iso2" = "iso2"))
    wrappedData_end <- worldDataList$wrappedData %>%
      left_join(tableCountryCounts,
                by = c("iso2" = "iso2"))

    world_gg <- ggplot() +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey85") +
      geom_polygon(data = worldData_end, aes(x = long, y = lat, group = group,
                                             fill = Freq),
                   # fill = "grey50",
                   colour = "grey35", linewidth = 0.025) +
      geom_polygon(data = wrappedData_end, aes(x = long, y = lat, group = group,
                                               fill = Freq),
                   # fill = "grey50",
                   colour = "grey35", linewidth = 0.025) +
      coord_cartesian(xlim = c(-180-90, 90),
                      ylim = c(-84, 84),
                      expand = 0) +
      # scale_fill_viridis_d() +
      scale_fill_gradient(high = groupColourDark, low = groupColourLight) +
      # scale_linewidth_continuous(range = c(0.15, 4), breaks = routeRange) +
      scale_size_continuous(range = c(1, 5)) +
      # scale_alpha_continuous(range = c(0.1, 0.5), breaks = scales::pretty_breaks(n = 5)) +
      labs(title = titleColoured,
           fill = "<i style = 'font-size:6pt'>Number of traded threatened species</i>") +
      theme_bw() +
      theme(
        aspect.ratio = 0.5,
        # plot.title = element_markdown(size = 12),
        plot.title = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = 4, hjust = 0, vjust = 0),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        # legend.position = c(1, 0),
        legend.position = "none",
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

    mapList[[grp]] <- world_gg

  }

  allFacets <- patchwork::wrap_plots(
    mapList[c("Mammals",
              "Birds",
              "Reptiles",
              "Amphibians"
    )],
    ncol = 1)

  iucnSummary_colour <- iucnSummary %>%
    ungroup() %>%
    mutate(redlistCategory = as.character(redlistCategory)) %>%
    rename("iucn" = redlistCategory) %>%
    mutate(
      # iucnSimp = sub("\\).*", "", sub(".*\\(", "", iucn)),
      # iucn = sub(".{5}$", "", iucn),
      colour = dplyr::case_when(
        iucn == "Least Concern" ~ "#309706",
        iucn == "Near Threatened" ~ "#A8DB06",
        iucn == "Vulnerable" ~ "#F5D800",
        iucn == "Endangered" ~ "#DC7000",
        iucn == "Critically Endangered" ~ "#CB1500",
        iucn == "Extinct in the Wild" ~ "#701E08",
        iucn == "Extinct" ~ "#000000",
        iucn == "Data Deficient" ~ "#717171",
        is.na(iucn) ~ "#BFBFBF"),
      iucnSimp = dplyr::case_when(
        iucn == "Least Concern" ~ "LC",
        iucn == "Near Threatened" ~ "NT",
        iucn == "Vulnerable" ~ "VU",
        iucn == "Endangered" ~ "EN",
        iucn == "Critically Endangered" ~ "CR",
        iucn == "Extinct in the Wild" ~ "EW",
        iucn == "Extinct" ~ "EX",
        iucn == "Data Deficient" ~ "DD",
        is.na(iucn) ~ "NA")) %>%
    mutate(iucnCol = glue::glue("{iucn} <b style='color:{colour}'>({iucnSimp})</b>")) %>%
    mutate(iucnCol = factor(iucnCol, levels = c(
      "NA <b style='color:#BFBFBF'>(NA)</b>",
      "Data Deficient <b style='color:#717171'>(DD)</b>",
      "Extinct <b style='color:#000000'>(EX)</b>",
      "Extinct in the Wild <b style='color:#701E08'>(EW)</b>",
      "Critically Endangered <b style='color:#CB1500'>(CR)</b>",
      "Endangered <b style='color:#DC7000'>(EN)</b>",
      "Vulnerable <b style='color:#F5D800'>(VU)</b>",
      "Near Threatened <b style='color:#A8DB06'>(NT)</b>",
      "Least Concern <b style='color:#309706'>(LC)</b>"
    ))) %>%
    mutate(group_ = case_when(group_ %in% c("Terrestrial Mammals", "Marine Mammals") ~ "Mammals",
                              TRUE ~ group_))

  iucnSummary_colour <- iucnSummary_colour %>%
    ungroup() %>%
    mutate(nSpecies = ifelse(traded == "Not present", -count, count)) %>%
    tidyr::complete(traded, iucnCol,
                    fill = list(nSpecies = 0)) %>%
    filter(!is.na(nSpp))  %>%
    left_join(paletteList$groupPaletteDF %>%
                mutate(group_ = case_when(group_ == "Terrestrial Mammals" ~ "Mammals",
                                          TRUE ~ group_))) %>%
    mutate(group_col = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
    mutate(group_col = factor(group_col, levels = c(
      "<b><i style = 'font-size:16pt; color:#5BC0BE'>Amphibians</i></b>",
      "<b><i style = 'font-size:16pt; color:#CECE85'>Mammals</i></b>",
      "<b><i style = 'font-size:16pt; color:#9EB78D'>Birds</i></b>",
      "<b><i style = 'font-size:16pt; color:#4e862d'>Reptiles</i></b>"
    )))

  # cols <- scico::scico(2, palette = "roma")

  tots <- iucnSummary_colour %>%
    group_by(traded, group_, group_col) %>%
    summarise(Tot = abs(sum(nSpp, na.rm = TRUE)))

  threatenedData <- iucnSummary_colour %>%
    group_by(group_, group_col) %>%
    mutate(threat = case_when(
      iucnCol %in% c(
        "Extinct in the Wild <b style='color:#701E08'>(EW)</b>",
        "Critically Endangered <b style='color:#CB1500'>(CR)</b>",
        "Endangered <b style='color:#DC7000'>(EN)</b>",
        "Vulnerable <b style='color:#F5D800'>(VU)</b>") ~ "Threatened"
    )) %>%
    filter(threat == "Threatened", traded == "Traded") %>%
    summarise(treatCount = sum(count)) %>%
    mutate(label = paste0(treatCount, "\nthreatened\nspecies"))

  threatenedDataSegments <- iucnSummary_colour %>%
    group_by(iucnCol, group_, group_col) %>%
    slice_max(nSpecies, n = 1) %>%
    filter(iucnCol %in% c("Extinct in the Wild <b style='color:#701E08'>(EW)</b>",
                          "Vulnerable <b style='color:#F5D800'>(VU)</b>"))

  imageLocation <- data.frame(x = 600, y = 5.5,
                              group_ = c("Amphibians", "Birds", "Mammals", "Reptiles"),
                              imageLoc = c(here("data", "publicDomainAnimals", paste0("Amphibians", ".svg")),
                                           here("data", "publicDomainAnimals", paste0("Birds", ".svg")),
                                           here("data", "publicDomainAnimals", paste0("Terrestrial Mammals", ".svg")),
                                           here("data", "publicDomainAnimals", paste0("Reptiles", ".svg")))) %>%
    left_join(paletteList$groupPaletteDF %>%
                mutate(group_ = case_when(group_ == "Terrestrial Mammals" ~ "Mammals",
                                          TRUE ~ group_))) %>%
    mutate(group_col = glue::glue("<b><i style = 'font-size:16pt; color:{group_colour}'>{group_}</i></b>")) %>%
    mutate(group_col = factor(group_col, levels = c(
      "<b><i style = 'font-size:16pt; color:#5BC0BE'>Amphibians</i></b>",
      "<b><i style = 'font-size:16pt; color:#4e862d'>Reptiles</i></b>",
      "<b><i style = 'font-size:16pt; color:#9EB78D'>Birds</i></b>",
      "<b><i style = 'font-size:16pt; color:#CECE85'>Mammals</i></b>"
    )))


  iucnPlotDataCore <- iucnSummary_colour %>%
    mutate(hjust = ifelse(nSpecies < 0, 1, 0),
           vjust = 0.5,
           labPos = ifelse(nSpecies > 1200, Inf, ifelse(nSpecies < -1200, -Inf, nSpecies))) %>%
    mutate(hjust = ifelse(labPos == Inf, 1, hjust),
           hjust = ifelse(labPos == -Inf, 0, hjust),
           vjust = ifelse(labPos == Inf | labPos == -Inf, 0, vjust)
    )

  iucnPlotDataLCDD <- iucnPlotDataCore %>%
    filter(iucnSimp %in% c("LC"))

  iucnSummaryPlot <- iucnPlotDataCore %>%
    filter(!iucnSimp %in% c("LC")) %>%
    mutate(group_col = factor(group_col, levels = c(
      "<b><i style = 'font-size:16pt; color:#CECE85'>Mammals</i></b>",
      "<b><i style = 'font-size:16pt; color:#9EB78D'>Birds</i></b>",
      "<b><i style = 'font-size:16pt; color:#4e862d'>Reptiles</i></b>",
      "<b><i style = 'font-size:16pt; color:#5BC0BE'>Amphibians</i></b>"
    ))) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    geom_col(aes(x = nSpecies, y = iucnCol,
                 colour = iucnCol, fill = iucnCol), width = 0.25) +
    geom_point(aes(x = nSpecies, y = iucnCol, colour = iucnCol)) +
    geom_segment(data = threatenedDataSegments,
                 aes(x = nSpecies +100, xend = 400, y = iucnCol),
                 colour = paletteList$paletteWildCap["Other"]) +
    annotate("segment", x = 400, xend = 400, y = 3, yend = 6,
             colour = paletteList$paletteWildCap["Other"]) +
    # annotate("segment", x = 200, xend = 1000, y = 6, yend = 6) +
    # annotate("segment", x = 100, xend = 1000, y = 3, yend = 3) +
    geom_richtext(aes(x = labPos,
                      y = iucnCol, label = abs(nSpecies),
                      hjust = hjust, vjust = vjust,
                      colour = iucnCol),
                  fill = NA,
                  # colour = "white",
                  label.color = NA,
                  lineheight = 1.6,
                  fontface = 2,
                  # vjust = 0.5,
                  label.padding = grid::unit(rep(2, 4), "pt")) +
    geom_label(data = threatenedData,
               aes(x = 402, y = 6, label = label),
               hjust = 0, vjust = 1, fontface = 2, lineheight = 0.875, label.size = 0) +

    # geom_image(data = imageLocation,
    #            aes(x = x, y = y,
    #                image = imageLoc), size = 0.45) +

    facet_grid(rows = vars(group_col)) +

    # annotate("segment", x = -50, xend = -500, y = 10, yend = 10,
    #          size = 2, arrow = arrow(angle = 30, type = "closed",
    #                                  length = unit(3, "mm")),
    #          colour = cols[2]) +
    # annotate("text", x = -550, y = 10,
    #          label = paste(tots[1], ":Not traded"),
    #          vjust = 0.5, hjust = 1, fontface = 4) +
    # annotate("segment", x = 50, xend = 500, y = 10, yend = 10,
    #          size = 2, arrow = arrow(angle = 30, type = "closed",
    #                                  length = unit(3, "mm")),
    #          colour = cols[1]) +
    # annotate("text", x = 550, y = 10, label = paste("Traded:", tots[2]),
    #          vjust = 0.5, hjust = 0, fontface = 4) +

    labs(fill = "IUCN RedList\nStatus",
         alpha = "Traded",
         x = "< Not traded | Traded >       ",
         y = "") +
    scale_y_discrete(expand = expansion(c(0,0), c(1,1.5))) +
    scale_x_continuous(expand = expansion(c(0,0), c(100,20)), position = "top",
                       sec.axis = dup_axis(),
                       breaks = seq(-3000, 1500, 500),
                       labels = c(seq(3000, 0, -500), seq(500, 1500, 500))) +
    coord_cartesian(xlim = c(-1200, 900)) +
    scale_colour_manual(values = str_extract(unique(iucnSummary_colour$iucnCol)[1:7], "\\#.{6}"))+
    scale_fill_manual(values = str_extract(unique(iucnSummary_colour$iucnCol)[1:7], "\\#.{6}"))+
    theme_bw() +
    theme(
      line = element_line(colour = "#080808"),
      text = element_text(colour = "#080808"),
      axis.title = element_text(face = 2, hjust = 0.5),
      axis.title.y = element_text(hjust = 0, vjust = 1, angle = 0),
      axis.title.x = element_text(margin = margin(5,0,5,0), hjust = 0.58),
      axis.title.x.top = element_text(margin = margin(5,0,5,0), hjust = 0.58),
      legend.title = element_text(face = 2),
      legend.position = "none",
      axis.text.y = element_markdown(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_markdown(face = 4, hjust = 0),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      panel.grid = element_blank(),
      plot.margin = margin(0,0,0,0))

  (combinedPlot <- patchwork::wrap_plots(iucnSummaryPlot, allFacets,
                                         ncol = 2, widths = c(6,4)))

  ggsave(plot = combinedPlot, filename = here("figures", "IUCNsummaryPlot.png"),
         width = 300, height = 220,
         dpi = 300, units = "mm")
  ggsave(plot = combinedPlot, filename = here("figures", "IUCNsummaryPlot.pdf"),
         width = 300, height = 220,
         units = "mm")

  return(iucnSummaryPlot)

}
