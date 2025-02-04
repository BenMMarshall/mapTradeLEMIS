#' plot_iucn_summary
#'
#' @name plot_iucn_summary
#' @description Plots IUCN summary data
#' @return ggplot2 object
#'
#' @export
plot_iucn_summary <- function(iucnSummary, paletteList){

  # targets::tar_load("iucnSummary")
  # targets::tar_load("paletteList")
  # library(ggplot2)
  # library(ggtext)
  # library(ggimage)
  # library(stringr)
  # library(dplyr)
  # library(here)

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

  imageLocation <- data.frame(x = 800, y = 5.5,
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
      "<b><i style = 'font-size:16pt; color:#CECE85'>Mammals</i></b>",
      "<b><i style = 'font-size:16pt; color:#9EB78D'>Birds</i></b>",
      "<b><i style = 'font-size:16pt; color:#4e862d'>Reptiles</i></b>"
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
    filter(iucnSimp %in% c("LC", "DD"))

  iucnSummaryPlot <- iucnPlotDataCore %>%
    filter(!iucnSimp %in% c("LC", "DD")) %>%
    mutate(group_col = factor(group_col, levels = c(
      "<b><i style = 'font-size:16pt; color:#4e862d'>Reptiles</i></b>",
      "<b><i style = 'font-size:16pt; color:#CECE85'>Mammals</i></b>",
      "<b><i style = 'font-size:16pt; color:#9EB78D'>Birds</i></b>",
      "<b><i style = 'font-size:16pt; color:#5BC0BE'>Amphibians</i></b>"
    ))) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    geom_col(aes(x = nSpecies, y = iucnCol,
                 colour = iucnCol, fill = iucnCol), width = 0.25) +
    geom_point(aes(x = nSpecies, y = iucnCol, colour = iucnCol)) +
    geom_segment(data = threatenedDataSegments,
                 aes(x = nSpecies +100, xend = 500, y = iucnCol),
                 colour = paletteList$paletteWildCap["Other"]) +
    annotate("segment", x = 500, xend = 500, y = 2, yend = 5,
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
               aes(x = 510, y = 5, label = label),
               hjust = 0, vjust = 1, fontface = 2, lineheight = 0.875, label.size = 0) +

    geom_image(data = imageLocation,
               aes(x = x, y = y,
                   image = imageLoc), size = 0.45) +

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
         x = "\u2B9C Not traded | Traded \u2B9E       ",
         y = "") +
    scale_y_discrete(expand = expansion(c(0,0), c(1,1.5))) +
    scale_x_continuous(expand = expansion(c(0,0), c(50,50)), position = "top",
                       sec.axis = dup_axis(),
                       breaks = seq(-3000, 1500, 500),
                       labels = c(seq(3000, 0, -500), seq(500, 1500, 500))) +
    coord_cartesian(xlim = c(-1200, 900)) +
    scale_colour_manual(values = str_extract(unique(iucnSummary_colour$iucnCol)[2:7], "\\#.{6}"))+
    scale_fill_manual(values = str_extract(unique(iucnSummary_colour$iucnCol)[2:7], "\\#.{6}"))+
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
      panel.grid = element_blank())

  iucnSummaryPlot

  ggsave(plot = iucnSummaryPlot, filename = here("figures", "IUCNsummaryPlot.png"),
         width = 300, height = 220,
         dpi = 300, units = "mm")
  ggsave(plot = iucnSummaryPlot, filename = here("figures", "IUCNsummaryPlot.pdf"),
         width = 300, height = 220,
         units = "mm")

  return(iucnSummaryPlot)

}
