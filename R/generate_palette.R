#' generate_palette
#'
#' @name generate_palette
#' @return a
#'
#' @export
generate_palette <- function(){

  palette <- c("#281231", "#A4778B", "#7E3E1B", "#B55727", "#F58234",
               "#FEBF55", "#CECE85", "#9EB78D", "#4e862d", "#5BC0BE", "#068D9D",
               # "#FEBF55", "#CECE85", "#9EB78D", "#73975D", "#5BC0BE", "#068D9D",
               "#1C6E8C", "#324B64", "#3E640D", "#808080")

  groupPalette <- palette
  names(groupPalette) <- c(
    "Echinoderms and Cnidaria",
    "Crustaceans and Molluscs",
    "Arachnids",
    "Insecta and Myriapoda",
    "Lepidoptera",
    "Other Invertebrates",
    "Terrestrial Mammals",
    "Birds",
    "Reptiles",
    "Amphibians",
    "Fish",
    "Marine Mammals",
    "Porifera Sponges, Bryozoa, and Squirts",
    "Plants",
    "Miscellaneous")

  groupPaletteDF <- data.frame(
    group_ = names(groupPalette), group_colour = palette
  )

  names(groupPalette) <- groupPaletteDF %>%
    mutate(group_ = glue::glue("<b style='color:{group_colour}'>{group_}</b>")) %>%
    pull(group_)

  paletteWildCap <- c("#7e3d11", "#fa8334", "#388697", "#808080")
  names(paletteWildCap) <- c("Wild", "Ranched", "Captive", "Other")

  purposePalVector <- c("#7e3d11", "#b45618", "#fa8334", "#FDC056", "#E6C86F", "#ced088",
               "#9cb78d", "#72975E", "#388697", "#304b65", "#271033", "#808080")
  purposePal <- c("Breeding in captivity or artificial propagation" = "#FDC056",
                  "Educational" = "#304b65",
                  "Botanic gardens" = "#fa8334",
                  "Hunting Trophies" = "#7e3d11",
                  "Law Enforcement / Judicial / Forensic use only" = "#9cb78d",
                  "Personal" = "#b45618",
                  "Scientific / Research" = "#388697",
                  "Commercial" = "#271033",
                  "Reintroduction/introduction into the wild" = "#ced088",
                  "Zoos / Circuses" = "#E6C86F",
                  "Other" = "#808080")

  palList <- list(palette,
                  groupPaletteDF,
                  paletteWildCap,
                  purposePalVector,
                  purposePal)
  names(palList) <- c("groupPalette",
                      "groupPaletteDF",
                      "paletteWildCap",
                      "purposePalVector",
                      "purposePal")

  return(palList)
}
