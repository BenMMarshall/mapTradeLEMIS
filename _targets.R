# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble",
               "readr",
               "here",
               "here",
               "dplyr",
               "tidyr",
               "countrycode",
               "stringr",
               "ggplot2",
               "ggtext",
               "glue",
               "ggmap",
               "ggrepel",
               "colormod",
               "patchwork",
               "rsvg",
               "ggimage",
               "scatterpie",
               "colorspace"
  ), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# Replace the target list below with your own:
list(
  tar_target(
    lemisCodes,
    read_lemis_key()
  ),
  tar_target(
    portLocations,
    read_port_key()
  ),
  tar_target(
    lemisISOconversion,
    read_iso_key()
  ),
  tar_target(
    worldDataList,
    read_world_data()
  ),
  tar_target(
    iucnDistributionList,
    iucn_distribution_data()
  ),
  tar_target(
    extraManualDistributionDataList,
    read_extraManualDistribution_data()
  ),
  tar_target(
    allDistributionDataList,
    combine_distribution_data(iucnDistributionList,
                              gbifDistributionList)
  ),
  tar_target(
    lemisDataRenamed,
    read_renamed_data(extraManualDistributionDataList)
  ),
  tar_target(
    paletteList,
    generate_palette()
  ),
  tar_target(
    lemisDataFiltered,
    read_filtered_data(extraManualDistributionDataList)
  ),
  tar_target(
    iucnSummary,
    read_rename_redlist(lemisDataRenamed)
  ),
  tar_target(
    iucnSummaryPlot,
    plot_iucn_summary(iucnSummary, paletteList)
  ),
  # tar_target(
  #   tableSpeciesOrigin,
  #   table_species_origin(lemisDataCorrected = lemisDataRenamed,
  #                        portLocations = portLocations,
  #                        lemisISOconversion = lemisISOconversion,
  #                        worldDataList = worldDataList)
  # ),
  # tar_target(
  #   tableQuantityOrigin,
  #   table_quantity_origin(lemisDataFiltered = lemisDataFiltered,
  #                         portLocations = portLocations,
  #                         lemisISOconversion = lemisISOconversion,
  #                         worldDataList = worldDataList)
  # ),
  tar_target(
    plotSpeciesOrigin,
    plot_species_origin(lemisDataCorrected = lemisDataRenamed,
                        portLocations = portLocations,
                        paletteList = paletteList,
                        lemisISOconversion = lemisISOconversion,
                        worldDataList = worldDataList)
  ),
  tar_target(
    tableSpeciesOrigin,
    table_species_origin(lemisDataCorrected = lemisDataRenamed,
                         portLocations = portLocations,
                         lemisISOconversion = lemisISOconversion,
                         worldDataList = worldDataList)
  ),
  tar_target(
    plotQuantityOrigin,
    plot_quantity_origin(lemisDataFiltered = lemisDataFiltered,
                         portLocations = portLocations,
                         paletteList = paletteList,
                         lemisISOconversion = lemisISOconversion,
                         worldDataList = worldDataList)
  ),
  tar_target(
    tableQuantityOrigin,
    table_quantity_origin(lemisDataFiltered = lemisDataFiltered,
                          portLocations = portLocations,
                          lemisISOconversion = lemisISOconversion,
                          worldDataList = worldDataList)
  ),
  tar_target(
    plotLiveOrigin,
    plot_live_origin(lemisDataFiltered = lemisDataFiltered,
                     portLocations = portLocations,
                     paletteList = paletteList,
                     lemisISOconversion = lemisISOconversion,
                     worldDataList = worldDataList)
  ),
  tar_target(
    plotSpeciesResidence,
    plot_species_residence(lemisDataRenamed = lemisDataRenamed,
                           lemisISOconversion = lemisISOconversion,
                           worldDataList = worldDataList,
                           portLocations = portLocations,
                           paletteList = paletteList,
                           allDistributionDataList = allDistributionDataList
    )
  ),
  tar_target(
    tableQuantityResidence,
    table_quantity_residence(lemisDataFiltered = lemisDataFiltered,
                             lemisISOconversion = lemisISOconversion,
                             worldDataList = worldDataList,
                             portLocations = portLocations,
                             allDistributionDataList = allDistributionDataList
    )
  ),
  tar_target(
    tableSupps,
    table_supp_clean(
    )
  ),
  tar_target(
    plotSpeciesMismatch,
    plot_species_mismatch(paletteList = paletteList,
                          tableSupps
    )
  ),
  tar_target(
    plotWildCompare,
    plot_wild_compare(paletteList = paletteList,
                      tableSupps
    )
  ),
  tar_target(
    outputResults,
    render_rmd(fileIN = here::here("notebook",
                                   "mapLEMISResults.Rmd"),
               fileOUT = here::here("notebook",
                                    "mapLEMISResults.html"),
               tableSupps, plotSpeciesResidence),
    priority = 0
  )
)
