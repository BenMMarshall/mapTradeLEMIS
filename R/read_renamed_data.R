#' Read in renamed lemis data
#'
#' @name read_trade_data
#' @description abc.
#' @return The file as a dataframe, ready for downstream functions.
#'
#' @export
read_renamed_data <- function(extraManualDistributionDataList, ...){
  lemisDataCorrected <- read_csv(here("data", "figshareDownload", "lemisDataCorrected_2023-11-11.csv.gz"))

  # updating a few misnamed species
  for(grp in names(extraManualDistributionDataList)){
    # grp <- "reptiles"
    extraDistDF <- extraManualDistributionDataList[[grp]]
    for(corr in extraDistDF$corrected){
      # corr <- extraDistDF$corrected[1]
      if(length(extraDistDF[extraDistDF$corrected == corr,]$corrected) >0){
        lemisDataCorrected[lemisDataCorrected$corrected == corr,]$corrected <-
          ifelse(is.null(extraDistDF$correctedSpelling[extraDistDF$corrected == corr]),
                 extraDistDF$corrected[extraDistDF$corrected == corr],
                 extraDistDF$correctedSpelling[extraDistDF$corrected == corr])
      }
    }
  }

  return(lemisDataCorrected)
}

#' @export
read_filtered_data <- function(extraManualDistributionDataList, ...){
  lemisDataFiltered <- read_csv(here("data", "figshareDownload", "lemisDataOutliersRemoved_2024-01-09_mag2_tenK.csv.gz"))

  # updating a few misnamed species
  for(grp in names(extraManualDistributionDataList)){
    # grp <- "reptiles"
    print(grp)
    extraDistDF <- extraManualDistributionDataList[[grp]]
    for(corr in extraDistDF$corrected){
      print(corr)
      # corr <- extraDistDF$corrected[1]
      if(length(extraDistDF[extraDistDF$corrected == corr,]$corrected) >0){
        lemisDataFiltered[lemisDataFiltered$corrected == corr,]$corrected <-
          ifelse(is.null(extraDistDF$correctedSpelling[extraDistDF$corrected == corr]),
                 extraDistDF$corrected[extraDistDF$corrected == corr],
                 extraDistDF$correctedSpelling[extraDistDF$corrected == corr])
      }
    }
  }

  return(lemisDataFiltered)
}


