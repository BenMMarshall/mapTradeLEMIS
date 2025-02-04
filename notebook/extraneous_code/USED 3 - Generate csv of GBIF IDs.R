library(here)
library(dplyr)
library(readr)
library(rgbif)

groups_ <- c("amphibians", "arachnids", "birds", "mammals", "reptiles")
allGBIFList <- vector("list", length = length(groups_))
names(allGBIFList) <- groups_
for(grp in names(allGBIFList)){
  # grp <- groups_[1]
  files <- list.files(here("data", "gbifDistData", grp), pattern = "_gbifClean.csv$",
                      full.names = TRUE)

  readInList <- lapply(files, function(x){
    read_csv(x, na = c("", "<NA>"))
  })
  countryData <- do.call(rbind, readInList[unlist(lapply(readInList, function(x){
    any(str_detect(names(x), "gbifID"))
  }))])

  countryData <- countryData %>%
    mutate(group_ = ifelse(grp == "mammals", "Terrestrial Mammals", str_to_sentence(grp))) %>%
    select(species, gbifID, group_)

  write.csv(countryData, file = here("data", "gbifDistData", paste0(grp, "_gbifIDs.csv")),
            row.names = FALSE)

}


