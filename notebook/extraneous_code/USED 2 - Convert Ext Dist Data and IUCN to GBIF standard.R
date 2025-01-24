library(here)
library(dplyr)
library(stringr)
library(taxize)
library(readr)

extDistFiles <- list.files(here("data", "speciesDistributions"), pattern = "distributionData.csv$",
                           full.names = TRUE)
groups_ <- gsub("_distributionData.csv", "", unlist(lapply(str_split(extDistFiles, "/"), function(x){tail(x, 1)})))
distributionDataList <- lapply(extDistFiles, function(x){
  return(read_csv(x, na = c("", "<NA>")))
})
names(distributionDataList) <- groups_

distributionDataList$birds <- distributionDataList$birds %>%
  mutate(inputName = Scientific)
distributionDataList$reptiles <- distributionDataList$reptiles %>%
  mutate(inputName = paste(genus, species))
distributionDataList$mammals <- distributionDataList$mammals %>%
  mutate(inputName = sciName)
distributionDataList$arachnids <- distributionDataList$arachnids %>%
  mutate(inputName = speciesName)
distributionDataList$amphibians <- distributionDataList$amphibians %>%
  mutate(inputName = species)

nameReviewList <- vector("list", length = length(groups_))
names(nameReviewList) <- groups_
for(grp in names(distributionDataList)){
  # grp <- names(distributionDataList)[1]
  # grp <- "arachnids"
  print(grp)

  if("temp_nameReviewList" %in% list.files(here("data"))){
    load(file = here("data", "temp_nameReviewList"))
    if(!is.null(nameReviewList[[grp]])){
      {next}
    }
  }

  currData <- distributionDataList[[grp]] %>%
    group_by(inputName) %>%
    slice_head(n = 1)

  seqSplit <- seq(101, nrow(currData), by = 100)
  seqSplit[length(seqSplit)] <- nrow(currData)
  gbifResList <- vector("list", length = length(seqSplit))
  for(i in seq_along(seqSplit)){
    if(i == 1){
      print(paste0(i, " - ", 1, ":", (seqSplit[i])))
      gbifResList[[i]] <- gna_verifier(currData$inputName[1:seqSplit[i]], data_sources = 11,
                                       fields = "all", with_context = TRUE, canonical = TRUE)
      {next}
    }
    print(paste0(i, " - ", (seqSplit[i-1]+1), ":", (seqSplit[i])))
    gbifResList[[i]] <-
      try(
        gna_verifier(currData$inputName[(seqSplit[i-1]+1):(seqSplit[i])], data_sources = 11,
                     fields = "all", with_context = TRUE, canonical = TRUE)
      )
  }
  gbifResFull <- do.call(plyr::rbind.fill,
                         gbifResList[!unlist(lapply(gbifResList, function(x){class(x)[1]})) == "try-error"])

  issueNames <- currData$inputName[which(!currData$inputName %in% gbifResFull$submittedName)]
  gbifResIssues_List <- vector("list", length = length(issueNames))
  j <- 0
  for(name in issueNames){
    j <- j + 1
    print(paste0(j, " - ", name))
    gbifResIssues_List[[j]] <-
      try(
        gna_verifier(name, data_sources = 11,
                     fields = "all", canonical = TRUE)
      )
  }
  gbifResFull_2 <- do.call(plyr::rbind.fill,
                           gbifResIssues_List[!unlist(lapply(gbifResIssues_List, function(x){class(x)[1]})) == "try-error"])

  issueNames2 <- currData$inputName[which(!currData$corrected %in% c(gbifResFull$submittedName,
                                                                     gbifResFull_2$submittedName))]

  groupList <- list(gbifResFull, gbifResFull_2, issueNames2)

  nameReviewList[[grp]] <- groupList

  save(nameReviewList, file = here("data", "temp_nameReviewList"))

}
nameReviewList

simplifiedList <- lapply(nameReviewList, function(x){
  plyr::rbind.fill(x[[1]], x[[2]]) %>%
    select(submittedName, matchedCanonicalSimple) %>%
    mutate(submittedName = gsub("_", " ", submittedName)) %>%
    rename("corrected" = matchedCanonicalSimple)
})

grpFiles <- list.files(here("data", "speciesRedlist"), pattern = ".zip")
nameIUCNList <- vector("list", length = length(grpFiles))
names(nameIUCNList) <- str_extract(grpFiles, ".+?(?=_)")
for(grpF in grpFiles){
  # grpF <- grpFiles[3]
  grp <- str_extract(grpF, ".+?(?=_)")
  # iucnAssessment <- read_csv(unzip(here("data", "speciesRedlist",
  #                                       "Birds_redlist_species_data_86a42404-3c66-4205-aece-74c49efd72ae.zip"), "assessments.csv"))
  print(grpF)

  if("temp_iucnNameList" %in% list.files(here("data"))){
    load(file = here("data", "temp_iucnNameList"))
    if(!is.null(nameIUCNList[[grp]])){
      {next}
    }
  }

  currData <- read_csv(unzip(here("data", "speciesRedlist",
                                  grpF), "assessments.csv"))

  # check names that are already converted in the dist data
  currData <- currData %>%
    left_join(simplifiedList[[sub("1|2", "", grp)]] %>%
                rename("scientificName" = submittedName), by = "scientificName")

  unknownNameConversions <- currData$scientificName[is.na(currData$corrected)]

  if(length(unknownNameConversions) > 100){

    seqSplit <- seq(101, length(unknownNameConversions), by = 100)
    seqSplit[length(seqSplit)] <- length(unknownNameConversions)
    gbifResList <- vector("list", length = length(seqSplit))
    for(i in seq_along(seqSplit)){
      if(i == 1){
        print(paste0(i, " - ", 1, ":", (seqSplit[i])))
        gbifResList[[i]] <- try(
          gna_verifier(unknownNameConversions[1:seqSplit[i]], data_sources = 11,
                       fields = "all", with_context = TRUE, canonical = TRUE)
        )
        {next}
      }
      print(paste0(i, " - ", (seqSplit[i-1]+1), ":", (seqSplit[i])))
      gbifResList[[i]] <-
        try(
          gna_verifier(unknownNameConversions[(seqSplit[i-1]+1):(seqSplit[i])], data_sources = 11,
                       fields = "all", with_context = TRUE, canonical = TRUE)
        )
    }
    gbifResFull <- do.call(plyr::rbind.fill,
                           gbifResList[!unlist(lapply(gbifResList, function(x){class(x)[1]})) == "try-error"])

    issueNames <- unknownNameConversions[which(!unknownNameConversions %in% gbifResFull$submittedName)]
  } else {
    issueNames <- unknownNameConversions
  }

  gbifResIssues_List <- vector("list", length = length(issueNames))
  j <- 0
  for(name in issueNames){
    j <- j + 1
    print(paste0(j, " - ", name))
    gbifResIssues_List[[j]] <-
      try(
        gna_verifier(name, data_sources = 11,
                     fields = "all", canonical = TRUE)
      )
  }
  gbifResFull_2 <- do.call(plyr::rbind.fill,
                           gbifResIssues_List[!unlist(lapply(gbifResIssues_List, function(x){class(x)[1]})) == "try-error"])

  issueNames2 <- currData$inputName[which(!currData$corrected %in% c(gbifResFull$submittedName,
                                                                     gbifResFull_2$submittedName))]

  groupList <- list(gbifResFull, gbifResFull_2, issueNames2)

  nameIUCNList[[grp]] <- groupList

  # these are just the extras that aren't already tackled by the dist data searches
  save(nameIUCNList, file = here("data", "temp_iucnNameList"))

}

simplifiedIUCNList <- lapply(nameIUCNList, function(x){
  plyr::rbind.fill(x[[1]], x[[2]]) %>%
    select(submittedName, matchedCanonicalSimple) %>%
    mutate(submittedName = gsub("_", " ", submittedName)) %>%
    rename("corrected" = matchedCanonicalSimple)
})

simplifiedIUCNList$birds <- rbind(simplifiedIUCNList$birds1, simplifiedIUCNList$birds2)
simplifiedIUCNList$birds1 <- NULL
simplifiedIUCNList$birds2 <- NULL

# combine both simple lists for name conversions --------------------------
allNamesList <- vector("list", length = length(groups_))
names(allNamesList) <- groups_
for(grp in names(simplifiedList)){

  allNamesList[[grp]] <- rbind(simplifiedList[[grp]], simplifiedIUCNList[[grp]])

}
save(allNamesList, file = here("data", "temp_allNamesList"))

# apply GBIF conversion to files ------------------------------------------

for(grp in names(distributionDataList)){
  # grp <- names(distributionDataList)[1]

  write.csv(distributionDataList[[grp]] %>%
              left_join(allNamesList[[grp]] %>%
                          rename("inputName" = submittedName), by = "inputName") %>%
              mutate(group_ = ifelse(grp == "mammals", "Terrestrial Mammals", str_to_sentence(grp))),
            file = here("data", "externalDistributionData", paste0(grp, "_distributionData_gbifNames.csv")),
            row.names = FALSE)

}

grpFiles <- list.files(here("data", "speciesRedlist"), pattern = ".zip")
# names(grpFiles)
for(grp in groups_){
  # grp <- groups_[3]
  print(grp)

  if(grp %in% "birds"){
    currData1 <- read_csv(unzip(here("data", "speciesRedlist",
                                     grpFiles[str_detect(grpFiles, grp)][1]), "assessments.csv"))
    currData2 <- read_csv(unzip(here("data", "speciesRedlist",
                                     grpFiles[str_detect(grpFiles, grp)][1]), "assessments.csv"))
    iDistData1 <- read_csv(unzip(here("data", "speciesRedlist",
                                      grpFiles[str_detect(grpFiles, grp)][1]), "countries.csv"),
                           na = "<NA>")
    iDistData2 <- read_csv(unzip(here("data", "speciesRedlist",
                                      grpFiles[str_detect(grpFiles, grp)][1]), "countries.csv"),
                           na = "<NA>")
    currData <- rbind(currData1, currData2)
    iDistData <- rbind(iDistData1, iDistData2)
  } else {
    currData <- read_csv(unzip(here("data", "speciesRedlist",
                                    grpFiles[str_detect(grpFiles, grp)]), "assessments.csv"))
    iDistData <- read_csv(unzip(here("data", "speciesRedlist",
                                      grpFiles[str_detect(grpFiles, grp)]), "countries.csv"),
                           na = "<NA>")
  }

  currData_withNames <- currData %>%
    left_join(allNamesList[[grp]] %>%
                rename("scientificName" = submittedName), by = "scientificName") %>%
    mutate(group_ = ifelse(grp == "mammals", "Terrestrial Mammals", str_to_sentence(grp)))

  iDistData_withNames <- iDistData %>%
    left_join(allNamesList[[grp]] %>%
                rename("scientificName" = submittedName), by = "scientificName") %>%
    mutate(group_ = ifelse(grp == "mammals", "Terrestrial Mammals", str_to_sentence(grp)))

  write.csv(currData_withNames, file = here("data", "iucnData", paste0(grp, "_assessments_gbifNames.csv")),
            row.names = FALSE)
  write.csv(iDistData_withNames, file = here("data", "iucnData", paste0(grp, "_countries_gbifNames.csv")),
            row.names = FALSE)

}

# Combine GBIF data into final source of distribution data ----------------

allGBIFList <- vector("list", length = length(groups_))
names(allGBIFList) <- groups_
for(grp in names(allGBIFList)){

  files <- list.files(here("data", "gbifDistData", grp), pattern = "_gbifCountries.csv$",
                      full.names = TRUE)

  readInList <- lapply(files, function(x){
    read_csv(x, na = c("", "<NA>"))
  })
  countryData <- do.call(rbind, readInList)

  countryData <- countryData %>%
    mutate(group_ = ifelse(grp == "mammals", "Terrestrial Mammals", str_to_sentence(grp))) %>%
    select(corrected, gbifDistIso2, group_)

  write.csv(countryData, file = here("data", "gbifDistData", paste0(grp, "_gbifCountries.csv")),
            row.names = FALSE)

}
