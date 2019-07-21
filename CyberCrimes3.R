crimes_2018_19 <- read.csv("crimes_2018-19.csv",stringsAsFactors = FALSE)


crimes_2018_19$PrimaryType <- as.factor(crimes_2018_19$PrimaryType) 
crimes_2018_19$Description <- as.factor(crimes_2018_19$Description) 
crimes_2018_19$LocationDescription <- as.factor(crimes_2018_19$LocationDescription) 
crimes_2018_19$IUCR <- as.factor(crimes_2018_19$IUCR) 

crimes_2018_19$Arrest[which(crimes_2018_19$Arrest == "True")] <- 1
crimes_2018_19$Arrest[which(crimes_2018_19$Arrest == "False")] <- 0

crimes_2018_19$Domestic[which(crimes_2018_19$Domestic == "True")] <- 1
crimes_2018_19$Domestic[which(crimes_2018_19$Domestic == "False")] <- 0

head(crimes_2018_19)


#install.packages('ggplot2')
library(ggplot2)
primary_type <- ggplot(crimes_2018_19, aes(PrimaryType))
primary_type + geom_histogram(stat = "count") + coord_flip()

ggplot(crimes_2018_19, aes(Ward)) +
  geom_density()

#install.packages('plotrix')
library(plotrix)
arrests <- table(crimes_2018_19$Arrest)
lbls <- paste(names(arrests), "\n", arrests, sep="")
pie(arrests, labels = lbls, 
      main="Arrests results (1 = True, 0 = False) from Crimes commited ")


#levels(crimes_12_to_17$IUCR) #353 Levels
top10_iucr <- tail(names(sort(table(crimes_2018_19$IUCR))), 10)
iucr_raw <- table(crimes_2018_19$IUCR)
barplot(iucr_raw[order(iucr_raw, decreasing = TRUE)], xlim = c(0,11))


#levels(crimes_12_to_17$IUCR) #353 Levels
top10_iucr <- tail(names(sort(table(crimes_2018_19$IUCR))), 10)
iucr_raw <- table(crimes_2018_19$IUCR)
barplot(iucr_raw[order(iucr_raw, decreasing = TRUE)], xlim = c(0,11))

#levels(crimes_12_to_17$Description) #340 Levels
top10_description <- tail(names(sort(table(crimes_2018_19$Description))), 10)
head(top10_description)

#levels(crimes_12_to_17$Location.Description) #141 Levels
top10_location_description <- tail(names(sort(table(crimes_2018_19$LocationDescription))), 10)
head(top10_location_description)

crimes_2018_19$Beat <- as.factor(crimes_2018_19$Beat) #Put this at the beggining of the report
#levels(crimes_12_to_17$Beat) #289 Levels
top10_beat <- tail(names(sort(table(crimes_2018_19$Beat))), 10)
beat_raw <- table(crimes_2018_19$Beat)
barplot(beat_raw[order(beat_raw, decreasing = TRUE)], xlim = c(0,11))

crimes_2018_19$District <- as.factor(crimes_2018_19$District) #Put this at the beggining of the report
#levels(crimes_12_to_17$District) #23 Levels
top10_district <- tail(names(sort(table(crimes_2018_19$District))), 10)
district_raw <- table(crimes_2018_19$District)
barplot(district_raw[order(district_raw, decreasing = TRUE)], xlim = c(0,11))


crimes_2018_19$Ward <- as.factor(crimes_2018_19$Ward) #Put this at the beggining of the report
#levels(crimes_12_to_17$Ward) #45 Levels
top10_ward <- tail(names(sort(table(crimes_2018_19$Ward))), 10)
ward_raw <- table(crimes_2018_19$Ward)
barplot(ward_raw[order(ward_raw, decreasing = TRUE)], xlim = c(0,11))


crimes_2018_19$Community.Area <- as.factor(crimes_2018_19$CommunityArea) #Put this at the beggining of the report
#levels(crimes_12_to_17$Community.Area) #67 Levels
top10_community_area <- tail(names(sort(table(crimes_2018_19$CommunityArea))), 10)
community_area_raw <- table(crimes_2018_19$CommunityArea)
barplot(community_area_raw[order(community_area_raw, decreasing = TRUE)], xlim = c(0,11))


crimes_2018_19$FBICode <- as.factor(crimes_2018_19$FBICode) #Put this at the beggining of the report
#levels(crimes_12_to_17$FBI.Code) #19 Levels
top10_fbi_code <- tail(names(sort(table(crimes_2018_19$FBICode))), 10)
fbi_raw <- table(crimes_2018_19$FBICode)
barplot(fbi_raw[order(fbi_raw, decreasing = TRUE)], xlim = c(0,11))

