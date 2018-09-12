#Global Variations in OpenStreetMap
#Cory Williams 


#Greater Manchester Ward Analysis 

#Load relevant libraries 
library(caret)
library(pls)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(readxl)
library(corrplot)
library(Hmisc)
library(relaimpo)
library(readxl)

#Load .csv files created from PostgreSQL queries
GM_PCA <- read_excel("Case_study_common_data.xlsx", sheet = "GM_PCA")
GM_PCA <- GM_PCA[, -1]
gm_sum <- read_excel("Manchester_ward_data_summary.xlsx", sheet = "Manchester_ward_data_summary")
Manchester_ward_tags <- read_excel("casestudy_ward_tags.xlsx", sheet = "New_Greater_Manchester")

#Reorder dataframe
Manchester_ward_tags1 <- Manchester_ward_tags[, c(50, 1:49, 51:58)]
#Remove first column
Manchester_ward_tags1 <- Manchester_ward_tags1[,-1]


#Change inconsitent naming 
Manchester_ward_tags1[58, 2] <-  "Crompton Bolton"
Manchester_ward_tags1[59, 2] <-  "Crompton Oldham"
Manchester_ward_tags1[34, 2] <-  "Brooklands Manchester"
Manchester_ward_tags1[35, 2] <-  "Brooklands Trafford"
Manchester_ward_tags1[179, 2] <-  "St Mary's Bury"
Manchester_ward_tags1[180, 2] <-  "St Mary's Oldham"
Manchester_ward_tags1[181, 2] <-  "St Mary's Trafford"
Manchester_ward_tags1 <- Manchester_ward_tags1[, c(1:57, 1)]
Manchester_ward_tags <- Manchester_ward_tags1

#Change inconsitent naming 
GM_PCA[5, 2] <-  "Crompton Bolton"
GM_PCA[75, 2] <-  "Crompton Oldham"
GM_PCA[42, 2] <-  "Brooklands Manchester"
GM_PCA[174, 2] <-  "Brooklands Trafford"
GM_PCA[86, 2] <-  "St Mary's Oldham"
GM_PCA[185, 2] <-  "St Mary's Trafford"
GM_PCA[34, 2] <-  "St Mary's Bury"

#Subset data
m_tag_unique_users <- gm_sum[,1:8]

#Extract specific tags
#This is because the imported .csv file includes the wards in which each feature occured
#This extracts the two relevant columns (feature, wards they occurred in)
#This will allow them to be joined in a more appropriate manner later on 
m_tag_begin <- Manchester_ward_tags1[, 1]
m_tag_school <- Manchester_ward_tags1[, 2:3]
m_tag_college <- Manchester_ward_tags1[, 4:5]
m_tag_pub <- Manchester_ward_tags1[, 6:7]
m_tag_bar <- Manchester_ward_tags1[, 8:9]
m_tag_pharmacy <- Manchester_ward_tags1[, 10:11]
m_tag_hospital <- Manchester_ward_tags1[, 12:13]
m_tag_dentist <- Manchester_ward_tags1[, 14:15]
m_tag_clinic <- Manchester_ward_tags1[, 16:17]
m_tag_police <- Manchester_ward_tags1[, 18:19]
m_tag_bank <- Manchester_ward_tags1[, 20:21]
m_tag_atm <- Manchester_ward_tags1[, 22:23]
m_tag_restaurant <- Manchester_ward_tags1[, 24:25]
m_tag_fast_food <- Manchester_ward_tags1[, 26:27]
m_tag_toilets <- Manchester_ward_tags1[, 28:29]
m_tag_drinking_water <- Manchester_ward_tags1[, 30:31]
m_tag_place_of_worship <- Manchester_ward_tags1[, 32:33]
m_tag_bus_stop <- Manchester_ward_tags1[, 34:35]
m_tag_street_lamp <- Manchester_ward_tags1[, 36:37]
m_tag_hotel <- Manchester_ward_tags1[, 38:39]
m_tag_industrial <- Manchester_ward_tags1[, 40:41]
m_tag_apartment <- Manchester_ward_tags1[, 42:43]
m_tag_house <- Manchester_ward_tags1[, 44:45]
m_tag_church <- Manchester_ward_tags1[, 46:47]
m_tag_mosque <- Manchester_ward_tags1[, 48:49]
m_tag_drinking_water[is.na(m_tag_drinking_water)] <- 0
m_Tag_footway <- Manchester_ward_tags1[, 50:51]
m_Tag_primary <- Manchester_ward_tags1[, 52:53]
m_Tag_residential <- Manchester_ward_tags1[, 54:55]
m_Tag_unclassified <- Manchester_ward_tags1[, 56:57]


#Reassign the variable
m_tag1 <- m_tag_unique_users

#Alter the name of the first column
names(m_tag1)[1] <- "ward"

#join each tag
m_tag1 <- left_join(m_tag1, m_tag_school, by = c("ward" = "ward1"))
m_tag1 <- left_join(m_tag1, m_tag_college, by = c("ward" = "ward2"))
m_tag1 <- left_join(m_tag1, m_tag_pub, by = c("ward" = "ward3"))
m_tag1 <- left_join(m_tag1, m_tag_bar, by = c("ward" = "ward4"))
m_tag1 <- left_join(m_tag1, m_tag_pharmacy, by = c("ward" = "ward5"))
m_tag1 <- left_join(m_tag1, m_tag_hospital, by = c("ward" = "ward6"))
m_tag1 <- left_join(m_tag1, m_tag_dentist, by = c("ward" = "ward7"))
m_tag1 <- left_join(m_tag1, m_tag_clinic, by = c("ward" = "ward8"))
m_tag1 <- left_join(m_tag1, m_tag_police, by = c("ward" = "ward9"))
m_tag1 <- left_join(m_tag1, m_tag_bank, by = c("ward" = "ward10"))
m_tag1 <- left_join(m_tag1, m_tag_atm, by = c("ward" = "ward11"))
m_tag1 <-left_join(m_tag1, m_tag_restaurant, by = c("ward" = "ward12"))
m_tag1 <-left_join(m_tag1, m_tag_fast_food, by = c("ward" = "ward13"))
m_tag1 <- left_join(m_tag1, m_tag_toilets, by = c("ward" = "ward14"))
m_tag1 <-left_join(m_tag1, m_tag_place_of_worship, by = c("ward" = "ward16"))
m_tag1 <- left_join(m_tag1, m_tag_bus_stop, by = c("ward" = "ward17"))
m_tag1 <-left_join(m_tag1, m_tag_street_lamp, by = c("ward" = "ward18"))
m_tag1 <- left_join(m_tag1, m_tag_hotel, by = c("ward" = "ward19"))
m_tag1 <-left_join(m_tag1, m_tag_industrial, by = c("ward" = "ward20"))
m_tag1 <-left_join(m_tag1, m_tag_apartment, by = c("ward" = "ward21"))
m_tag1 <- left_join(m_tag1, m_tag_house, by = c("ward" = "ward22"))
m_tag1 <- left_join(m_tag1, m_tag_church, by = c("ward" = "ward23"))
m_tag1 <- left_join(m_tag1, m_tag_mosque, by = c("ward" = "ward24"))
m_tag1 <- left_join(m_tag1, m_Tag_footway, by = c("ward" = "ward25"))
m_tag1 <- left_join(m_tag1, m_Tag_primary, by = c("ward" = "ward26"))
m_tag1 <- left_join(m_tag1, m_Tag_residential, by = c("ward" = "ward27"))
m_tag1 <- left_join(m_tag1, m_Tag_unclassified, by = c("ward" = "ward28"))
m_tag2 <- m_tag1

#Reaplce all NA values with 0
m_tag2[is.na(m_tag2)] <- 0

#Export the result to a .csv file
#write.csv(m_tag2, file = "Manchester_wards_whatismapped.csv")

#Reasign to dependent and independent variables 
m_depdf <- m_tag2
m_indepdf <- GM_PCA

#Join dependent and independent variables together 
manchester_join <- left_join(m_indepdf, m_depdf, by = c("Ward name" = "ward"))

#Change column name
names(manchester_join)[5] <- "total_hh"


#Create the pp_hh variable
manchester_join$pp_hh <- as.numeric(manchester_join$population)/as.numeric(manchester_join$total_hh)

#Reassign 
manchester_join3 <- manchester_join

#Function to remove the effect of non-scaled variables (Willbrink, 2017)
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100

#Assign variables to assess correlation of independent and dependent variables
m_indepcor <- m_indepdf
m_depcor <- m_depdf

#Remove non-numeric columns
m_indepcor2 <- m_indepcor[-1:-2]

#Calculate correlation matrix
m_indepcor2 <- rcorr(as.matrix(m_indepcor2))

#Extract result
mancr <- m_indepcor2$r

#Remove variables considered highly dependent
m_indepcor3 <- mancr[c(-2,-3,-5,-5,-12,-13), c(-2,-3,-5,-5,-12,-13)]

#Reassign
manchester_join2 <- manchester_join3

#Extract the column names of the data frame into a list variable
manc_names <- colnames(manchester_join2)

#Change dataframe columns of type 'character' to type 'numeric'
manchester_join3 <- data.frame(sapply(manchester_join3, function(x) as.numeric(as.character(x))))

#Change the column names of the dataframe
colnames(manchester_join3) <- manc_names

#Insert the first two columns into the newer variable (ward, wardcode)
manchester_join3[,1:2] <- manchester_join2[,1:2]

#If applicable, run the following line to remove ".x" in column headers which sometimes occur
#names(manchester_join3) <- gsub(".x", "", names(join), fixed = TRUE)

#Change column names to avoid issues with muliple linear regression
names(manchester_join3)[23] <- "users"
names(manchester_join3)[29] <- "ft_density"


#MULTIPLE LINEAR REGRESSION
#RUN FOR ALL OSM FEATURES

lm_osmuid <- lm(
  manchester_join3$users ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_totaledits <- lm(
  manchester_join3$`total edits` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_point <- lm(
  manchester_join3$Point_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_line <- lm(
  manchester_join3$Line_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_polygon <- lm(
  manchester_join3$Polygon_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_ftdensity <- lm(
  manchester_join3$ft_density ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_school <- lm(
  manchester_join3$School ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_college <- lm(
  manchester_join3$College ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_pub <- lm(
  manchester_join3$Pub ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_bar <- lm(
  manchester_join3$Bar ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_pharmacy <- lm(
  manchester_join3$Pharmacy ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

lm_hospital <- lm(
  manchester_join3$Hospital ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

lm_dentist <- lm(
  manchester_join3$Dentist ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

lm_clinic <- lm(
  manchester_join3$Clinic ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_police <- lm(
  manchester_join3$Police ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_bank <- lm(
  manchester_join3$Bank ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

lm_atm <- lm(
  manchester_join3$ATM ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_restaurant <- lm(
  manchester_join3$Restaurant ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_fastfood <- lm(
  manchester_join3$`Fast Food` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_toilets <- lm(
  manchester_join3$Toilets ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_placeofworship <- lm(
  manchester_join3$`Place of Worship` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_busstop <- lm(
  manchester_join3$Bus_Stop ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_streetlamp <- lm(
  manchester_join3$`Street Lamp` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_hotel <- lm(
  manchester_join3$Hotel ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_industrial <- lm(
  manchester_join3$Industrial ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_apartments <- lm(
  manchester_join3$Apartments ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

lm_house <- lm(
  manchester_join3$House ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

lm_church <- lm(
  manchester_join3$Church ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_mosque <- lm(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_footway <- lm(
  manchester_join3$Footway ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_primary <- lm(
  manchester_join3$Primary ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_residential <- lm(
  manchester_join3$Residential ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)


lm_unclassified <- lm(
  manchester_join3$Unclassified ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent
)

#Output of the MLR models - Adjusted R2 and P Value
summary(lm_osmuid)
summary(lm_totaledits)
summary(lm_point)
summary(lm_line)
summary(lm_polygon)
summary(lm_ftdensity)
summary(lm_school)
summary(lm_college)
summary(lm_pub)
summary(lm_bar)
summary(lm_pharmacy)
summary(lm_hospital)
summary(lm_dentist)
summary(lm_clinic)
summary(lm_police)
summary(lm_bank)
summary(lm_atm)
summary(lm_restaurant)
summary(lm_fastfood)
summary(lm_toilets)
summary(lm_placeofworship)
summary(lm_busstop)
summary(lm_streetlamp)
summary(lm_hotel)
summary(lm_industrial)
summary(lm_apartments)
summary(lm_house)
summary(lm_church)
summary(lm_mosque)
summary(lm_footway)
summary(lm_primary)
summary(lm_residential)
summary(lm_unclassified)

#Calculate the AIC value for the regresssion
#This allows for comparison with GWR later 
#But ONLY for the exact same dataset
AIC(lm_osmuid)
AIC(lm_totaledits)
AIC(lm_point)
AIC(lm_line)
AIC(lm_polygon)
AIC(lm_ftdensity)
AIC(lm_school)
AIC(lm_college)
AIC(lm_pub)
AIC(lm_bar)
AIC(lm_pharmacy)
AIC(lm_hospital)
AIC(lm_dentist)
AIC(lm_clinic)
AIC(lm_police)
AIC(lm_bank)
AIC(lm_atm)
AIC(lm_restaurant)
AIC(lm_fastfood)
AIC(lm_toilets)
AIC(lm_placeofworship)
AIC(lm_busstop)
AIC(lm_streetlamp)
AIC(lm_hotel)
AIC(lm_industrial)
AIC(lm_apartments)
AIC(lm_house)
AIC(lm_church)
AIC(lm_mosque)
AIC(lm_footway)
AIC(lm_primary)
AIC(lm_residential)
AIC(lm_unclassified)



#Calculate the importance of residuals
#Reassinging the variable each time to avoid clogging the environment with data and having to remove it later
v.test <- calc.relimp(lm_osmuid, type = c("lmg"), rela = TRUE) 
v.test <- calc.relimp(lm_totaledits, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_line, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_ftdensity, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_pub, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_bar, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_bank, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_atm, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_restaurant, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_fastfood, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_busstop, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_hotel, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_unclassified, type = c("lmg"), rela = TRUE)

#Print the output as a percentage
(v.test@lmg)*100

#Plot the Q-Q plot output of each regression plot
plot(lm_osmuid, which=2)
plot(lm_totaledits,which=2)
plot(lm_point,which=2)
plot(lm_line,which=2)
plot(lm_polygon,which=2)
plot(lm_ftdensity,which=2)
plot(lm_school,which=2)
plot(lm_college,which=2)
plot(lm_pub,which=2)
plot(lm_bar,which=2)
plot(lm_pharmacy,which=2)
plot(lm_hospital,which=2)
plot(lm_dentist,which=2)
plot(lm_clinic,which=2)
plot(lm_police,which=2)
plot(lm_bank,which=2)
plot(lm_atm,which=2)
plot(lm_restaurant,which=2)
plot(lm_fastfood,which=2)
plot(lm_toilets,which=2)
#plot(lm_drinkingwater,which=2)
plot(lm_placeofworship,which=2)
plot(lm_busstop,which=2)
plot(lm_streetlamp,which=2)
plot(lm_hotel,which=2)
plot(lm_industrial,which=2)
plot(lm_apartments,which=2)
plot(lm_house,which=2)
plot(lm_church,which=2)
plot(lm_mosque,which=2)
plot(lm_footway,which=2)
plot(lm_primary,which=2)
plot(lm_residential,which=2)
plot(lm_unclassified,which=2)


#SPATIAL AUTOCORRELATION

#Read in the shapefile
GM_wardshp1 <- readOGR(dsn = ".", layer = "gm_wardsshp1")

#Plot the shapefile
plot(GM_wardshp1)

#Remove uneeded data
GM_wardshp1 <- GM_wardshp1[-3]

#Change the column name of the dataframe to match the shapefile
#This allows the two to be merged
colnames(manchester_join3)[2] <- "name"

#Merge the dataframe and shapefile
m_jointrial <- merge(GM_wardshp1, manchester_join3)

#Test plotting a specific variable
#Results in a chloroplet style map
#plot(m_jointrial$`total edits`)

#Calculate the nearest neighbour
#Adapted from Nick Bearman's spatial analysis practical

#calculate a neighbours list for each ward polygon
neighbours <- poly2nb(m_jointrial)

#plot Greater Manchester, adding the neighbours list on top
plot(m_jointrial, col = "grey")
plot(neighbours, coordinates(m_jointrial),add = TRUE, col = 'red')

#Create a listw and perform moran test
listw <- nb2listw(neighbours)
listw

#Moran I - Independent variables
#Results are printed to the console
moran.test(manchester_join3$population, listw)
moran.test(manchester_join3$`general sex ratio (females to males)`, listw) 
moran.test(manchester_join3$`% pop 18-64`, listw) 
moran.test(manchester_join3$`% households with 1-3 people`, listw) 
moran.test(manchester_join3$`% Employment Rate`, listw)
moran.test(manchester_join3$`% of households owning house they live in`, listw) 
moran.test(manchester_join3$car_or_van, listw) 
moran.test(manchester_join3$highest_qual_level4_plus, listw) 
moran.test(manchester_join3$christian_count, listw) 
moran.test(manchester_join3$muslim_count, listw)
moran.test(manchester_join3$muslim_count, listw) 
moran.test(manchester_join3$female_lone_parent, listw)

#Moran I - Dependent variables
#Results are printed to the console
moran.test(manchester_join3$users, listw)
moran.test(manchester_join3$`total edits`, listw)
moran.test(manchester_join3$Point_Count, listw)
moran.test(manchester_join3$Line_Count, listw)
moran.test(manchester_join3$Polygon_Count, listw)
moran.test(manchester_join3$ft_density, listw)
moran.test(manchester_join3$School, listw)
moran.test(manchester_join2$College, listw)
moran.test(manchester_join3$Pub, listw)
moran.test(manchester_join3$Bar, listw)
moran.test(manchester_join3$Pharmacy, listw)
moran.test(manchester_join3$Hospital, listw)
moran.test(manchester_join3$Dentist, listw)
moran.test(manchester_join3$Clinic, listw)
moran.test(manchester_join3$Police, listw)
moran.test(manchester_join3$Bank, listw)
moran.test(manchester_join3$ATM, listw)
moran.test(manchester_join3$Restaurant, listw)
moran.test(manchester_join3$`Fast Food`, listw)
moran.test(manchester_join3$Toilets, listw)
moran.test(manchester_join3$`Place of Worship`, listw)
moran.test(manchester_join3$Bus_Stop, listw)
moran.test(manchester_join3$`Street Lamp`, listw)
moran.test(manchester_join3$Hotel, listw)
moran.test(manchester_join3$Industrial, listw)
moran.test(manchester_join2$Apartments, listw) 
moran.test(manchester_join3$House, listw)
moran.test(manchester_join3$Church, listw)
moran.test(manchester_join3$Mosque, listw)
moran.test(manchester_join3$Footway, listw)
moran.test(manchester_join3$Primary, listw)
moran.test(manchester_join3$Residential, listw)
moran.test(manchester_join3$Unclassified, listw)


#Add in grouped variables 
write.csv(manchester_join3, file = "manc_grouping.csv")



#Geographically Weighted Analysis 
#Adapted from Nick Bearman GWR practical (Spatial Analaysis - Term 1 module)

#BANDWIDTH SELECTION
gwrbandwidth_lmosmuid <- gwr.sel(
  manchester_join3$users ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmtotaledits <- gwr.sel(
  manchester_join3$`total edits` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmpoint <- gwr.sel(
  manchester_join3$Point_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmline <- gwr.sel(
  manchester_join3$Line_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmpolygon <- gwr.sel(
  manchester_join3$Polygon_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmftdensity <- gwr.sel(
  manchester_join3$ft_density ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmschool <- gwr.sel(
  manchester_join3$School ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmcollege <- gwr.sel(
  manchester_join3$~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent,
  data = m_jointrial,
  adapt = T
)

gwrbandwidth_lmpub <- gwr.sel(
  manchester_join3$Pub ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmbar <- gwr.sel(
  manchester_join3$Bar ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmpharmacy <- gwr.sel(
  manchester_join3$Pharmacy ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmhospital <- gwr.sel(
  manchester_join3$Hospital ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmdentist <- gwr.sel(
  manchester_join3$Dentist ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmclinic <- gwr.sel(
  manchester_join3$Clinic ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmpolice <- gwr.sel(
  manchester_join3$Police ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmbank <- gwr.sel(
  manchester_join3$Bank ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmatm <- gwr.sel(
  manchester_join3$ATM ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmrestaurant <- gwr.sel(
  manchester_join3$Restaurant ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmfastfood <- gwr.sel(
  manchester_join3$`Fast Food` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmtoilets <- gwr.sel(
  manchester_join3$Toilets ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)


gwrbandwidth_lmplaceofworship <- gwr.sel(
  manchester_join3$`Place of Worship` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmbusstop <- gwr.sel(
  manchester_join3$Bus_Stop ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmstreetlamp <- gwr.sel(
  manchester_join3$`Street Lamp` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmhotel <- gwr.sel(
  manchester_join3$Hotel ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmindustrial <- gwr.sel(
  manchester_join3$Industrial ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)


gwrbandwidth_lmapartments <- gwr.sel(
  manchester_join3$Apartments ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmhouse <- gwr.sel(
  manchester_join3$House ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T)

gwrbandwidth_lmchurch <- gwr.sel(
  manchester_join3$Church ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmmosque <- gwr.sel(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmfootway <- gwr.sel(
  manchester_join3$Footway ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmprimary<- gwr.sel(
  manchester_join3$Primary ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmresidential <- gwr.sel(
  manchester_join3$Residential ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)

gwrbandwidth_lmunclassified <- gwr.sel(
  manchester_join3$Unclassified ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = T
)


#Create GWR models for each OSM feature
gwrmodel.lmosmuid <- gwr(
  manchester_join3$users ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmosmuid, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmtotaledits <- gwr(
  manchester_join3$`total edits` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmtotaledits, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmpoint <- gwr(
  manchester_join3$Point_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmpoint, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmline <- gwr(
  manchester_join3$Line_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmline, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmpolygon <- gwr(
  manchester_join3$Polygon_Count ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmpolygon, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmftdensity <- gwr(
  manchester_join3$ft_density ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmftdensity, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmschool <- gwr(
  manchester_join3$School ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmschool, hatmatrix=TRUE, se.fit=TRUE
)


gwrmodel.lmcollege <- gwr(
   manchester_join3$College ~ manchester_join3$population +
     manchester_join3$`general sex ratio (females to males)` +
     manchester_join3$`% pop 18-64` +
     manchester_join3$`% households with 1-3 people` +
     manchester_join3$`% Employment Rate` +
     manchester_join3$`% of households owning house they live in` +
     manchester_join3$car_or_van +
     manchester_join3$highest_qual_level4_plus +
     manchester_join3$christian_count +
     manchester_join3$muslim_count +
     manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmcollege, hatmatrix=TRUE, se.fit=TRUE
 )

gwrmodel.lmpub <- gwr(
  manchester_join3$Pub ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmpub, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmbar <- gwr(
  manchester_join3$Bar ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmbar, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmpharmacy <- gwr(
  manchester_join3$Pharmacy ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmpharmacy, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmhospital <- gwr(
  manchester_join3$Hospital ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmhospital, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmdentist <- gwr(
  manchester_join3$Dentist ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmdentist, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmclinic <- gwr(
  manchester_join3$Clinic ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmclinic, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmpolice <- gwr(
  manchester_join3$Police ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmpolice, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmbank <- gwr(
  manchester_join3$Bank ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmbank, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmatm <- gwr(
  manchester_join3$ATM ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmatm, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmrestaurant <- gwr(
  manchester_join3$Restaurant ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmrestaurant, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmfastfood <- gwr(
  manchester_join3$`Fast Food` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmfastfood, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmtoilets <- gwr(
  manchester_join3$Toilets ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmtoilets, hatmatrix=TRUE, se.fit=TRUE
)


gwrmodel.lmplaceofworship <- gwr(
  manchester_join3$`Place of Worship` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmplaceofworship, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmbusstop <- gwr(
  manchester_join3$Bus_Stop ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmbusstop, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmstreetlamp <- gwr(
  manchester_join3$`Street Lamp` ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmstreetlamp, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmhotel <- gwr(
  manchester_join3$Hotel ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmhotel, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmindustrial <- gwr(
  manchester_join3$Industrial ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmindustrial, hatmatrix=TRUE, se.fit=TRUE
)


gwrmodel.lmapartments <- gwr(
  manchester_join3$Apartments ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmapartments, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmhouse <- gwr(
  manchester_join3$House ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmhouse, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmchurch <- gwr(
  manchester_join3$Church ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmchurch, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmmosque <- gwr(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmmosque, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmfootway <- gwr(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmfootway, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmprimary <- gwr(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmprimary, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmresidential <- gwr(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmresidential, hatmatrix=TRUE, se.fit=TRUE
)

gwrmodel.lmunclassified <- gwr(
  manchester_join3$Mosque ~ manchester_join3$population +
    manchester_join3$`general sex ratio (females to males)` +
    manchester_join3$`% pop 18-64` +
    manchester_join3$`% households with 1-3 people` +
    manchester_join3$`% Employment Rate` +
    manchester_join3$`% of households owning house they live in` +
    manchester_join3$car_or_van +
    manchester_join3$highest_qual_level4_plus +
    manchester_join3$christian_count +
    manchester_join3$muslim_count +
    manchester_join3$female_lone_parent, data =m_jointrial, adapt = gwrbandwidth_lmunclassified, hatmatrix=TRUE, se.fit=TRUE
)



#ASSIGN RESULTS

results.gwrmodel.lmosmuid <- as.data.frame(gwrmodel.lmosmuid$SDF)

results.gwrmodel.lmtotaledits <- as.data.frame(gwrmodel.lmtotaledits$SDF)

results.gwrmodel.lmpoint <-as.data.frame(gwrmodel.lmpoint$SDF)

results.gwrmodel.lmline <- as.data.frame(gwrmodel.lmline$SDF)

results.gwrmodel.lmpolygon <- as.data.frame(gwrmodel.lmpolygon$SDF)

results.gwrmodel.lmftdensity <- as.data.frame(gwrmodel.lmftdensity$SDF)

results.gwrmodel.lmschool <- as.data.frame(gwrmodel.lmschool$SDF)

results.gwrmodel.lmcollege <- as.data.frame(gwrmodel.lmcollege$SDF)

results.gwrmodel.lmpub <- as.data.frame(gwrmodel.lmpub$SDF)

results.gwrmodel.lmbar <- as.data.frame(gwrmodel.lmbar$SDF)

results.gwrmodel.lmpharmacy <- as.data.frame(gwrmodel.lmpharmacy$SDF)

results.gwrmodel.lmhospital <- as.data.frame(gwrmodel.lmhospital$SDF)

results.gwrmodel.lmdentist <- as.data.frame(gwrmodel.lmdentist$SDF)

results.gwrmodel.lmclinic <- as.data.frame(gwrmodel.lmclinic$SDF)

results.gwrmodel.lmpolice <- as.data.frame(gwrmodel.lmpolice$SDF)

results.gwrmodel.lmbank <- as.data.frame(gwrmodel.lmbank$SDF)

results.gwrmodel.lmatm <- as.data.frame(gwrmodel.lmatm$SDF)

results.gwrmodel.lmrestaurant <- as.data.frame(gwrmodel.lmrestaurant$SDF)

results.gwrmodel.lmfastfood <- as.data.frame(gwrmodel.lmfastfood$SDF)

results.gwrmodel.lmplaceofworship <- as.data.frame(gwrmodel.lmplaceofworship$SDF)

results.gwrmodel.lmbusstop <- as.data.frame(gwrmodel.lmbusstop$SDF)

results.gwrmodel.lmstreetlamp <- as.data.frame(gwrmodel.lmstreetlamp$SDF)

results.gwrmodel.lmhotel <- as.data.frame(gwrmodel.lmhotel$SDF)

results.gwrmodel.lmindustrial <- as.data.frame(gwrmodel.lmindustrial$SDF)

results.gwrmodel.lmapartments <- as.data.frame(gwrmodel.lmapartments$SDF)

results.gwrmodel.lmhouse <- as.data.frame(gwrmodel.lmhouse$SDF)

results.gwrmodel.lmchurch <- as.data.frame(gwrmodel.lmchurch$SDF)

results.gwrmodel.lmmosque <- as.data.frame(gwrmodel.lmmosque$SDF)

results.gwrmodel.lmfootway <- as.data.frame(gwrmodel.lmfootway$SDF)

results.gwrmodel.lmprimary <- as.data.frame(gwrmodel.lmprimary$SDF)

results.gwrmodel.lmresidential <- as.data.frame(gwrmodel.lmresidential$SDF)

results.gwrmodel.lmunclassified <- as.data.frame(gwrmodel.lmunclassified$SDF)


#Bind the GWR result to the SPDF
gwr.map.lmosmuid <- 
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmosmuid))


gwr.map.lmtotaledits <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmtotaledits))

gwr.map.lmpoint <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmpoint))

gwr.map.lmline <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmline))

gwr.map.lmpolygon <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmpolygon))

gwr.map.lmftdensity <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmftdensity))

gwr.map.lmschool <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmschool))

 gwr.map.lmcollege <-
   cbind(m_jointrial,
         as.matrix(results.gwrmodel.lmcollege))

gwr.map.lmpub <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmpub))

gwr.map.lmpub <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmbar))

gwr.map.lmpharmacy <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmpharmacy))

gwr.map.lmhospital <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmhospital))

gwr.map.lmdentist <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmdentist))

gwr.map.lmclinic <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmclinic))

gwr.map.lmpolice <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmpolice))

gwr.map.lmbank <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmbank))

gwr.map.lmatm <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmatm))

gwr.map.lmrestaurant <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmrestaurant))

gwr.map.lmfastfood <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmfastfood))

gwr.map.lmplaceofworship <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmplaceofworship))

gwr.map.lmbusstop <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmbusstop))

gwr.map.lmstreetlamp <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmstreetlamp))

gwr.map.lmhotel <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmhotel))

gwr.map.lmindustrial <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmindustrial))

gwr.map.lmapartments <-
  cbind(m_jointrial,
        as.matrix(results.gwrmodel.lmapartments))

gwr.map.lmhouse <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmhouse))

gwr.map.lmchurch <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmchurch))

gwr.map.lmmosque <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmmosque))

gwr.map.lmfootway <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmfootway))

gwr.map.lmprimary <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmprimary))

gwr.map.lmresidential <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmresidential))

gwr.map.lmunclassified <-
  cbind(m_jointrial, as.matrix(results.gwrmodel.lmunclassified))


#Map the local R2 results for visual investigation
#Only global R2 will be used in the report 
qtm(gwr.map.lmosmuid, fill = "localR2")
qtm(gwr.map.lmtotaledits, fill = "localR2")
qtm(gwr.map.lmpoint, fill = "localR2") 
qtm(gwr.map.lmline, fill = "localR2")
qtm(gwr.map.lmpolygon, fill = "localR2")
qtm(gwr.map.lmftdensity, fill = "localR2") 
qtm(gwr.map.lmschool, fill = "localR2")
qtm(gwr.map.lmcollege, fill = "localR2")
qtm(gwr.map.lmpub, fill = "localR2")
qtm(gwr.map.lmbar, fill = "localR2") 
qtm(gwr.map.lmpharmacy, fill = "localR2")
qtm(gwr.map.lmhospital, fill = "localR2")
qtm(gwr.map.lmdentist, fill = "localR2")
qtm(gwr.map.lmclinic, fill = "localR2")
qtm(gwr.map.lmpolice, fill = "localR2")
qtm(gwr.map.lmbank, fill = "localR2")
qtm(gwr.map.lmatm, fill = "localR2")
qtm(gwr.map.lmrestaurant, fill = "localR2") 
qtm(gwr.map.lmfastfood, fill = "localR2")
qtm(gwr.map.lmplaceofworship, fill = "localR2")
qtm(gwr.map.lmbusstop, fill = "localR2")
qtm(gwr.map.lmstreetlamp, fill = "localR2")
qtm(gwr.map.lmhotel, fill = "localR2")
qtm(gwr.map.lmapartments, fill = "localR2") 
qtm(gwr.map.lmhouse, fill = "localR2") 
qtm(gwr.map.lmchurch, fill = "localR2") 
qtm(gwr.map.lmmosque, fill = "localR2")  
qtm(gwr.map.lmfootway, fill = "localR2") 
qtm(gwr.map.lmprimary, fill = "localR2")
qtm(gwr.map.lmresidential, fill = "localR2")
qtm(gwr.map.lmunclassified, fill = "localR2")

#Write shapefile to be imported into ArcGIS
writeOGR(m_jointrial, ".", "m_jointrial3", driver="ESRI Shapefile")


#print the r2 GWR results for the report
#will be compared to standard OLS regression 
#To determine if using spatial regression explains more variance within the dataset
gwrmodel.lmosmuid 
gwrmodel.lmtotaledits 
gwrmodel.lmpoint 
gwrmodel.lmline 
gwrmodel.lmpolygon 
gwrmodel.lmftdensity
gwrmodel.lmschool 
gwrmodel.lmcollege 
gwrmodel.lmpub 
gwrmodel.lmbar
gwrmodel.lmpharmacy
gwrmodel.lmhospital 
gwrmodel.lmdentist 
gwrmodel.lmclinic 
gwrmodel.lmpolice 
gwrmodel.lmbank 
gwrmodel.lmatm 
gwrmodel.lmrestaurant 
gwrmodel.lmfastfood 
gwrmodel.lmtoilets 
gwrmodel.lmdrinkingwater
gwrmodel.lmplaceofworship 
gwrmodel.lmbusstop 
gwrmodel.lmstreetlamp 
gwrmodel.lmhotel 
gwrmodel.lmindustrial 
gwrmodel.lmapartments 
gwrmodel.lmhouse 
gwrmodel.lmchurch 
gwrmodel.lmmosque 
gwrmodel.lmfootway 
gwrmodel.lmprimary 
gwrmodel.lmresidential
gwrmodel.lmunclassified 





#Grouped Variables

#Add in additional grouped variables and repeat the above method

#Group
gm.group <- read_excel("manc_grouping.xlsx", sheet = "ward_group")

#If needed, re-load data
GM_PCA <- read_excel("Case_study_common_data.xlsx", sheet = "GM_PCA")
GM_PCA <- GM_PCA[, -1]

gm.join <- left_join(m_indepdf, gm.group, by = c("Ward name" = "name"))

gm.name <- colnames(gm.join)
gm.join2 <- data.frame(sapply(gm.join, function(x) as.numeric(as.character(x))))
colnames(gm.join2) <- gm.name
gm.join2[,1:2] <- gm.join[,1:2]


#linear regression
lm_group_rec <- lm(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$general.sex.ratio..females.to.males. +
    gm.join2$X..pop.18.64 +
    gm.join2$X..households.with.1.3.people +
    gm.join2$X..Employment.Rate +
    gm.join2$X..of.households.owning.house.they.live.in +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent
)

lm_group_edu <- lm(
  gm.join2$edu ~ gm.join2$population +
    gm.join2$general.sex.ratio..females.to.males. +
    gm.join2$X..pop.18.64 +
    gm.join2$X..households.with.1.3.people +
    gm.join2$X..Employment.Rate +
    gm.join2$X..of.households.owning.house.they.live.in +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent
)

lm_group_rel <- lm(
  gm.join2$rel ~ gm.join2$population +
    gm.join2$general.sex.ratio..females.to.males. +
    gm.join2$X..pop.18.64 +
    gm.join2$X..households.with.1.3.people +
    gm.join2$X..Employment.Rate +
    gm.join2$X..of.households.owning.house.they.live.in +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent
)

lm_group_prop <- lm(
  gm.join2$prop ~ gm.join2$population +
    gm.join2$general.sex.ratio..females.to.males. +
    gm.join2$X..pop.18.64 +
    gm.join2$X..households.with.1.3.people +
    gm.join2$X..Employment.Rate +
    gm.join2$X..of.households.owning.house.they.live.in +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent
)

lm_group_infra <- lm(
  gm.join2$infra ~ gm.join2$population +
    gm.join2$general.sex.ratio..females.to.males. +
    gm.join2$X..pop.18.64 +
    gm.join2$X..households.with.1.3.people +
    gm.join2$X..Employment.Rate +
    gm.join2$X..of.households.owning.house.they.live.in +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent
)

lm_group_ps <- lm(
  gm.join2$ps ~ gm.join2$population +
    gm.join2$general.sex.ratio..females.to.males. +
    gm.join2$X..pop.18.64 +
    gm.join2$X..households.with.1.3.people +
    gm.join2$X..Employment.Rate +
    gm.join2$X..of.households.owning.house.they.live.in +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent
)


#Results of linear regression
summary(lm_group_rec)
summary(lm_group_edu)
summary(lm_group_rel)
summary(lm_group_prop)
summary(lm_group_infra)
summary(lm_group_ps)


#global moran test for grouped variables
#Uses the listw calculated previously
moran.test(gm.join2$rec, listw)
moran.test(gm.join2$edu, listw)
moran.test(gm.join2$rel, listw)
moran.test(gm.join2$prop, listw)
moran.test(gm.join2$infra, listw)
moran.test(gm.join2$ps, listw)

#Local moran calculated in ArcGIS

#GWR
GM_wardshp1$name
#Change column name
colnames(gm.join2)[2] <- "name"
#Merge dataframe and shapefile
gm.group.join <- merge(GM_wardshp1, gm.join2)

#Change column name due to issue with spaces
colnames(gm.join2)[11] <- "18_64"

#GWR
gwr.sel_group_rec <- gwr.sel(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt= T)

gwr.sel_group_edu <- gwr.sel(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt= T)


gwr.sel_group_rel <- gwr.sel(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt= T)


gwr.sel_group_prop <- gwr.sel(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt= T)


gwr.sel_group_infra <- gwr.sel(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt= T)


gwr.sel_group_ps <- gwr.sel(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt= T)


#GWR Model
gwr.rec <- gwr(
  gm.join2$rec ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join,  adapt = gwr.sel_group_rec, hatmatrix=TRUE, se.fit=TRUE)

gwr.edu <- gwr(
  gm.join2$edu ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join,  adapt = gwr.sel_group_edu, hatmatrix=TRUE, se.fit=TRUE)


gwr.rel <- gwr(
  gm.join2$rel ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join,  adapt = gwr.sel_group_rel, hatmatrix=TRUE, se.fit=TRUE)

gwr.prop <- gwr(
  gm.join2$prop ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt =gwr.sel_group_prop, hatmatrix=TRUE, se.fit=TRUE)

gwr.infra <- gwr(
  gm.join2$infra ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, data =gm.group.join, adapt = gwr.sel_group_infra, hatmatrix=TRUE, se.fit=TRUE)

gwr.ps <- gwr(
  gm.join2$ps ~ gm.join2$population +
    gm.join2$`general sex ratio (females to males)` +
    gm.join2$`18_64` +
    gm.join2$`% households with 1-3 people` +
    gm.join2$`% Employment Rate` +
    gm.join2$`% of households owning house they live in` +
    gm.join2$car_or_van +
    gm.join2$highest_qual_level4_plus +
    gm.join2$christian_count +
    gm.join2$muslim_count +
    gm.join2$female_lone_parent, 
  data =gm.group.join, adapt =gwr.sel_group_ps, hatmatrix=TRUE, se.fit=TRUE)

gwr.ps
gwr.edu
gwr.rec
gwr.rel
gwr.prop
gwr.infra

#Linear regression results
summary(lm_group_rec)
summary(lm_group_edu) 
summary(lm_group_rel)
summary(lm_group_prop)
summary(lm_group_infra)
summary(lm_group_ps)

#AIC
AIC(lm_group_ps)
AIC(lm_group_edu) 
AIC(lm_group_rec)
AIC(lm_group_rel)
AIC(lm_group_prop)
AIC(lm_group_infra)

#Importance of each variable in the MLR
v.test <- calc.relimp(lm_group_ps, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_group_rec, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_group_infra, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_group_edu, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(lm_group_prop, type = c("lmg"), rela = TRUE)

#Print the output as a percentage
(v.test@lmg)*100


