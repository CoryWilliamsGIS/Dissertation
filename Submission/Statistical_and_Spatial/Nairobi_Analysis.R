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
library(sp)
library(rgeos)
library(tmap)
library(spatstat)
library(maptools)
library(classInt)
library(dplyr)
library(spdep)

#NAIROBI

#Load data
Nairobi <-
  read_excel("Case_study_common_data.xlsx", sheet = "Nairobi")
n_sum <-
  read_excel("nairobi_ward_data_summary.xlsx", sheet = "nairobi_ward_data_summary")




#Distinct users per ward
#Read in excel file which you have exported each in
n_uid <-
  read_excel("nairobi_ward_unique_uid.xlsx", sheet = "Sheet5")


#subset
Nairobi <-
  Nairobi[, c(-1,-3,-4,-7,-12:-15, -26:-28, -30, -32, -36, -39)]

#Rename
names(n_sum)[8] <- "ft_density_km2"
ft_density <- n_sum$ft_density_km2
n_sum$population_density <- n_sum$population / n_sum$`Area (km2)`
pop_density <- n_sum$population_density



#Dependent variables
#Load data exported manually colated from PostgreSQL
Nairobi_ward_tags <-
  read_excel("casestudy_ward_tags.xlsx", sheet = "Nairobi")

#Extract specific tags
#This is because the imported .csv file includes the wards in which each feature occured
#This extracts the two relevant columns (feature, wards they occurred in)
#This will allow them to be joined in a more appropriate manner later on
n_tag_begin <- Nairobi_ward_tags[, 1]
n_tag_school <- Nairobi_ward_tags[, 2:3]
n_tag_college <- Nairobi_ward_tags[, 4:5]
n_tag_pub <- Nairobi_ward_tags[, 6:7]
n_tag_bar <- Nairobi_ward_tags[, 8:9]
n_tag_pharmacy <- Nairobi_ward_tags[, 10:11]
n_tag_hospital <- Nairobi_ward_tags[, 12:13]
n_tag_dentist <- Nairobi_ward_tags[, 14:15]
n_tag_clinic <- Nairobi_ward_tags[, 16:17]
n_tag_police <- Nairobi_ward_tags[, 18:19]
n_tag_bank <- Nairobi_ward_tags[, 20:21]
n_tag_atm <- Nairobi_ward_tags[, 22:23]
n_tag_restaurant <- Nairobi_ward_tags[, 24:25]
n_tag_fast_food <- Nairobi_ward_tags[, 26:27]
n_tag_toilets <- Nairobi_ward_tags[, 28:29]
n_tag_drinking_water <- Nairobi_ward_tags[, 30:31]
n_tag_place_of_worship <- Nairobi_ward_tags[, 32:33]
n_tag_bus_stop <- Nairobi_ward_tags[, 34:35]
n_tag_street_lamp <- Nairobi_ward_tags[, 36:37]
n_tag_hotel <- Nairobi_ward_tags[, 38:39]
n_tag_industrial <- Nairobi_ward_tags[, 40:41]
n_tag_apartment <- Nairobi_ward_tags[, 42:43]
n_tag_house <- Nairobi_ward_tags[, 44:45]
n_tag_church <- Nairobi_ward_tags[, 46:47]
n_tag_mosque <- Nairobi_ward_tags[, 48:49]
n_tag_footway <- Nairobi_ward_tags[, 50:51]
n_tag_primary <- Nairobi_ward_tags[, 52:53]
n_tag_residential <- Nairobi_ward_tags[, 54:55]
n_tag_unclassified <- Nairobi_ward_tags[, 56:57]
n_tag_unique_users <- n_sum[, 1:6]



#join back together,
n_tag1 <- n_tag_begin
n_tag1 <-
  left_join(n_tag1, n_tag_unique_users, by = c("ward" = "WARD"))
n_tag1 <- left_join(n_tag1, n_tag_school, by = c("ward" = "ward1"))
n_tag1 <- left_join(n_tag1, n_tag_college, by = c("ward" = "ward2"))
n_tag1 <- left_join(n_tag1, n_tag_pub, by = c("ward" = "ward3"))
n_tag1 <- left_join(n_tag1, n_tag_bar, by = c("ward" = "ward4"))
n_tag1 <-
  left_join(n_tag1, n_tag_pharmacy, by = c("ward" = "ward5"))
n_tag1 <-
  left_join(n_tag1, n_tag_hospital, by = c("ward" = "ward6"))
n_tag1 <- left_join(n_tag1, n_tag_dentist, by = c("ward" = "ward7"))
n_tag1 <- left_join(n_tag1, n_tag_clinic, by = c("ward" = "ward8"))
n_tag1 <- left_join(n_tag1, n_tag_police, by = c("ward" = "ward9"))
n_tag1 <- left_join(n_tag1, n_tag_bank, by = c("ward" = "ward10"))
n_tag1 <- left_join(n_tag1, n_tag_atm, by = c("ward" = "ward11"))
n_tag1 <-
  left_join(n_tag1, n_tag_restaurant, by = c("ward" = "ward12"))
n_tag1 <-
  left_join(n_tag1, n_tag_fast_food, by = c("ward" = "ward13"))
n_tag1 <-
  left_join(n_tag1, n_tag_toilets, by = c("ward" = "ward14"))
n_tag1 <-
  left_join(n_tag1, n_tag_drinking_water, by = c("ward" = "ward15"))
n_tag1 <-
  left_join(n_tag1, n_tag_place_of_worship, by = c("ward" = "ward16"))
n_tag1 <-
  left_join(n_tag1, n_tag_bus_stop, by = c("ward" = "ward17"))
n_tag1 <-
  left_join(n_tag1, n_tag_street_lamp, by = c("ward" = "ward18"))
n_tag1 <- left_join(n_tag1, n_tag_hotel, by = c("ward" = "ward19"))
n_tag1 <-
  left_join(n_tag1, n_tag_industrial, by = c("ward" = "ward20"))
n_tag1 <-
  left_join(n_tag1, n_tag_apartment, by = c("ward" = "ward21"))
n_tag1 <- left_join(n_tag1, n_tag_house, by = c("ward" = "ward22"))
n_tag1 <- left_join(n_tag1, n_tag_church, by = c("ward" = "ward23"))
n_tag1 <- left_join(n_tag1, n_tag_mosque, by = c("ward" = "ward24"))
n_tag1 <-
  left_join(n_tag1, n_tag_footway, by = c("ward" = "ward25"))
n_tag1 <-
  left_join(n_tag1, n_tag_primary, by = c("ward" = "ward26"))
n_tag1 <-
  left_join(n_tag1, n_tag_residential, by = c("ward" = "ward27"))
n_tag1 <-
  left_join(n_tag1, n_tag_unclassified, by = c("ward" = "ward28"))

#Backup the original
n_tag2 <- n_tag1

#change na to 0
n_tag2[is.na(n_tag2)] <- 0

#Write dataframe to .csv the first time you do it
#write.csv(n_tag2, file = "Nairobi_wards_whatismapped.csv")

#Create variables for dependent and independent variables
n_depdf <- n_tag2
n_indepdf <- Nairobi

#Join together
nairobi_join <-
  left_join(n_indepdf, n_depdf, by = c("name" = "ward"))

#Create the pp_hh variable
nairobi_join$pp_hh <-
  as.numeric(nairobi_join$`total population`) / as.numeric(nairobi_join$`total households`)

#Reasign
nairobi_join3 <- nairobi_join

#Function to remove the effect of non-scaled variables (Willbrink, 2017)
rescale <- function(x)
  (x - min(x)) / (max(x) - min(x)) * 100

#Rescale
rs_total_pop <-
  rescale(as.numeric(nairobi_join3$`total population`))
nairobi_join3$`total population` <- rs_total_pop
nairobi_join3$ft_density <- ft_density
nairobi_join3$pop_density <- pop_density

#Change name to avoid issue when perforing regression
names(nairobi_join3)[20] <- "education level index"

#Backup
nairobi_bkup <- nairobi_join3

#Assign column names to a different variable
nairobi_names <- colnames(nairobi_join3)
nairobi_join3 <-
  data.frame(sapply(nairobi_join3, function(x)
    as.numeric(as.character(x))))
colnames(nairobi_join3) <- nairobi_names
nairobi_join3$name <- nairobi_bkup$name


#Perform linear regression for each OSM variable
n_lm_osmuid <-
  lm(
    nairobi_join3$`Distinct osm_users` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_totaledits <-
  lm(
    nairobi_join3$`total edits` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_point <-
  lm(
    nairobi_join3$Point_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_line <-
  lm(
    nairobi_join3$Line_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_polygon <-
  lm(
    nairobi_join3$Polygon_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_ftdensity <-
  lm(
    nairobi_join3$ft_density ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_school <-
  lm(
    nairobi_join3$School ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_college <-
  lm(
    nairobi_join3$College ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_pub <-
  lm(
    nairobi_join3$Pub ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_bar <-
  lm(
    nairobi_join3$Bar ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_pharmacy <-
  lm(
    nairobi_join3$Pharmacy ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_hospital <-
  lm(
    nairobi_join3$Hospital ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_dentist <-
  lm(
    nairobi_join3$Dentist ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_clinic <-
  lm(
    nairobi_join3$Clinic ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_police <-
  lm(
    nairobi_join3$Police ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_bank <-
  lm(
    nairobi_join3$Bank ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_atm <- lm(
  nairobi_join3$ATM ~ nairobi_join3$`total population` +
    nairobi_join3$`general sex ratio (females to males)` +
    nairobi_join3$`% of primary school attendance (6-13)` +
    nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
    nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
    nairobi_join3$`% households with 1-3 people` +
    nairobi_join3$`% of female headed households` +
    nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
    nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
    nairobi_join3$pop_density
)

n_lm_restaurant <-
  lm(
    nairobi_join3$Restaurant ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_fastfood <-
  lm(
    nairobi_join3$`Fast Food` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_toilets <-
  lm(
    nairobi_join3$Toilets ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_drinkingwater <-
  lm(
    nairobi_join3$`Drinking Water` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_placeofworship <-
  lm(
    nairobi_join3$`Place of Worship` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_busstop <-
  lm(
    nairobi_join3$Bus_Stop ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_streetlamp <-
  lm(
    nairobi_join3$`Street Lamp` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_hotel <-
  lm(
    nairobi_join3$Hotel ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_industrial <-
  lm(
    nairobi_join3$Industrial ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_apartments <-
  lm(
    nairobi_join3$Apartments ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_house <-
  lm(
    nairobi_join3$House ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_church <-
  lm(
    nairobi_join3$Church ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_mosque <-
  lm(
    nairobi_join3$Mosque ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` +
      nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` +
      nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` +
      nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_footway <-
  lm(
    nairobi_join3$Footway ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` +
      nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` +
      nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` +
      nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_primary <-
  lm(
    nairobi_join3$Primary ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` +
      nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` +
      nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` +
      nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_residential <-
  lm(
    nairobi_join3$Residential ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` +
      nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` +
      nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` +
      nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )

n_lm_unclassified <-
  lm(
    nairobi_join3$Unclassified ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` +
      nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` +
      nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` +
      nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density
  )


#Output of the MLR models - Adjusted R2 and P Value
summary(n_lm_osmuid)
summary(n_lm_totaledits)
summary(n_lm_point)
summary(n_lm_line)
summary(n_lm_polygon)
summary(n_lm_ftdensity)
summary(n_lm_school)
summary(n_lm_college)
summary(n_lm_pub)
summary(n_lm_bar)
summary(n_lm_pharmacy)
summary(n_lm_hospital)
summary(n_lm_dentist)
summary(n_lm_clinic)
summary(n_lm_police)
summary(n_lm_bank)
summary(n_lm_atm)
summary(n_lm_restaurant)
summary(n_lm_fastfood)
summary(n_lm_toilets)
summary(n_lm_drinkingwater)
summary(n_lm_placeofworship)
summary(n_lm_busstop)
summary(n_lm_streetlamp)
summary(n_lm_hotel)
summary(n_lm_industrial)
summary(n_lm_apartments)
summary(n_lm_house)
summary(n_lm_church)
summary(n_lm_mosque)
summary(n_lm_footway)
summary(n_lm_primary)
summary(n_lm_residential)
summary(n_lm_unclassified)

#Calculate the AIC value for the regresssion
#This allows for comparison with GWR later
#But ONLY for the exact same datasetAIC(n_lm_ftdensity)
AIC(n_lm_streetlamp)
AIC(n_lm_footway)
AIC(n_lm_primary)
AIC(n_lm_residential)
AIC(n_lm_unclassified)


#Calculate the importance of residuals
#Reassinging the variable each time to avoid clogging the environment with data and having to remove it laterv.test <- calc.relimp(n_lm_osmuid, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_point, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_line, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_ftdensity, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_college, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_bank, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_restaurant, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_fastfood, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_busstop, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_hotel, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n_lm_footway, type = c("lmg"), rela = TRUE)
v.test <-
  calc.relimp(n_lm_residential, type = c("lmg"), rela = TRUE)
v.test <- calc.relimp(n.lm.group.rec, type = c("lmg"), rela = TRUE)
v.test <-
  calc.relimp(n.lm.group.infra, type = c("lmg"), rela = TRUE)

#Print the output as a percentage
(v.test@lmg) * 100

#Clean up
rm(
  n_lm_atm,
  n_lm_apartments,
  n_lm_bank,
  n_lm_bar,
  n_lm_busstop,
  n_lm_church,
  n_lm_clinic,
  n_lm_college,
  n_lm_dentist,
  n_lm_drinkingwater,
  n_lm_fastfood,
  n_lm_ftdensity,
  n_lm_hospital,
  n_lm_hotel,
  n_lm_house,
  n_lm_industrial,
  n_lm_line,
  n_lm_mosque,
  n_lm_osmuid,
  n_lm_pharmacy,
  n_lm_placeofworship,
  n_lm_point,
  n_lm_police,
  n_lm_polygon,
  n_lm_pub,
  n_lm_restaurant,
  n_lm_school,
  n_lm_streetlamp,
  n_lm_toilets,
  n_lm_totaledits,
  n_lm_footway,
  n_lm_primary,
  n_lm_residential,
  n_lm_unclassified
)

rm(
  n_tag_apartment,
  n_tag_atm,
  n_tag_bank,
  n_tag_bar,
  n_tag_begin,
  n_tag_bus_stop,
  n_tag_church,
  n_tag_clinic,
  n_tag_college,
  n_tag_dentist,
  n_tag_drinking_water,
  n_tag_fast_food,
  n_tag_hospital,
  n_tag_hotel,
  n_tag_house,
  n_tag_industrial,
  n_tag_mosque,
  n_tag_pharmacy,
  n_tag_place_of_worship,
  n_tag_police,
  n_tag_pub,
  n_tag_restaurant,
  n_tag_school,
  n_tag_street_lamp,
  n_tag_toilets,
  n_tag_unique_users,
  n_tag_footway,
  n_tag_primary,
  n_tag_residential,
  n_tag_unclassified
)



#spatial autocorrelation morans I

#Load Nairobi shapefile
nairobi_wardshp1 <- readOGR(dsn = ".", layer = "nairobi_wardsshp1")
#plot shapefile
plot(nairobi_wardshp1)


#List the column names which are going to be removed from the Nairobi Shapefile
drops <-
  c(
    "OBJECTID_2",
    "OBJECTID_1",
    "OBJECTID",
    "OBJECTID_3",
    "CONSTITUEN",
    "COUNTY_COD",
    "Shape_Leng",
    "COUNTY_NAME",
    "Shape_Le_1",
    "Shape_Are",
    "Shape_Len",
    "Shape_Le_2",
    "Shape_Area"
  )

#Remove specific columns
nairobi_wardshp2 <-
  nairobi_wardshp1[,!(names(nairobi_wardshp1) %in% drops)]

#Change column name to allow for a sucessful merge
colnames(nairobi_join3)[1] <- "NAME"
#Merhe with shapefile
n_jointrial <- merge(nairobi_wardshp2, nairobi_join3)


#Calculate the nearest neighbour
#Adapted from Nick Bearman's spatial analysis practical
n_neighbours <- poly2nb(n_jointrial)


#Create listw and perform moran test
n_listw <- nb2listw(n_neighbours)


#Global Moran I - Indepdendent variables
moran.test(n_jointrial$`total population`, n_listw)
moran.test(n_jointrial$`general sex ratio (females to males)` , n_listw)
moran.test(n_jointrial$`% of primary school attendance (6-13)` , n_listw)
moran.test(n_jointrial$`Secondary School Attendance of 14- to 17-Year-Olds`,n_listw)
moran.test(n_jointrial$`education level index`, n_listw)
moran.test(n_jointrial$`% households owning own livestock` , n_listw)
moran.test(n_jointrial$`% pop 18-64` , n_listw)
moran.test(n_jointrial$`% households with 1-3 people` , n_listw)
moran.test(n_jointrial$`% of female headed households` , n_listw)
moran.test(n_jointrial$`% of households owning house they live in` , n_listw)
moran.test(n_jointrial$`% Employment Rate`, n_listw)
moran.test(n_jointrial$`% access to safe water source` , n_listw)
moran.test(n_jointrial$`% access to improved sanitation` , n_listw)
moran.test(n_jointrial$pop_density , n_listw)


#Global Moran I - Dependent variabls
moran.test(n_jointrial$`Distinct osm_users`, n_listw)
moran.test(n_jointrial$`total edits`, n_listw)
moran.test(n_jointrial$Point_Count, n_listw)
moran.test(n_jointrial$Line_Count, n_listw)
moran.test(n_jointrial$Polygon_Count, n_listw)
moran.test(n_jointrial$ft_density, n_listw)
moran.test(n_jointrial$School, n_listw)
moran.test(n_jointrial$College, n_listw)
moran.test(n_jointrial$Pub, n_listw)
moran.test(n_jointrial$Bar, n_listw)
moran.test(n_jointrial$Pharmacy, n_listw)
moran.test(n_jointrial$Hospital, n_listw)
moran.test(n_jointrial$Dentist, n_listw)
moran.test(n_jointrial$Clinic, n_listw)
moran.test(n_jointrial$Police, n_listw)
moran.test(n_jointrial$Bank, n_listw)
moran.test(n_jointrial$ATM, n_listw)
moran.test(n_jointrial$Restaurant, n_listw)
moran.test(n_jointrial$`Fast Food`, n_listw)
moran.test(n_jointrial$Toilets, n_listw)
moran.test(n_jointrial$`Drinking Water`, n_listw)
moran.test(n_jointrial$`Place of Worship`, n_listw)
moran.test(n_jointrial$Bus_Stop, n_listw)
moran.test(n_jointrial$`Street Lamp`, n_listw)
moran.test(n_jointrial$Hotel, n_listw)
moran.test(n_jointrial$Industrial, n_listw)
moran.test(n_jointrial$Apartments, n_listw)
moran.test(n_jointrial$House, n_listw)
moran.test(n_jointrial$Church, n_listw)
moran.test(n_jointrial$Mosque, n_listw)
moran.test(n_jointrial$Footway, n_listw)
moran.test(n_jointrial$Primary, n_listw)
moran.test(n_jointrial$Residential, n_listw)
moran.test(n_jointrial$Unclassified, n_listw)


#Geographically Weighted Regression (GWR)
#Calculate bandwidth
gwrbandwidth.n_lm_osmuid <-
  gwr.sel(
    nairobi_join3$`Distinct osm_users` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_totaledits <-
  gwr.sel(
    nairobi_join3$`total edits` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_point <-
  gwr.sel(
    nairobi_join3$Point_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_line <-
  gwr.sel(
    nairobi_join3$Line_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_polygon <-
  gwr.sel(
    nairobi_join3$Polygon_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_ftdensity <-
  gwr.sel(
    nairobi_join3$ft_density ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_school <-
  gwr.sel(
    nairobi_join3$School ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_college <-
  gwr.sel(
    nairobi_join3$College ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_pub <-
  gwr.sel(
    nairobi_join3$Pub ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_bar <-
  gwr.sel(
    nairobi_join3$Bar ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_pharmacy <-
  gwr.sel(
    nairobi_join3$Pharmacy ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_hospital <-
  gwr.sel(
    nairobi_join3$Hospital ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_dentist <-
  gwr.sel(
    nairobi_join3$Dentist ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_clinic <-
  gwr.sel(
    nairobi_join3$Clinic ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_police <-
  gwr.sel(
    nairobi_join3$Police ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` +
      nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_bank <-
  gwr.sel(
    nairobi_join3$Bank ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` +
      nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_atm <-
  gwr.sel(
    nairobi_join3$ATM ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_restaurant <-
  gwr.sel(
    nairobi_join3$Restaurant ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_fastfood <-
  gwr.sel(
    nairobi_join3$`Fast Food` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_toilets <-
  gwr.sel(
    nairobi_join3$Toilets ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_drinkingwater <-
  gwr.sel(
    nairobi_join3$`Drinking Water` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_placeofworship <-
  gwr.sel(
    nairobi_join3$`Place of Worship` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_busstop <-
  gwr.sel(
    nairobi_join3$Bus_Stop ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_streetlamp <-
  gwr.sel(
    nairobi_join3$`Street Lamp` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_hotel <-
  gwr.sel(
    nairobi_join3$Hotel ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_industrial <-
  gwr.sel(
    nairobi_join3$Industrial ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_apartments <-
  gwr.sel(
    nairobi_join3$Apartments ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_house <-
  gwr.sel(
    nairobi_join3$House ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_church <-
  gwr.sel(
    nairobi_join3$Church ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_mosque <-
  gwr.sel(
    nairobi_join3$Mosque ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_footway <-
  gwr.sel(
    nairobi_join3$Footway ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_primary <-
  gwr.sel(
    nairobi_join3$Primary ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )

gwrbandwidth.n_lm_residential <-
  gwr.sel(
    nairobi_join3$Residential ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )



gwrbandwidth.n_lm_unclassified <-
  gwr.sel(
    nairobi_join3$Unclassified ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = T
  )



#GWR Models
gwrmodel.n_lm_osmuid <-
  gwr(
    nairobi_join3$`Distinct osm_users` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_osmuid,
    hatmatrix = TRUE,
    se.fit = TRUE
  )


gwrmodel.n_lm_totaledits <-
  gwr(
    nairobi_join3$`total edits` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_totaledits,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_point <-
  gwr(
    nairobi_join3$Point_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_point,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_line <-
  gwr(
    nairobi_join3$Line_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_line,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_polygon <-
  gwr(
    nairobi_join3$Polygon_Count ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_polygon,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_ftdensity <-
  gwr(
    nairobi_join3$ft_density ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_ftdensity,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_school <-
  gwr(
    nairobi_join3$School ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_school,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_college <-
  gwr(
    nairobi_join3$College ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_college,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_pub <-
  gwr(
    nairobi_join3$Pub ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_pub,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_bar <-
  gwr(
    nairobi_join3$Bar ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_bar,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_pharmacy <-
  gwr(
    nairobi_join3$Pharmacy ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_pharmacy,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_hospital <-
  gwr(
    nairobi_join3$Hospital ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_hospital,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_dentist <-
  gwr(
    nairobi_join3$Dentist ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_dentist,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_clinic <-
  gwr(
    nairobi_join3$Clinic ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_clinic,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_police <-
  gwr(
    nairobi_join3$Police ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_police,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_bank <-
  gwr(
    nairobi_join3$Bank ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_bank,
    hatmatrix = TRUE,
    se.fit = TRUE
  )


gwrmodel.n_lm_atm <-
  gwr(
    nairobi_join3$ATM ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_atm,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_restaurant <-
  gwr(
    nairobi_join3$Restaurant ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_restaurant,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_fastfood <-
  gwr(
    nairobi_join3$`Fast Food` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_fastfood,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_toilets <-
  gwr(
    nairobi_join3$Toilets ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_toilets,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_drinkingwater <-
  gwr(
    nairobi_join3$`Drinking Water` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_drinkingwater,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_placeofworship <-
  gwr(
    nairobi_join3$`Place of Worship` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_placeofworship,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_busstop <-
  gwr(
    nairobi_join3$Bus_Stop ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_busstop,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_streetlamp <-
  gwr(
    nairobi_join3$`Street Lamp` ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_streetlamp,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_hotel <-
  gwr(
    nairobi_join3$Hotel ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_hotel,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_industrial <-
  gwr(
    nairobi_join3$Industrial ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_industrial,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_apartments <-
  gwr(
    nairobi_join3$Apartments ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_apartments,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_house <-
  gwr(
    nairobi_join3$House ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_house,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_church <-
  gwr(
    nairobi_join3$Church ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_church,
    hatmatrix = TRUE,
    se.fit = TRUE
  )


gwrmodel.n_lm_mosque <-
  gwr(
    nairobi_join3$Mosque ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_mosque,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_footway <-
  gwr(
    nairobi_join3$Footway ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_footway,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_primary <-
  gwr(
    nairobi_join3$Primary ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_primary,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_residential <-
  gwr(
    nairobi_join3$Residential ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_residential,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

gwrmodel.n_lm_unclassified <-
  gwr(
    nairobi_join3$Unclassified ~ nairobi_join3$`total population` +
      nairobi_join3$`general sex ratio (females to males)` +
      nairobi_join3$`% of primary school attendance (6-13)` +
      nairobi_join3$`Secondary School Attendance of 14- to 17-Year-Olds` + nairobi_join3$`education level index` +
      nairobi_join3$`% households owning own livestock` + nairobi_join3$`% pop 18-64` +
      nairobi_join3$`% households with 1-3 people` +
      nairobi_join3$`% of female headed households` +
      nairobi_join3$`% of households owning house they live in` + nairobi_join3$`% Employment Rate` +
      nairobi_join3$`% access to safe water source` + nairobi_join3$`% access to improved sanitation` +
      nairobi_join3$pop_density,
    data = n_jointrial,
    adapt = gwrbandwidth.n_lm_unclassified,
    hatmatrix = TRUE,
    se.fit = TRUE
  )



#Assign the results of the models
results.gwrmodel.n_lm_osmuid <-
  as.data.frame(gwrmodel.n_lm_osmuid$SDF)

results.gwrmodel.n_lm_totaledits <-
  as.data.frame(gwrmodel.n_lm_totaledits$SDF)

results.gwrmodel.n_lm_point <- as.data.frame(gwrmodel.n_lm_point$SDF)

results.gwrmodel.n_lm_line <- as.data.frame(gwrmodel.n_lm_line$SDF)

results.gwrmodel.n_lm_polygon <-
  as.data.frame(gwrmodel.n_lm_polygon$SDF)

results.gwrmodel.n_lm_ftdensity <-
  as.data.frame(gwrmodel.n_lm_ftdensity$SDF)

results.gwrmodel.n_lm_school <-
  as.data.frame(gwrmodel.n_lm_school$SDF)

results.gwrmodel.n_lm_college <-
  as.data.frame(gwrmodel.n_lm_college$SDF)

results.gwrmodel.n_lm_pub <- as.data.frame(gwrmodel.n_lm_pub$SDF)

results.gwrmodel.n_lm_bar <- as.data.frame(gwrmodel.n_lm_bar$SDF)

results.gwrmodel.n_lm_pharmacy <-
  as.data.frame(gwrmodel.n_lm_pharmacy$SDF)

results.gwrmodel.n_lm_hospital <-
  as.data.frame(gwrmodel.n_lm_hospital$SDF)

results.gwrmodel.n_lm_dentist <-
  as.data.frame(gwrmodel.n_lm_dentist$SDF)

results.gwrmodel.n_lm_clinic <-
  as.data.frame(gwrmodel.n_lm_clinic$SDF)

results.gwrmodel.n_lm_police <-
  as.data.frame(gwrmodel.n_lm_police$SDF)

results.gwrmodel.n_lm_bank <- as.data.frame(gwrmodel.n_lm_bank$SDF)

results.gwrmodel.n_lm_atm <- as.data.frame(gwrmodel.n_lm_atm$SDF)

results.gwrmodel.n_lm_restaurant <-
  as.data.frame(gwrmodel.n_lm_restaurant$SDF)

results.gwrmodel.n_lm_fastfood <-
  as.data.frame(gwrmodel.n_lm_fastfood$SDF)

results.gwrmodel.n_lm_placeofworship <-
  as.data.frame(gwrmodel.n_lm_placeofworship$SDF)

results.gwrmodel.n_lm_busstop <-
  as.data.frame(gwrmodel.n_lm_busstop$SDF)

results.gwrmodel.n_lm_streetlamp <-
  as.data.frame(gwrmodel.n_lm_streetlamp$SDF)

results.gwrmodel.n_lm_hotel <-
  as.data.frame(gwrmodel.n_lm_hotel$SDF)

results.gwrmodel.n_lm_industrial <-
  as.data.frame(gwrmodel.n_lm_industrial$SDF)

results.gwrmodel.n_lm_apartments <-
  as.data.frame(gwrmodel.n_lm_apartments$SDF)

results.gwrmodel.n_lm_house <-
  as.data.frame(gwrmodel.n_lm_house$SDF)

results.gwrmodel.n_lm_church <-
  as.data.frame(gwrmodel.n_lm_church$SDF)

results.gwrmodel.n_lm_mosque <-
  as.data.frame(gwrmodel.n_lm_mosque$SDF)

results.gwrmodel.n_lm_footway <-
  as.data.frame(gwrmodel.n_lm_footway$SDF)

results.gwrmodel.n_lm_primary <-
  as.data.frame(gwrmodel.n_lm_primary$SDF)

results.gwrmodel.n_lm_residential <-
  as.data.frame(gwrmodel.n_lm_residential$SDF)

results.gwrmodel.n_lm_unclassified <-
  as.data.frame(gwrmodel.n_lm_unclassified$SDF)




#Bind the GWR result to the SPDF
gwr.map.n_lm_osmuid <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_osmuid))####

gwr.map.n_lm_totaledits <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_totaledits))

gwr.map.n_lm_point <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_point))

gwr.map.n_lm_line <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_line))

gwr.map.n_lm_polygon <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_polygon))

gwr.map.n_lm_ftdensity <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_ftdensity))

gwr.map.n_lm_school <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_school))

gwr.map.n_lm_college <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_college))

gwr.map.n_lm_pub <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_pub))

gwr.map.n_lm_bar <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_bar))

gwr.map.n_lm_pharmacy <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_pharmacy))

gwr.map.n_lm_hospital <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_hospital))

gwr.map.n_lm_dentist <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_dentist))

gwr.map.n_lm_clinic <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_clinic))

gwr.map.n_lm_police <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_police))

gwr.map.n_lm_bank <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_bank))

gwr.map.n_lm_atm <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_atm))

gwr.map.n_lm_restaurant <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_restaurant))

gwr.map.n_lm_fastfood <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_fastfood))

gwr.map.n_lm_placeofworship <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_placeofworship))

gwr.map.n_lm_busstop <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_busstop))

gwr.map.n_lm_streetlmap <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_streetlamp))


gwr.map.n_lm_hotel <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_hotel))
gwr.map.n_lm_industrial <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_industrial))

gwr.map.n_lm_apartments <-
  cbind(n_jointrial,
        as.matrix(results.gwrmodel.n_lm_apartments))

gwr.map.n_lm_house <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_house))

gwr.map.n_lm_church <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_church))

gwr.map.n_lm_mosque <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_mosque))

gwr.map.n_lm_footway <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_footway))

gwr.map.n_lm_primary <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_primary))

gwr.map.n_lm_residential <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_residential))

gwr.map.n_lm_unclassified <-
  cbind(n_jointrial, as.matrix(results.gwrmodel.n_lm_unclassified))


#Map the local R2 results for visual investigation
#Only global R2 will be used in the report 
qtm(gwr.map.n_lm_osmuid, fill = "localR2")
qtm(gwr.map.n_lm_totaledits, fill = "localR2")
qtm(gwr.map.n_lm_point, fill = "localR2")
qtm(gwr.map.n_lm_line, fill = "localR2")
qtm(gwr.map.n_lm_polygon, fill = "localR2")
qtm(gwr.map.n_lm_ftdensity, fill = "localR2")
qtm(gwr.map.n_lm_school, fill = "localR2")
qtm(gwr.map.n_lm_college, fill = "localR2")
qtm(gwr.map.n_lm_pub, fill = "localR2")
qtm(gwr.map.n_lm_bar, fill = "localR2")
qtm(gwr.map.n_lm_pharmacy, fill = "localR2")
qtm(gwr.map.n_lm_hospital, fill = "localR2")
qtm(gwr.map.n_lm_dentist, fill = "localR2")
qtm(gwr.map.n_lm_clinic, fill = "localR2")
qtm(gwr.map.n_lm_police, fill = "localR2")
qtm(gwr.map.n_lm_bank, fill = "localR2")#gooooooood
qtm(gwr.map.n_lm_atm, fill = "localR2")
qtm(gwr.map.n_lm_restaurant, fill = "localR2") #gooood
qtm(gwr.map.n_lm_fastfood, fill = "localR2") #goood
qtm(gwr.map.n_lm_placeofworship, fill = "localR2")
qtm(gwr.map.n_lm_busstop, fill = "localR2")
qtm(gwr.map.n_lm_streetlmap, fill = "localR2")
qtm(gwr.map.n_lm_hotel, fill = "localR2") #gooood
qtm(gwr.map.n_lm_apartments, fill = "localR2")
qtm(gwr.map.n_lm_house, fill = "localR2")
qtm(gwr.map.n_lm_mosque, fill = "localR2")
qtm(gwr.map.n_lm_footway, fill = "localR2")
qtm(gwr.map.n_lm_primary, fill = "localR2")
qtm(gwr.map.n_lm_residential, fill = "localR2")
qtm(gwr.map.n_lm_unclassified, fill = "localR2")

#Write shapefile to be imported into ArcGIS
writeOGR(n_jointrial, ".", "_jointrial", driver = "ESRI Shapefile")


gm_freq <-
  read_excel("ward_amenity_frequency.xlsx", sheet = "nairobi")


gm_freqjoin <-
  left_join(nairobi_join3, gm_freq, by = c("NAME" = "ward"))
View(gm_freqjoin)
gm_freqjoin[is.na(gm_freqjoin)] <- 0


n_jointrial4 <- merge(nairobi_wardshp2, gm_freqjoin)
writeOGR(n_jointrial4, ".", "n_jointrial4", driver = "ESRI Shapefile")

write.csv(manchester_join3, file = "Manc_Dep_Appendix.csv")



#Print the GWR results to the console
gwrmodel.n_lm_osmuid
gwrmodel.n_lm_totaledits 
gwrmodel.n_lm_point 
gwrmodel.n_lm_line 
gwrmodel.n_lm_polygon 
gwrmodel.n_lm_ftdensity 
gwrmodel.n_lm_school 
gwrmodel.n_lm_college
gwrmodel.n_lm_pub 
gwrmodel.n_lm_bar 
gwrmodel.n_lm_pharmacy 
gwrmodel.n_lm_hospital 
gwrmodel.n_lm_dentist
gwrmodel.n_lm_clinic
gwrmodel.n_lm_police
gwrmodel.n_lm_bank 
gwrmodel.n_lm_atm 
gwrmodel.n_lm_restaurant 
gwrmodel.n_lm_fastfood
gwrmodel.n_lm_toilets 
gwrmodel.n_lm_drinkingwater 
gwrmodel.n_lm_placeofworship 
gwrmodel.n_lm_busstop 
gwrmodel.n_lm_streetlamp 
gwrmodel.n_lm_hotel 
gwrmodel.n_lm_industrial
gwrmodel.n_lm_apartments 
gwrmodel.n_lm_house 
gwrmodel.n_lm_church 
gwrmodel.n_lm_mosque 
gwrmodel.n_lm_footway
gwrmodel.n_lm_primary 
gwrmodel.n_lm_residential
gwrmodel.n_lm_unclassified 


#Grouped Variables

#Add in additional grouped variables and repeat the above method

#Grouped dependent variables
n.group <- read_excel("Nairobi_wards_whatismapped.xlsx", sheet = "Sheet1") 

#Independent
Nairobi <- read_excel("Case_study_common_data.xlsx", sheet = "Nairobi") 

#Join dataframes
n.join <- left_join(n_indepdf, n.group, by=c("name"="ward"))


n.name <- colnames(n.join)
n.join2 <- data.frame(sapply(n.join, function(x) as.numeric(as.character(x))))
colnames(n.join2) <- n.name
n.join2[,1:2] <- n.join[,1:2]
n.join2$pop_density <- n_sum$population_density
names(n.join2)[20] <- "education level index"



#Linear regression for grouped variables
n.lm.group.rec <- lm(n.join2$rec ~
                       n.join2$`general sex ratio (females to males)`+ 
                       n.join2$`% of primary school attendance (6-13)`+ 
                       n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                       n.join2$`education level index`+ 
                       n.join2$`% households owning own livestock`+
                       n.join2$`% pop 18-64`+ 
                       n.join2$`% households with 1-3 people`+
                       n.join2$`% of female headed households`+ 
                       n.join2$`% of households owning house they live in`+
                       n.join2$`% Employment Rate`+ 
                       n.join2$`% access to safe water source`+ 
                       n.join2$`% access to improved sanitation`+
                       n.join2$pop_density)

n.lm.group.edu <- lm(n.join2$education ~
                       n.join2$`general sex ratio (females to males)`+ 
                       n.join2$`% of primary school attendance (6-13)`+ 
                       n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                       n.join2$`education level index`+ 
                       n.join2$`% households owning own livestock`+
                       n.join2$`% pop 18-64`+ 
                       n.join2$`% households with 1-3 people`+
                       n.join2$`% of female headed households`+ 
                       n.join2$`% of households owning house they live in`+
                       n.join2$`% Employment Rate`+ 
                       n.join2$`% access to safe water source`+ 
                       n.join2$`% access to improved sanitation`+
                       n.join2$pop_density
)


n.lm.group.rel <- lm(n.join2$rel ~
                       n.join2$`general sex ratio (females to males)`+ 
                       n.join2$`% of primary school attendance (6-13)`+ 
                       n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                       n.join2$`education level index`+ 
                       n.join2$`% households owning own livestock`+
                       n.join2$`% pop 18-64`+ 
                       n.join2$`% households with 1-3 people`+
                       n.join2$`% of female headed households`+ 
                       n.join2$`% of households owning house they live in`+
                       n.join2$`% Employment Rate`+ 
                       n.join2$`% access to safe water source`+ 
                       n.join2$`% access to improved sanitation`+
                       n.join2$pop_density
)


n.lm.group.prop <- lm(n.join2$prop ~
                        n.join2$`general sex ratio (females to males)`+ 
                        n.join2$`% of primary school attendance (6-13)`+ 
                        n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                        n.join2$`education level index`+ 
                        n.join2$`% households owning own livestock`+
                        n.join2$`% pop 18-64`+ 
                        n.join2$`% households with 1-3 people`+
                        n.join2$`% of female headed households`+ 
                        n.join2$`% of households owning house they live in`+
                        n.join2$`% Employment Rate`+ 
                        n.join2$`% access to safe water source`+ 
                        n.join2$`% access to improved sanitation`+
                        n.join2$pop_density
)


n.lm.group.infra <- lm(n.join2$infra ~
                         n.join2$`general sex ratio (females to males)`+ 
                         n.join2$`% of primary school attendance (6-13)`+ 
                         n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                         n.join2$`education level index`+ 
                         n.join2$`% households owning own livestock`+
                         n.join2$`% pop 18-64`+ 
                         n.join2$`% households with 1-3 people`+
                         n.join2$`% of female headed households`+ 
                         n.join2$`% of households owning house they live in`+
                         n.join2$`% Employment Rate`+ 
                         n.join2$`% access to safe water source`+ 
                         n.join2$`% access to improved sanitation`+
                         n.join2$pop_density
)


n.lm.group.ps <- lm(n.join2$ps ~
                      n.join2$`general sex ratio (females to males)`+ 
                      n.join2$`% of primary school attendance (6-13)`+ 
                      n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                      n.join2$`education level index`+ 
                      n.join2$`% households owning own livestock`+
                      n.join2$`% pop 18-64`+ 
                      n.join2$`% households with 1-3 people`+
                      n.join2$`% of female headed households`+ 
                      n.join2$`% of households owning house they live in`+
                      n.join2$`% Employment Rate`+ 
                      n.join2$`% access to safe water source`+ 
                      n.join2$`% access to improved sanitation`+
                      n.join2$pop_density
)


#Output from grouped variables 
summary(n.lm.group.rec)
summary(n.lm.group.edu)
summary(n.lm.group.rel)
summary(n.lm.group.prop)
summary(n.lm.group.infra)
summary(n.lm.group.ps)


#Reload shapefile
nairobi_wardshp1 <- readOGR(dsn=".", layer = "nairobi_wardsshp1")

#Global Spatial Autocorrelation from grouped variables 
moran.test(n.join2$rec, n_listw)
moran.test(n.join2$education, n_listw)
moran.test(n.join2$rel, n_listw)
moran.test(n.join2$prop, n_listw)
moran.test(n.join2$infra, n_listw)
moran.test(n.join2$ps, n_listw)

#Recalculate listw
n_listw <- nb2listw(n_neighbours)

#Change column name to facilliate the join
colnames(n.join2)[1] <- "NAME"

#Join dataframe to shapefile
n.group.join <- merge(nairobi_wardshp1, n.join2)

#Write shapeifle for use in ArcGIS
writeOGR(n.group.join, ".", "n_group", driver="ESRI Shapefile")

#Calculatye Bandwidth
n.rec <- gwr.sel(n.join2$rec ~
                   n.join2$`general sex ratio (females to males)`+ 
                   n.join2$`% of primary school attendance (6-13)`+ 
                   n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                   n.join2$`education level index`+ 
                   n.join2$`% households owning own livestock`+
                   n.join2$`% pop 18-64`+ 
                   n.join2$`% households with 1-3 people`+
                   n.join2$`% of female headed households`+ 
                   n.join2$`% of households owning house they live in`+
                   n.join2$`% Employment Rate`+ 
                   n.join2$`% access to safe water source`+ 
                   n.join2$`% access to improved sanitation`,
                 data=n.group.join, adapt = T)

n.edu <- gwr.sel(n.join2$education ~
                   n.join2$`general sex ratio (females to males)`+ 
                   n.join2$`% of primary school attendance (6-13)`+ 
                   n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                   n.join2$`education level index`+ 
                   n.join2$`% households owning own livestock`+
                   n.join2$`% pop 18-64`+ 
                   n.join2$`% households with 1-3 people`+
                   n.join2$`% of female headed households`+ 
                   n.join2$`% of households owning house they live in`+
                   n.join2$`% Employment Rate`+ 
                   n.join2$`% access to safe water source`+ 
                   n.join2$`% access to improved sanitation`,
                 data=n.group.join, adapt = T
)


n.rel <- gwr.sel(n.join2$rel ~
                   n.join2$`general sex ratio (females to males)`+ 
                   n.join2$`% of primary school attendance (6-13)`+ 
                   n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                   n.join2$`education level index`+ 
                   n.join2$`% households owning own livestock`+
                   n.join2$`% pop 18-64`+ 
                   n.join2$`% households with 1-3 people`+
                   n.join2$`% of female headed households`+ 
                   n.join2$`% of households owning house they live in`+
                   n.join2$`% Employment Rate`+ 
                   n.join2$`% access to safe water source`+ 
                   n.join2$`% access to improved sanitation`,
                 data=n.group.join, adapt = T
)


n.prop <- gwr.sel(n.join2$prop ~
                    n.join2$`general sex ratio (females to males)`+ 
                    n.join2$`% of primary school attendance (6-13)`+ 
                    n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                    n.join2$`education level index`+ 
                    n.join2$`% households owning own livestock`+
                    n.join2$`% pop 18-64`+ 
                    n.join2$`% households with 1-3 people`+
                    n.join2$`% of female headed households`+ 
                    n.join2$`% of households owning house they live in`+
                    n.join2$`% Employment Rate`+ 
                    n.join2$`% access to safe water source`+ 
                    n.join2$`% access to improved sanitation`,
                  data=n.group.join, adapt = T
)


n.infra <- gwr.sel(n.join2$infra ~
                     n.join2$`general sex ratio (females to males)`+ 
                     n.join2$`% of primary school attendance (6-13)`+ 
                     n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                     n.join2$`education level index`+ 
                     n.join2$`% households owning own livestock`+
                     n.join2$`% pop 18-64`+ 
                     n.join2$`% households with 1-3 people`+
                     n.join2$`% of female headed households`+ 
                     n.join2$`% of households owning house they live in`+
                     n.join2$`% Employment Rate`+ 
                     n.join2$`% access to safe water source`+ 
                     n.join2$`% access to improved sanitation`,
                   data=n.group.join, adapt = T
)


n.ps <- gwr.sel(n.join2$ps ~
                  n.join2$`general sex ratio (females to males)`+ 
                  n.join2$`% of primary school attendance (6-13)`+ 
                  n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                  n.join2$`education level index`+ 
                  n.join2$`% households owning own livestock`+
                  n.join2$`% pop 18-64`+ 
                  n.join2$`% households with 1-3 people`+
                  n.join2$`% of female headed households`+ 
                  n.join2$`% of households owning house they live in`+
                  n.join2$`% Employment Rate`+ 
                  n.join2$`% access to safe water source`+ 
                  n.join2$`% access to improved sanitation`,
                data=n.group.join, adapt = T
)


#GWR models
n.rec.model <- gwr(n.join2$rec ~
                     n.join2$`general sex ratio (females to males)`+ 
                     n.join2$`% of primary school attendance (6-13)`+ 
                     n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                     n.join2$`education level index`+ 
                     n.join2$`% households owning own livestock`+
                     n.join2$`% pop 18-64`+ 
                     n.join2$`% households with 1-3 people`+
                     n.join2$`% of female headed households`+ 
                     n.join2$`% of households owning house they live in`+
                     n.join2$`% Employment Rate`+ 
                     n.join2$`% access to safe water source`+ 
                     n.join2$`% access to improved sanitation`,
                   data=n.group.join, adapt = n.rec, hatmatrix=TRUE, se.fit=TRUE)

n.edu.model <- gwr(n.join2$education ~
                     n.join2$`general sex ratio (females to males)`+ 
                     n.join2$`% of primary school attendance (6-13)`+ 
                     n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                     n.join2$`education level index`+ 
                     n.join2$`% households owning own livestock`+
                     n.join2$`% pop 18-64`+ 
                     n.join2$`% households with 1-3 people`+
                     n.join2$`% of female headed households`+ 
                     n.join2$`% of households owning house they live in`+
                     n.join2$`% Employment Rate`+ 
                     n.join2$`% access to safe water source`+ 
                     n.join2$`% access to improved sanitation`,
                   data=n.group.join, adapt = n.edu, hatmatrix=TRUE, se.fit=TRUE)

n.rel.model <- gwr(n.join2$rel ~
                     n.join2$`general sex ratio (females to males)`+ 
                     n.join2$`% of primary school attendance (6-13)`+ 
                     n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                     n.join2$`education level index`+ 
                     n.join2$`% households owning own livestock`+
                     n.join2$`% pop 18-64`+ 
                     n.join2$`% households with 1-3 people`+
                     n.join2$`% of female headed households`+ 
                     n.join2$`% of households owning house they live in`+
                     n.join2$`% Employment Rate`+ 
                     n.join2$`% access to safe water source`+ 
                     n.join2$`% access to improved sanitation`,
                   data=n.group.join, adapt = n.rel, hatmatrix=TRUE, se.fit=TRUE)


n.prop.model <- gwr(n.join2$prop ~
                      n.join2$`general sex ratio (females to males)`+ 
                      n.join2$`% of primary school attendance (6-13)`+ 
                      n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                      n.join2$`education level index`+ 
                      n.join2$`% households owning own livestock`+
                      n.join2$`% pop 18-64`+ 
                      n.join2$`% households with 1-3 people`+
                      n.join2$`% of female headed households`+ 
                      n.join2$`% of households owning house they live in`+
                      n.join2$`% Employment Rate`+ 
                      n.join2$`% access to safe water source`+ 
                      n.join2$`% access to improved sanitation`,
                    data=n.group.join, adapt = n.prop, hatmatrix=TRUE, se.fit=TRUE)


n.infra.model <- gwr(n.join2$infra ~
                       n.join2$`general sex ratio (females to males)`+ 
                       n.join2$`% of primary school attendance (6-13)`+ 
                       n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                       n.join2$`education level index`+ 
                       n.join2$`% households owning own livestock`+
                       n.join2$`% pop 18-64`+ 
                       n.join2$`% households with 1-3 people`+
                       n.join2$`% of female headed households`+ 
                       n.join2$`% of households owning house they live in`+
                       n.join2$`% Employment Rate`+ 
                       n.join2$`% access to safe water source`+ 
                       n.join2$`% access to improved sanitation`,
                     data=n.group.join, adapt = n.infra,hatmatrix=TRUE, se.fit=TRUE)


n.ps.model <- gwr(n.join2$ps ~
                    n.join2$`general sex ratio (females to males)`+ 
                    n.join2$`% of primary school attendance (6-13)`+ 
                    n.join2$`Secondary School Attendance of 14- to 17-Year-Olds`+
                    n.join2$`education level index`+ 
                    n.join2$`% households owning own livestock`+
                    n.join2$`% pop 18-64`+ 
                    n.join2$`% households with 1-3 people`+
                    n.join2$`% of female headed households`+ 
                    n.join2$`% of households owning house they live in`+
                    n.join2$`% Employment Rate`+ 
                    n.join2$`% access to safe water source`+ 
                    n.join2$`% access to improved sanitation`,
                  data=n.group.join, adapt = n.ps,hatmatrix=TRUE, se.fit=TRUE)

#Print output of GWR to console
n.ps.model 
n.edu.model 
n.rec.model
n.rel.model
n.prop.model 
n.infra.model 

#Calculate AIC values for comparison with MLR
AIC(n.lm.group.ps)
AIC(n.lm.group.edu)
AIC(n.lm.group.rec) 
AIC(n.lm.group.rel) 
AIC(n.lm.group.prop) 
AIC(n.lm.group.infra)



