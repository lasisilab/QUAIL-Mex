library(tidyr)
library(dplyr)


#Loading file
d <- read.csv("~/R/Screening/Screening.x", na.strings = "")
head(d)

# Separate year of birth and age in column Birthyear
d_sep <- d %>% 
  separate(BirthYear, into = c("BirthYear", "Age2"), sep = " - ") %>%
#replace cells without data by NAs
  mutate(across(everything(), ~ ifelse(. == "#VALUE!", NA, .)))
head(d_sep, 25)

# combine information from two columns into one
d_sep$Age3 <- coalesce(d_sep$Age, d_sep$Age2)
head(d_sep)

d1 <- d_sep %>%
  select(!(c(Age, Age2))) 
head(d1)


# Combine columns that contain comments
d2_sep$CommentsScreening <- coalesce(d2_sep$Comm_screening, d2_sep$Comm_screening.1, d2_sep$Comm_screening.1.1)
head(d2_sep)


# Re-Order columns
d2 <- d1 %>% 
    select("SampleID", "WS.WI", "BirthYear", "Age3", "Zip_code", "YearsInNeighborhood", "Smoke", "HormonalTreatment", "Medications", "ChronicPain", "ChronicDisease", "HouseholdSize", "HouseholdNum_SharedWater", "ChildrenNum", "Employ_formal", "WorkingAdults", "WorkingAdults.1", "YearsEducation",  "YearsEducation.1",  "BathroomNum.1",    "BathroomNum", "CarNum", "CarNum.1", "Internet.1", "Internet", "BedroomsNum", "BedroomsNum.1"    , "TotalSES", "TotalSES.1",     "Comm_screening", "Comm_screening.1", "Comm_screening.1.1", "WaterStorageInfr", "HouseholdNum_SharedWater", "Work_days.week")

# Separate number of people working in household
d2_sep <- d2 %>% 
  separate(WorkingAdults.1, into = c("WorkingAdultNum", "WorkingAdultScore"), sep = " - ") %>%
  #replace cells without data by NAs
  mutate(across(everything(), ~ ifelse(. == "#VALUE!", NA, .))) %>%
  select(!c(WorkingAdultScore, BathroomNum.1, CarNum.1, Internet.1, YearsEducation.1, BedroomsNum.1))
head(d2_sep, 25)
  

c <- read.csv("~/R/Screening/Chronic_pain_illness.csv", na.strings = "")
head(c)

# clean file
c2 <- c %>% 
  select(-e) %>%
  #replace cells without data by NAs
  mutate(across(everything(), ~ ifelse(. == "#VALUE!", NA, .)))
head(c2, 30)

#Translate to English
c2$Chronic_pain[c$Chronic_pain == "Si"] <- "Yes"
c2$SampleID <- as.numeric(c2$SampleID)

# combine information from two df into one
d3 <- merge(c2, d, by = "SampleID")

# combine with hours of water supply (../R)
hrs <- read.csv("~/R/hours_water_supply.csv", na.strings = "")

colnames(hrs) <- c("SampleID", "ContinIntermit", "HrsPerWeek") 
hrs$SampleID <- as.character(hrs$SampleID)

# combine df into one
d4 <- merge(d3, hrs, by = "SampleID")

#Translate to English
d4$ContinIntermit[d4$ContinIntermit == "continua"] <- "Continuous"
d4$ContinIntermit[d4$ContinIntermit == "Intermitent"] <- "Intermittent"
head(d4)
summary(d4)
# save file
write.csv(d4, "Screening_V2.csv", row.names = F)

