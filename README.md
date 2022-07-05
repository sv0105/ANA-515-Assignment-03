# ANA-515-Assignment-03
ANA 515 Assignment 03


installed.packages()
getwd()
library(tidyverse)

#Read and save the csv as data frame "wrs" for Weather Related Storms

wrs <- read_csv("C:\\Users\\Veere\\Desktop\\StormEvents_details-ftp_v1.0_d1989_c20220425.csv\\StormEvents_details-ftp_v1.0_d1989_c20220425.csv")

view(wrs)
head(wrs, 10)
colnames(wrs)


#Limiting wrs by creating a subset data frame "wrs_n" to the following columns: 

# "BEGIN_DATE_TIME" 
# "END_DATE_TIME"
# "EPISODE_ID"
# "EVENT_ID"
# "STATE" 
# "STATE_FIPS"
# "CZ_NAME"
# "CZ_TYPE"
# "CZ_FIPS" 
# "EVENT_TYPE"
# "SOURCE"
# "BEGIN_LAT"         
# "BEGIN_LON"          
# "END_LAT"            
# "END_LON"

wrs_n <- wrs[c(7,8,9,10,13,14,15,16,18,20,27,45,46,47,48)]

view(wrs_n)
head(wrs_n, 10)

#Arrange the data by state name

library(dplyr)
SortByState <- arrange(wrs_n,(STATE))
head(SortByState, 10)

#Change state and county names to title case 

install.packages("stringr")
library(stringr)
StateTitleCase <- str_to_title(wrs_n$STATE)
CountyTitleCase <- str_to_title(wrs_n$CZ_NAME)

#Limit the events listed by county FIPS (CZ_TYPE of "C") 

CFilter <- filter(wrs_n, CZ_TYPE=="C")

#Remove CZ_TYPE column

RemoveColumn <- select(wrs_n, -CZ_TYPE)

#Padding the state and county FIPS with a "0" at the beginning

StatePadding <- str_pad(wrs_n$STATE_FIPS, width=3, side="left", pad="0")
CountyPadding <- str_pad(wrs_n$CZ_FIPS, width=3, side="left", pad="0")

#Unite the two columns to make one FIPS column with the 5-digit county FIPS code.

library(tidyr)
NewCol <- unite(wrs_n,"FIPS", c("STATE_FIPS","CZ_FIPS"))

#Changing all the column names to lower case

Lower <- rename_all(wrs_n,tolower)

#Creating a data frame with these three columns: state name, area, and region with base R state data

data("state")
State_Info <- data.frame(state = state.name, region = state.region, area = state.area)

#Create a data frame with the number of events per state in the year of my birth

table(wrs_n$STATE)
StateFreq <- data.frame(table(wrs_n$STATE))
head(StateFreq)


#Merge the state information data frame with state freq data frame

StateFreqN <- rename(StateFreq, c("state"="Var1"))
head(StateFreqN)
StateInfoFreq <- merge(x=State_Info, y=StateFreqN, by.x="state",.y="state")
head(StateInfoFreq)

#Match the letter cases of state info and state freq

StateInfoN <- (mutate_all(State_Info, toupper))
head(StateInfoN)
StateInfoFreq <- merge(x=StateInfoN, y=StateFreqN, by.x="state",.y="state")
head(StateInfoFreq)

#Convert the values for "area" from character to numeric

AreaNew <- as.numeric(StateInfoFreq$area)

#Load a custom font from my local machine       

windowsFonts(CorpoS=windowsFont("CorpoS"))

#Plot generation  

StormPlot <- ggplot(StateInfoFreq, aes(x = AreaNew, y = Freq)) + geom_point(aes(color=region)) + labs(x="Land Area (Square Miles)", y="Storm Events in 1989") + theme(text=element_text(family="CorpoS", size=15)) 

StormPlot
