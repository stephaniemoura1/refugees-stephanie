######################################################
########## Federal University of Pernambuco ##########
############ Political Science Department ############
######### Masters Degree in Political Science ########
######################################################
###### Paper for the discipline of data analysis #####
######################################################
############### Professor Davi Moreira ###############
######## Student: Stephanie Moura de Oliveira ########
######################################################
###################### Title: ########################
#### Is welfare state the priority? Refugees flow ####
##### through Europe and their target countries ######
######################################################

### Commented Script ###

# Open Packages

if(require(tidyverse) == F) install.packages('tidyverse'); require (tidyverse)
if(require(readxl) == F) install.packages('readxl'); require (readxl)
if(require(readr) == F) install.packages('readr'); require (readr)
if(require(ggplot2) == F) install.packages('ggplot2'); require (ggplot2)
if(require(stargazer) == F) install.packages('stargazer'); require (stargazer)

# Open Databases

unhcr <- read_csv("Dados/Stephanie-Moura-bd-unhcr-tf-ad-ufpe-2018.csv", 
                  skip = 3)

# Prior to the analysis, I have carefully read all the variables on the 
# QOG database to see which ones were mostly suitable for the welfare
# analysis. I have chosen 58 variables, from which I chose the ones I used in 
# the final model

# That said, It's necessary to open two QOG databases: the "qog_2", which is the 
# complete database, with all the countries and variables; and the "qog", which 
# is the database that I have modified, including only the countries present
# in the study and the variables of interest. 

qog_2 <- read_csv("Dados/Stephanie-Moura-bd-qog_2-tf-ad-ufpe-2018.csv")
qog <- read_excel("Dados/Stephanie-Moura-bd-qog-tf-ad-ufpe-2018.xlsx")

# Now It's necessary to transform the variable "applied during year" at the 
# unhcr database from a categoric to a numeric variable. 

unhcr$`Applied during year` <- as.numeric(unhcr$`Applied during year`)

# Summarise the cases by year and country at the unhcr database, ignoring
# cases where the quantity was N/A and ignoring the categories of different
# types of refugees, uniting all in one count.

unhcr <- summarise(group_by(unhcr, Year, `Country / territory of asylum/residence`), 
                   Applied = sum(`Applied during year`, na.rm = T))

# change the variable name from `Country / territory of asylum/residence` to 
# "Country_Asylum" to facilitate the manipulation of the data and the variable

names(unhcr) <- c("Year", "Country_Asylum", "Applied")

# change the names of variables that are different in the UNHCR and QOG databases
# to then unite the databases.

unhcr$Country_Asylum[unhcr$Country_Asylum == "France"] <- "France (1963-)"
unhcr$Country_Asylum[unhcr$Country_Asylum == "Russian Federation"] <- "Russia"
unhcr$Country_Asylum[unhcr$Country_Asylum == "The former Yugoslav Republic of Macedonia"] <- "Macedonia"
unhcr$Country_Asylum[unhcr$Country_Asylum == "Czech Rep."] <- "Czech Republic"
unhcr$Country_Asylum[unhcr$Country_Asylum == "Serbia and Kosovo (S/RES/1244 (1999))" ] <- "Serbia"

# create a new object, restricting the "qog_2" database to only the countries 
# that are present on the "unhcr" database, creating a new object, caleed "qog_3"
# reducing from 15.192 observations to 2.088.

qog_3 <- filter(qog_2, cname %in% unhcr$Country_Asylum)

# restrict the object one more time, delimitating only the years 2011 and 2015,
# decreasing database size from 2088 to 78 observations.

qog_3 <- filter(qog_3, year == 2011 | year == 2015)

# Select from the "qog_3" variables, only the variables that have the greatest chance
# of being of interest to the research, selected in the previous moment next to the
# QOG codebook

qog_4 <- select(qog_3, c("year", intersect(names(qog), names(qog_3))) )

# unite the two databases: "qog_4" and "unhcr" by country and year and creates the database
# that will be used for analysis: "base".

base <- left_join(qog_4, 
                  unhcr, by = c("year" = "Year", "cname" = "Country_Asylum"))

# removes the databases that will no longer be used and have already been modified.

rm("qog", "qog_2", "qog_3")

# In the Applied variable, it replaces all cases that are listed as N / A by 0.

base$Applied[is.na(base$Applied)] <- 0

# save database
write.table(base, "Dados/Dados tratados/dataset-stephanie-preprocess.csv", row.names = F, sep = ";")
