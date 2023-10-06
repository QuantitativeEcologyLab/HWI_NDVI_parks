# Loading packages ---- 
options(timeout = max(1000, getOption("timeout")))
library(lattice) # for making graphs
library(knitr) # for knitting
library(ggplot2) # for scatter plot
library(dplyr) # for pipes
library(skimr) # for skimming data
library(tidyverse) #summing
library(lubridate) #convert whole columns to dates

#set working directory
setwd("C:/Users/gracelou/OneDrive - UBC/Documents/GitHub/data-exploration")

#importing all the datasets to see which ones are useful ----
#3 
coexistence_incidents_record <- read.csv("data/pca-human-wildlife-coexistence-incidents-detailed-records-2010-2021.csv")
#4
animals_involved <- read.csv("data/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")
#5
responses <- read.csv("data/pca-human-wildlife-coexistence-responses-detailed-records-2010-2021.csv")
#6
activites <- read.csv("data/pca-human-wildlife-coexistence-activities-detailed-records-2010-2021.csv")
#7
incidents_by_incidents_type <- read.csv("data/pca-human-wildlife-coexistence-incidents-detailed-records-2010-2021.csv")
#8
staff_time_incident <- read.csv("data/pca-hours-of-staff-time-by-incident-type-2010-2021.csv")
#9
number_incidents_species <- read.csv("data/pca-number-of-incidents-by-species-2010-2021.csv")
#10
staff_time_species <- read.csv("data/pca-hours-of-staff-time-by-species-2010-2021.csv")
#11
animals_killed_human_cause <- read.csv("data/pca-number-of-animals-killed-by-human-causes-2010-2021.csv")
#12
aggressive_encounters_species <- read.csv("data/pca-number-of-aggressive-encounters-by-species-2010-2021.csv")
#13
animals_involved_unnatural_attractants <- read.csv("data/pca-number-of-animals-involved-with-unnatural-attractants-2010-2021.csv")
#14
animals_killed_collisions <-read.csv("data/pca-number-of-animals-killed-by-collisions-with-vehicles-and-trains-2010-2021.csv")
#15
incidents_by_response <- read.csv("data/pca-number-of-incidents-by-response-type-2010-2021.csv")
#16
incidents_by_site <- read.csv("data/pca-number-of-incidents-by-site-2010-2021.csv")
#17
staff_time_site <- read.csv("data/pca-hours-of-staff-time-by-site-2010-2021.csv")
#18
animals_killed_collisions_site <- read.csv("data/pca-number-of-animals-killed-by-collisions-with-vehicles-and-trains-by-site-2010-2021.csv")
#19
aggressive_encounters_species_site <- read.csv("data/pca-number-of-aggressive-encounters-by-species-and-by-site-2010-2021.csv")
#Choosing 4 (animals_involved) and 19 (agressive_encounters_species_site)

#Skim animals involved data ----
skim_without_charts("animals-involved")

#filter out all the human wildlife interactions ----
HWI <- animals_involved %>% 
  filter(Incident.Type %in% c("Human Wildlife Interaction"))

#visualize the number of individuals per species ----
ggplot(data = HWI, aes(x = Species.Common.Name, y = Sum.of.Number.of.Animals)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  ylab("Frequency") +
  xlab("Species") +
  theme_bw()

#Cleaning the first nations heritage site in HWI data ----
HWI$Protected.Heritage.Area[HWI$Protected.Heritage.Area == "Saoy\xfa-?ehdacho National Historic Site of Canada"]<- "Grizzly Bear Mountain and Scented Grass Hills"

#visualize the number of individuals per region ----
ggplot(data = HWI, aes(x = Protected.Heritage.Area, y = Sum.of.Number.of.Animals)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  ylab("Frequency") +
  xlab("Region") +
  theme_bw()

# count the number of regions and the number of species in HWI ----
regioncount <- HWI %>% 
  count(HWI$Protected.Heritage.Area)

speciescount <- HWI %>% 
  count(HWI$Species.Common.Name)

# count the number of regions with aggressive encounters ----
aggressive_regioncount <- aggressive_encounters_species_site %>% 
  count(aggressive_encounters_species_site$Protected.Heritage.Area)

# count the number of each species engaged in aggressive encounters ----
aggressive_speciescount <- aggressive_encounters_species_site %>% 
  count(aggressive_encounters_species_site$Species.Common.Name)

#conclusion: same(?) 31 protected sites, but 152 species engaged in HWI, while only 87 was recorded in aggressive encounters
#only using the animal-involved data

# Convert dates in HWI from characters to date ----
HWI$Incident.Date <- ymd(HWI$Incident.Date)

# Add a column "Incident Year" to HWI ----
HWI$Incident.Year <- as.numeric(format(HWI$Incident.Date, "%Y"))

# Add a colume "Incident Month" to HWI ----
HWI$Incident.Month <- as.numeric(format(HWI$Incident.Date, "%m"))

# Count incident by year in HWI ----
incidentcountbyyear <- HWI %>% 
  count(HWI$Incident.Year)
#incidents increase by year






