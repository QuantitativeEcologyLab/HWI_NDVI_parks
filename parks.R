# Loading packages ---- 
options(timeout = max(1000, getOption("timeout")))
library(lattice) # for making graphs
library(knitr) # for knitting
library(ggplot2) # for scatter plot
library(dplyr) # for pipes
library(skimr) # for skimming data
library(tidyverse) #summing
library(lubridate) #convert whole columns to dates
library(zoo) #dates as year month
library(canadianmaps) #import annotated map of Canada
library(sf) # spatial data
library(sp) #Spatial Points function
library(rstudioapi) #for creating colour palette
library(grDevices) #for creating colour palette
library(fBasics) #for creating colour palette
library(mgcv) #gam
library(terra) #shape file 

# set working directory
setwd("C:/Users/grace/Documents/GitHub/HWI_parks")

# importing data
animals_involved <- read.csv("data/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")

# filter out all the human wildlife interactions ----
HWI <- animals_involved %>% 
  filter(Incident.Type %in% c("Human Wildlife Interaction"))

# Cleaning the first nations heritage site in HWI data ----
HWI$Protected.Heritage.Area[HWI$Protected.Heritage.Area == "Saoy\xfa-?ehdacho National Historic Site of Canada"]<- "Grizzly Bear Mountain and Scented Grass Hills"

# Convert dates in HWI from characters to date ----
HWI$Incident.Date <- ymd(HWI$Incident.Date)

# Add a column "Incident Year" to HWI ----
HWI$Incident.Year <- as.numeric(format(HWI$Incident.Date, "%Y"))

# Add a colume "Incident Month" to HWI ----
HWI$Incident.Month <- as.numeric(format(HWI$Incident.Date, "%m"))

# Combine year and month into a single column
HWI$year_month <- as.yearmon(paste(HWI$Incident.Year, HWI$Incident.Month), "%Y %m") 

# Renaming columns in HWI 
HWI_parks <- HWI %>% 
  rename("park" = "Protected.Heritage.Area") %>% 
  rename("species" = "Species.Common.Name") %>% 
  rename("year" = "Incident.Year") %>% 
  rename("month" = "Incident.Month") %>% 
  rename("HWI" = "Incident.Type")

# Shortening park names
HWI_parks$park[HWI_parks$park == "Banff National Park of Canada"]<- "Banff"
HWI_parks$park[HWI_parks$park == "Pacific Rim National Park Reserve of Canada"]<- "Pacific_Rim"
HWI_parks$park[HWI_parks$park == "Waterton Lakes National Park of Canada"]<- "Waterton_Lakes"
HWI_parks$park[HWI_parks$park == "Kejimkujik National Park and National Historic Site of Canada"]<- "Kejimkujik"
HWI_parks$park[HWI_parks$park == "Jasper National Park of Canada"]<- "Jasper"
HWI_parks$park[HWI_parks$park == "Forillon National Park of Canada"]<- "Forillon"
HWI_parks$park[HWI_parks$park == "Prince Albert National Park of Canada"]<- "Prince_Albert"
HWI_parks$park[HWI_parks$park == "Kootenay National Park of Canada"]<- "Kootenay"
HWI_parks$park[HWI_parks$park == "Glacier National Park of Canada"]<- "Glacier"
HWI_parks$park[HWI_parks$park == "Wapusk National Park of Canada"]<- "Wapusk"
HWI_parks$park[HWI_parks$park == "Grasslands National Park of Canada"]<- "Grasslands"
HWI_parks$park[HWI_parks$park == "Bruce Peninsula National Park of Canada"]<- "Bruce_Peninsula"
HWI_parks$park[HWI_parks$park == "Yoho National Park of Canada"]<- "Yoho"
HWI_parks$park[HWI_parks$park == "Terra Nova National Park of Canada"]<- "Terra_Nova"
HWI_parks$park[HWI_parks$park == "Mount Revelstoke National Park of Canada"]<- "Mount_Revelstoke"
HWI_parks$park[HWI_parks$park == "Elk Island National Park of Canada"]<- "Elk_Island"
HWI_parks$park[HWI_parks$park == "Georgian Bay Islands National Park of Canada"]<- "Georgian_Bay_Islands"
HWI_parks$park[HWI_parks$park == "Prince of Wales Fort National Historic Site of Canada"]<- "Prince_of_Wales_Fort"
HWI_parks$park[HWI_parks$park == "Point Pelee National Park of Canada"]<- "Point_Pelee"
HWI_parks$park[HWI_parks$park == "Thousand Islands National Park of Canada"]<- "Thousand_Islands"
HWI_parks$park[HWI_parks$park == "Wood Buffalo National Park of Canada"]<- "Wood_Buffalo"
HWI_parks$park[HWI_parks$park == "Prince Edward Island National Park of Canada"]<- "Prince_Edward_Island"
HWI_parks$park[HWI_parks$park == "Ivvavik National Park of Canada"]<- "Ivvavik"
HWI_parks$park[HWI_parks$park == "Kouchibouguac National Park of Canada"]<- "Kouchibouguac"
HWI_parks$park[HWI_parks$park == "Grizzly Bear Mountain and Scented Grass Hills"]<- "Grizzly_Bear_Mountain"
HWI_parks$park[HWI_parks$park == "Fundy National Park of Canada"]<- "Fundy"
HWI_parks$park[HWI_parks$park == "Nahanni National Park Reserve of Canada"]<- "Nahanni"
HWI_parks$park[HWI_parks$park == "Aulavik National Park of Canada"]<- "Aulavik"
HWI_parks$park[HWI_parks$park == "Sable Island National Park Reserve"]<- "Sable_Island"
HWI_parks$park[HWI_parks$park == "Fathom Five National Marine Park of Canada"]<- "Fathom_Five"
HWI_parks$park[HWI_parks$park == "Fort Walsh National Historic Site of Canada"]<- "Fort_Walsh"

# Cleaning the species by omitting unknowns
HWI_parks <- HWI_parks[HWI_parks$species != "None",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown bear",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown bird",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown bat",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown ungulate",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown gull",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown canid",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown snake",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown fish",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown Duck",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown grouse",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown rodent",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown Myotis bat",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown raptor",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown owl",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown sea lion",]
HWI_parks <- HWI_parks[HWI_parks$species != "Unknown deer",]

#Count number of incidents by park
incident_count <- HWI_parks %>% 
  count(HWI_parks$park)

# import Canada shape and extract boundaries only
canadashape <- st_as_sf(PROV) %>%  
  st_geometry()

# import coordinates of all national parks, coordinates obtained from Google Maps
park_coordinates <- read.csv("data/park_coordinates.csv")

# convert coordinates into spatial data
park_location <- SpatialPoints(select(park_coordinates, longitude, latitude))

# plot parks
plot(canadashape)
sp::plot(park_location, add = TRUE, col = 'coral', pch = 19, cex = 0.5) 
# Ivvavik keeps appearing in Alaska???

#Other visualisations ----

#Create another data frame by grouping according to months and years
HWI_grouped_date <- aggregate(HWI ~ year_month + park, data = HWI_parks, FUN = "length")
HWI_grouped_date$year_month <- as.Date(HWI_grouped_date$year_month, format = "%Y-%m")
HWI_grouped_date$year <- lubridate::year(HWI_grouped_date$year_month)
HWI_grouped_date$month <- lubridate::month(HWI_grouped_date$year_month)

# plot HWI across years and months ----
ggplot() +
  geom_point(data = HWI_grouped_date, aes(x = year_month, y = HWI, col = park)) +
  xlab("Time") +
  ylab("Human-wildlife interactions") +
  # using heat palette for parks
  scale_color_manual(values = heatPalette(n=31)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# group parks according to province ----
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Terra_Nova")] <- "NL"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Prince_Edward_Island")] <- "PE"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Kejimkujik", "Sable_Island")] <- "NS"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Kouchibouguac", "Fundy")] <- "NB"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Forillon")] <- "QC"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Bruce_Peninsula", "Fathom_Five", "Georgian_Bay_Islands", "Point_Pelee", "Thousand_Islands")] <- "ON"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Prince_of_Wales_Fort", "Wapusk")] <- "MB"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Prince_Albert")] <- "SK"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Banff", "Elk_Island", "Grasslands","Jasper", "Waterton_Lakes", "Wood_Buffalo")] <- "AB"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Glacier", "Kootenay", "Mount_Revelstoke", "Pacific_Rim", "Yoho")] <- "BC"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Ivvavik")] <- "YT"
HWI_grouped_date$park[HWI_grouped_date$park %in% c("Aulavik", "Grizzly_Bear_Mountain", "Nahanni")] <- "NT"

# plot parks according to province by month ----
HWI_province <- HWI_grouped_date %>% 
  rename("province" = "park")

ggplot() +
  geom_point(data = HWI_province, aes(x = year_month, y = HWI, col = province), alpha = 0.25) +
  xlab("Time") +
  ylab("Human-wildlife interactions") +
  # using rainbow palette for parks
  scale_color_manual(values = rainbowPalette(n=12)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# plot species sightings??? ----
HWI_grouped_species <- aggregate(HWI ~ year_month + park + species + month + year, data = HWI_parks, FUN = "length")

ggplot() +
  geom_point(data = HWI_grouped_species, aes(x = species, y = HWI, col = species), alpha = 0.8) +
  xlab("Time") +
  ylab("Human-wildlife interactions") +
  # using rainbow palette for parks
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#too much data

#still too much data
ggplot(data = HWI_grouped_species, aes(x = species, y = HWI)) +
  geom_bar(stat = "identity", position = position_dodge(), width=0.5) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1.5)) +
  ylab("No. of Sightings") +
  xlab("Species") +
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_text(size=2, family = "sans", face = "bold"),)

# count no. of sightings per species 
sightings <- HWI_grouped_species %>% 
  count(HWI_grouped_species$species)

# filter the sightings >10 by creating a subset (top 14 species)
filtered_sightings <- subset(HWI_grouped_species, HWI_grouped_species$HWI>10)

# plot species with >10 sightings (top 14 species) ----
ggplot(data = filtered_sightings, aes(x = species, y = HWI)) +
  geom_bar(stat = "identity", position = position_dodge(), width=0.5) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1.5)) +
  ylab("No. of Sightings") +
  xlab("Species") +
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_text(size=2, family = "sans", face = "bold"),)

# plot species with >10 sightings (top 8 species) with time ----
ggplot() +
  geom_point(data = filtered_sightings, aes(x = year_month, y = HWI, col = species), alpha = 0.8) +
  xlab("Time") +
  ylab("Frequency") +
  # using rainbow palette for parks
  scale_color_manual(values = rainbowPalette(n=14)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# plot HWI according to seasons ----
# creating a new dataset with seasons 
HWI_with_season <- HWI_parks %>%
  mutate(season = case_when(month %in% 3:5 ~ 'Spring',
                            month %in% 6:8 ~ 'Summer',
                            month %in% 9:11 ~ 'Autumn',
                            TRUE ~ 'Winter'))

#creating a new dataframe with seasons
HWI_grouped_season <- aggregate(HWI ~ year_month + park + species + season, data = HWI_with_season, FUN = "length")

HWI_grouped_season %>% 
  count(HWI_grouped_season$season)
sum(HWI_grouped_season$HWI)

#plotting the number of HWI according to season
ggplot(data = HWI_grouped_season, aes(x = season, y = HWI)) +
  geom_bar(stat = "identity", position = position_dodge(), width=0.5) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1.5)) +
  ylab("HWI Frequency") +
  xlab("Season") +
  theme(axis.title.x = element_text(size=8, family = "sans", face = "bold"),)

# filtering the species with >10 sightings ----
filtered_season_sightings <- subset(HWI_grouped_season, HWI_grouped_season$HWI>10)

# plotting the number of sightings of species >10 sightings according to season ----
ggplot() +
  geom_point(data = filtered_season_sightings, aes(x = season, y = HWI, col = species), alpha = 0.8) +
  xlab("Season") +
  ylab("No. of Sightings") +
  # using rainbow palette for parks
  scale_color_manual(values = rainbowPalette(n=14)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# models for visualising the normal trend ----
HWI_grouped_species <- HWI_grouped_species %>% 
  mutate(park = factor(park))

HWI_grouped_species$species <- as.factor(HWI_grouped_species$species)


model1 <- gam(HWI ~
              s(park, bs = "fs") +
                s(species, bs = "fs") +
              #Add a random effect for species
              ti(year, park, k = 12, bs = "fs") +
              ti(month, park, k = 8, bs = "fs"), #Adjust for a random effect of park, done
              family = "poisson",
              data = HWI_grouped_species, method = "REML")

summary(model1)
plot(model1, pages = 1)

# residuals of model 1
residuals(model1)

# add the residuals as a new column into the HWI_grouped_species dataframe ----
HWI_grouped_species$residuals <- residuals(model1)

# looking at the distribution of the residuals 
hist(HWI_grouped_species$residuals)

#look at the trend in Jasper ----
Jasper_trend <- HWI_grouped_species %>% 
  filter(park %in% c("Jasper"))

#plot the trend of residuals by year in Jasper ----
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Jasper_trend, aes(x = year_month, y = residuals, col = species)) +
  xlab("Date") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "none",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Look at the trend of residuals by month in Jasper ----
Jasper_jan_trend <- Jasper_trend %>% 
  filter(month %in% c("1"))
Jasper_feb_trend <- Jasper_trend %>% 
  filter(month %in% c("2"))
Jasper_mar_trend <- Jasper_trend %>% 
  filter(month %in% c("3"))
Jasper_apr_trend <- Jasper_trend %>% 
  filter(month %in% c("4"))
Jasper_may_trend <- Jasper_trend %>% 
  filter(month %in% c("5"))
Jasper_jun_trend <- Jasper_trend %>% 
  filter(month %in% c("6"))
Jasper_jul_trend <- Jasper_trend %>% 
  filter(month %in% c("7"))
Jasper_aug_trend <- Jasper_trend %>% 
  filter(month %in% c("8"))
Jasper_sep_trend <- Jasper_trend %>% 
  filter(month %in% c("9"))
Jasper_oct_trend <- Jasper_trend %>% 
  filter(month %in% c("10"))
Jasper_nov_trend <- Jasper_trend %>% 
  filter(month %in% c("11"))
Jasper_dec_trend <- Jasper_trend %>% 
  filter(month %in% c("12"))

# plot the monthly residual trend data in Jasper by year ----
ggplot() +
  geom_point(data = Jasper_jan_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_feb_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_mar_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_apr_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_may_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_jun_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_jul_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_aug_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_sep_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_oct_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_nov_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Jasper_dec_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#look at the trend in Banff ----
Banff_trend <- HWI_grouped_species %>% 
  filter(park %in% c("Banff"))

# Look at the trend of residuals by month in Banff ----
Banff_jan_trend <- Banff_trend %>% 
  filter(month %in% c("1"))
Banff_feb_trend <- Banff_trend %>% 
  filter(month %in% c("2"))
Banff_mar_trend <- Banff_trend %>% 
  filter(month %in% c("3"))
Banff_apr_trend <- Banff_trend %>% 
  filter(month %in% c("4"))
Banff_may_trend <- Banff_trend %>% 
  filter(month %in% c("5"))
Banff_jun_trend <- Banff_trend %>% 
  filter(month %in% c("6"))
Banff_jul_trend <- Banff_trend %>% 
  filter(month %in% c("7"))
Banff_aug_trend <- Banff_trend %>% 
  filter(month %in% c("8"))
Banff_sep_trend <- Banff_trend %>% 
  filter(month %in% c("9"))
Banff_oct_trend <- Banff_trend %>% 
  filter(month %in% c("10"))
Banff_nov_trend <- Banff_trend %>% 
  filter(month %in% c("11"))
Banff_dec_trend <- Banff_trend %>% 
  filter(month %in% c("12"))

# plot the monthly residual trend data in Banff by year ----
ggplot() +
  geom_point(data = Banff_jan_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_feb_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_mar_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_apr_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_may_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_jun_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_jul_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_aug_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_sep_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_oct_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_nov_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Banff_dec_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#plot the trend of residuals by year in Banff ----
ggplot() +
  geom_point(data = Banff_trend, aes(x = year_month, y = residuals, col = species)) +
  xlab("Date") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "none",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#look at the trend in Pacific Rim
Pacific_Rim_trend <- HWI_grouped_species %>% 
  filter(park %in% c("Pacific_Rim"))

# Look at the trend of residuals by month in Pacific Rim ----
Pacific_Rim_jan_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("1"))
Pacific_Rim_feb_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("2"))
Pacific_Rim_mar_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("3"))
Pacific_Rim_apr_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("4"))
Pacific_Rim_may_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("5"))
Pacific_Rim_jun_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("6"))
Pacific_Rim_jul_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("7"))
Pacific_Rim_aug_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("8"))
Pacific_Rim_sep_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("9"))
Pacific_Rim_oct_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("10"))
Pacific_Rim_nov_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("11"))
Pacific_Rim_dec_trend <- Pacific_Rim_trend %>% 
  filter(month %in% c("12"))

# plot the monthly residual trend data in Pacific Rim by year ----
ggplot() +
  geom_point(data = Pacific_Rim_jan_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_feb_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_mar_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_apr_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_may_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_jun_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_jul_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_aug_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_sep_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_oct_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_nov_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Pacific_Rim_dec_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#plot the trend of residuals by year in Pacific Rim (Vanouver Island) ----
ggplot() +
  geom_point(data = Pacific_Rim_trend, aes(x = year_month, y = residuals, col = species)) +
  xlab("Date") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "none",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#look at the trend in Waterton Lakes
Waterton_Lakes_trend <- HWI_grouped_species %>% 
  filter(park %in% c("Waterton_Lakes"))

# Look at the trend of residuals by month in Waterton Lakes ----
Waterton_Lakes_jan_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("1"))
Waterton_Lakes_feb_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("2"))
Waterton_Lakes_mar_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("3"))
Waterton_Lakes_apr_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("4"))
Waterton_Lakes_may_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("5"))
Waterton_Lakes_jun_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("6"))
Waterton_Lakes_jul_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("7"))
Waterton_Lakes_aug_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("8"))
Waterton_Lakes_sep_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("9"))
Waterton_Lakes_oct_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("10"))
Waterton_Lakes_nov_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("11"))
Waterton_Lakes_dec_trend <- Waterton_Lakes_trend %>% 
  filter(month %in% c("12"))

# plot the monthly residual trend data in Waterton Lakes by year ----
ggplot() +
  geom_point(data = Waterton_Lakes_jan_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_feb_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_mar_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_apr_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_may_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_jun_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_jul_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_aug_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_sep_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_oct_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_nov_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot() +
  geom_point(data = Waterton_Lakes_dec_trend, aes(x = year, y = residuals)) +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#plot the trend of residuals by year in Waterton Lakes ----
ggplot() +
  geom_point(data = Waterton_Lakes_trend, aes(x = year_month, y = residuals, col = species)) +
  xlab("Date") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "none",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



# Group species by higher taxonomic groups ---- #why aren't they working now???
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Grizzly Bear", "Black Bear", "Polar Bear" )] <- "Bear"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Domestic Cattle", "Domestic Cat", "Domestic Dog", "Domestic Horse", "Domestic Sheep")] <- "Domestic animal"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Mule Deer", "White-tailed Deer", "Black-tailed deer" )] <- "Deer"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Red Squirrel", "Beaver", "Columbian Ground Squirrel", "Hoary Marmot", "Woodchuck", "Woodrat", "Muskrat", "Richardson's Ground Squirrel", "Red-tailed Chipmunk", "Porcupine", "Golden-mantled Ground Squirrel", "Brown Rat", "Least Chipmunk", "Eastern Chipmunk", "Northern Flying Squirrel", "Black-tailed prairie dog", "Yellow-bellied Marmot")] <- "Rodent"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Massasauga", "Prairie Rattlesnake", "Smooth Greensnake", "Bullsnake", "Water Snake", "Plains Gartersnake", "Common Gartersnake", "Yellow-bellied Racer" )] <- "Snake"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Silver-haired Bat", "Big Brown Bat", "Little Brown Myotis" )] <- "Bat"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Tiger Salamander", "Long-toed Salamander" )] <- "Salamander"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Harbour seal", "California Sea Lion", "Northern Elephant Seal", "Steller Sea Lion" )] <- "Pinneped"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Marten", "Wolverine", "Badger", "River Otter", "Ermine", "Least Weasel" )] <- "Weasel"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Greater White-fronted Goose", "Canada Goose", "Trumpeter swan", "Ring-billed Gull", "Double-crested Cormorant", "Mallard", "Common Loon", "American Coot", "Common Merganser", "Red-necked Grebe", "Western Grebe")] <- "Waterfowl"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("American Tree Sparrow", "House Sparrow", "Barn Swallow", "Field Sparrow", "American Robin", "Golden-crowned Kinglet", "Common Redpoll", "Dark-eyed Junco" )] <- "Songbird"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Great Horned Owl", "Great Grey Owl", "Barred Owl", "Northern Pygmy-Owl", "Northern Saw-whet Owl", "Burrowing Owl" )] <- "Owl"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Semipalmated Plover", "Great Blue Heron", "Killdeer" )] <- "Shorebird"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Downy Woodpecker", "Pileated Woodpecker", "Hairy Woodpecker", "Lewis's Woodpecker" )] <- "Woodpecker"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Bald Eagle", "Osprey", "Golden Eagle", "Sharp-shinned Hawk", "Sharp-shinned Hawk" )] <- "Accipitridae"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Blue Grouse", "Ruffed Grouse", "Spruce Grouse", "Greater Sage-grouse", "Wild Turkey" )] <- "Phasianidae"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Rufous Hummingbird", "Ruby-throated Hummingbird" )] <- "Hummingbird"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Plains Bison", "Wood Bison" )] <- "Bison"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Raven", "Crow",  "Gray Jay",  "Steller's Jay", "Magpie" )] <- "Corvid"
HWI_grouped_species$species[HWI_grouped_species$species %in% c("Wasp", "Ant", "Earwigs" )] <- "Insect"

# importing shape files of Canadian national parks (visualisation only)----
CAshape <- vect("data/CLAB_CA_2023-09-08/CLAB_CA_2023-09-08.shp")
plot(CAshape)
# my HWI data only has 31 parks (e.g. no NU but they are in this shape file)

ABshape <- vect("data/CLAB_AB_2023-09-08/CLAB_AB_2023-09-08.shp")
plot(ABshape)

BCshape <- vect("data/CLAB_BC_2023-09-08/CLAB_BC_2023-09-08.shp")
plot(BCshape)

MBshape <- vect("data/CLAB_MB_2023-09-08/CLAB_MB_2023-09-08.shp")
plot(MBshape)

NBshape <- vect("data/CLAB_NB_2023-09-08/CLAB_NB_2023-09-08.shp")
plot(NBshape)

NLshape <- vect("data/CLAB_NL_2023-09-08/CLAB_NL_2023-09-08.shp")
plot(NLshape)

NSshape <- vect("data/CLAB_NS_2023-09-08/CLAB_NS_2023-09-08.shp")
plot(NSshape)

NTshape <- vect("data/CLAB_NT_2023-09-08/CLAB_NT_2023-09-08.shp")
plot(NTshape)

NUshape <- vect("data/CLAB_NU_2023-09-08/CLAB_NU_2023-09-08.shp")
plot(NUshape)

ONshape <- vect("data/CLAB_ON_2023-09-08/CLAB_ON_2023-09-08.shp")
plot(ONshape)

PEshape <- vect("data/CLAB_PE_2023-09-08/CLAB_PE_2023-09-08.shp")
plot(PEshape)

QCshape <- vect("data/CLAB_QC_2023-09-08/CLAB_QC_2023-09-08.shp")
plot(QCshape)

SKshape <- vect("data/CLAB_SK_2023-09-08/CLAB_SK_2023-09-08.shp")
plot(SKshape)

YTshape <- vect("data/CLAB_YT_2023-09-08/CLAB_YT_2023-09-08.shp")
plot(YTshape)

# importing polygons with sf ----

ABpolygon <- st_read("data/CLAB_AB_2023-09-08/CLAB_AB_2023-09-08.shp")
plot(ABpolygon)

BCpolygon <- st_read("data/CLAB_BC_2023-09-08/CLAB_BC_2023-09-08.shp")
plot(BCpolygon)

MBpolygon <- st_read("data/CLAB_MB_2023-09-08/CLAB_MB_2023-09-08.shp")
plot(MBpolygon)

NBpolygon <- st_read("data/CLAB_NB_2023-09-08/CLAB_NB_2023-09-08.shp")
plot(NBpolygon)

NLpolygon <- st_read("data/CLAB_NL_2023-09-08/CLAB_NL_2023-09-08.shp")
plot(NLpolygon)

NSpolygon <- st_read("data/CLAB_NS_2023-09-08/CLAB_NS_2023-09-08.shp")
plot(NSpolygon)

NTpolygon <- st_read("data/CLAB_NT_2023-09-08/CLAB_NT_2023-09-08.shp")
plot(NTpolygon)

NUpolygon <- st_read("data/CLAB_NU_2023-09-08/CLAB_NU_2023-09-08.shp")
plot(NUpolygon)

ONpolygon <- st_read("data/CLAB_ON_2023-09-08/CLAB_ON_2023-09-08.shp")
plot(ONpolygon)

PEpolygon <- st_read("data/CLAB_PE_2023-09-08/CLAB_PE_2023-09-08.shp")
plot(PEpolygon)

QCpolygon <- st_read("data/CLAB_QC_2023-09-08/CLAB_QC_2023-09-08.shp")
plot(QCpolygon)

SKpolygon <- st_read("data/CLAB_SK_2023-09-08/CLAB_SK_2023-09-08.shp")
plot(SKpolygon)

YTpolygon <- st_read("data/CLAB_YT_2023-09-08/CLAB_YT_2023-09-08.shp")
plot(YTpolygon)

# fitering for my 31 parks out of all the parks in each polygon ----

#AB

waterton_lakes <- ABpolygon[ABpolygon$CLAB_ID == "WATE", ]
plot(waterton_lakes)

elk_island <- ABpolygon[ABpolygon$CLAB_ID == "ELKI", ]
plot(elk_island)

jasper <- ABpolygon[ABpolygon$CLAB_ID == "JASP", ]
plot(jasper)

wood_buffalo <- ABpolygon[ABpolygon$CLAB_ID == "WOOD", ]
plot(wood_buffalo)

banff <- ABpolygon[ABpolygon$CLAB_ID == "BANF", ]
plot(banff)

# no polygon for grasslands

# BC

yoho <- BCpolygon[BCpolygon$CLAB_ID == "YOHO", ]
plot(yoho)

kootenay <- BCpolygon[BCpolygon$CLAB_ID == "KOOT", ]
plot(kootenay)

mount_revelstoke <- BCpolygon[BCpolygon$CLAB_ID == "REVE", ]
plot(mount_revelstoke)

pacific_rim <- BCpolygon[BCpolygon$CLAB_ID == "PRIM", ]
plot(pacific_rim)

glacier <- BCpolygon[BCpolygon$CLAB_ID == "GLAC", ]
plot(glacier)

# MB

wapusk <- MBpolygon[MBpolygon$CLAB_ID == "WAPU", ]
plot(wapusk)

# no polygon for prince of wales fort

# NB

fundy <- NBpolygon[NBpolygon$CLAB_ID == "FUND", ]
plot(fundy)

kouchibouguac <- NBpolygon[NBpolygon$CLAB_ID == "KOUC", ]
plot(kouchibouguac)

# NL

terra_nova <- NLpolygon[NLpolygon$CLAB_ID == "NOVA", ]
plot(terra_nova)

# NS

kejimkujik <- NSpolygon[NSpolygon$CLAB_ID == "KEJI", ]
plot(kejimkujik)

# no polygon on sable island

# NT

aulavik <- NTpolygon[NTpolygon$CLAB_ID == "AULA", ]
plot(aulavik)

nahanni <- NTpolygon[NTpolygon$CLAB_ID == "NAHA", ]
plot(nahanni)

# no polygon for grizzly bear

# no NU parks in my data

fathom_five <- ONpolygon[ONpolygon$CLAB_ID == "FIVE", ]
plot(fathom_five)

point_pelee <- ONpolygon[ONpolygon$CLAB_ID == "PELE", ]
plot(point_pelee)

georgian_bay_islands <- ONpolygon[ONpolygon$CLAB_ID == "GBIS", ]
plot(georgian_bay_islands)

thousand_islands <- ONpolygon[ONpolygon$CLAB_ID == "THIS", ]
plot(thousand_islands)

# no polygon for bruce peninsula

# PE

prince_edward_island <- PEpolygon[PEpolygon$CLAB_ID == "PEIS", ]
plot(prince_edward_island)

# QC

forillon <- QCpolygon[QCpolygon$CLAB_ID == "FORI", ]
plot(forillon)

# SK

prince_albert <- SKpolygon[SKpolygon$CLAB_ID == "PALB", ]
plot(prince_albert)

# YT

ivvavik <- YTpolygon[YTpolygon$CLAB_ID == "IVVA", ]
plot(ivvavik)

# need polygons for 5 more parks 

# merge all park polygons into one shape file ----
parks_polygon <- dplyr::bind_rows(list(waterton_lakes,elk_island,jasper, wood_buffalo, banff, yoho, kootenay, mount_revelstoke, pacific_rim, glacier, wapusk, fundy, kouchibouguac, terra_nova, kejimkujik, aulavik, nahanni, fathom_five, point_pelee, georgian_bay_islands, thousand_islands, prince_edward_island, forillon, prince_albert, ivvavik))
plot(parks_polygon)
plot(parks_polygon$geometry)
parks_geometry <- parks_polygon$geometry
plot(parks_geometry)
st_write(parks_polygon,"../../sf/parks_polygon.shp",driver = "ESRI Shapefile")

# shapefile(x = parks_geometry, file = "C:/Users/grace/Documents/GitHub/HWI_parks/file.shp")

# start looking at ndvi ----

library("xml2")
library("rvest")
library("dplyr")
library("terra")
library("raster")


# loop for extracting all links for 2010 [done] ----
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2010/"
pg <- read_html(url)
linkys <- html_attr(html_nodes(pg, "a"), "href")

LINKS <- list()
for(i in 1:length(linkys)){
  link <- paste(url, linkys[i], sep = "")
  LINKS[i] <- link
}

LINKS <- do.call(rbind, LINKS)

for(j in 6:length(linkys)){
    url_path <- paste(url, linkys[j], sep = "")
    path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/",linkys[j], sep="")
    try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
    
    Sys.sleep(5)
    
  }
  

#test the files to see if they can plot ndvi [done]
file1 <- "2010ndvi/2010_jan/AVHRR-Land_v005_AVH13C1_NOAA-19_20100101_c20170406091314.nc"
file2 <- "2010ndvi/2010_dec/AVHRR-Land_v005_AVH13C1_NOAA-19_20101231_c20170406211535.nc"
NDVI <- terra::rast(file1)
plot(NDVI$NDVI) # IT'S PLOTTING :DDDD



# rekha's code to rasterise the downloaded ndvi ----
# 2010ndvi----
# make a dataframe
jan_2010ndvi <- unique(list.files(path = 'C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jan/', # file for jan 
                             pattern = ".nc", full.names = T))

jan2010ndvi <- list()

for(i in 1:length(jan_2010ndvi)){ 
  r <- terra::rast(jan_2010ndvi[i])
  c <- crop(r, parks_polygon) # crop to my polygon --> it's cropping the entire canada??
  
  jan2010ndvi[[i]] <- c
  
} #now they're all added onto the list 

jan2010 <- rast(jan2010ndvi) # rasterise the list
trial <- stack(jan2010) # stack the rasters, each day has 3 layers
plot(trial$NDVI.1[1])
# drop the TIMEOFDAY and QA layers 
jan2010ndvi_only <- dropLayer(trial, c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27,29,30,32,33,35,36,38,39,41,42,44,45,47,48,50,51,53,54,56,57,59,60,62,63,65,66,68,69,71,72,74,75,77,78,80,81,83,84,86,87,89,90,92,93))
plot(jan2010ndvi_only)

# calculate mean of the month 
mean <- calc(jan2010ndvi_only, mean) 
plot(mean) 
plot(parks_polygon, add = T)



# how to crop it to my polygon??? ----

trialcrop <- crop(jan2010ndvi_only, parks_polygon) # no difference between plotting and not plotting? and what section is this?
plot(trialcrop)
# Plot full raster and polygon                       
plot(jan2010ndvi_only$NDVI.1)
plot(parks_geometry,add=T) # now they overlap but is it the right extent?

# Ryan's code to crop parks ----

library(terra)
library(sf)

# read parks
# parks = st_read(parks_polygon)

nc.dir = "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jan" #assign variable
setwd(nc.dir) #setwd, only way this is working
dat.dir = list.files(pattern="*.nc", all.files=TRUE, #list all files in there 
                     full.names=FALSE)  


jan = lapply(dat.dir, rast) #to files in the directory
jan = rast(jan) #read that as rasters + turn it into a stack 
names(jan) #turns jan into a list
jan <- jan[[1]] # extract that 1 band: NDVI 

st_crs(parks_polygon)
crs(jan)

# match extents
p.ext = ext(parks_polygon)
ext(jan) = p.ext

jan.crop = crop(jan, parks_polygon) #crop and mask similar, mask is cut pieces that fell outside
jan.mask = mask(jan, parks_polygon) # warning: [mask] CRS do not match 
plot(jan.mask)
jan.mean = app(jan.mask, mean)
hist(jan.mean) # warning:[hist] a sample of4% of the cells was used (of which 100% was NA) --> it's ok
project(jan.mean,"EPSG:4617")


st_crs(parks_polygon)
crs(jan)
writeRaster(jan.mean, "../jan_mean.tif") #../ moves it up one file path in






















#source(file = "gdal.R")
#newExtent <- extent(bbox(parks_geometry))

#library(mapview)
#mapview(sf::st_cast(parks_geometry, "MULTIPOLYGON"))





#ignore below ----

# corrupt <- sapply(feb_2010ndvi,
 #                 function(filename) {
 #                   .r <- tryCatch(rast(filename),
  #                                 error = function(e) return(as.character(e)))
   #                 return(is.character(.r))
    #              }) %>%
#  suppressWarnings()
#corrupt
#corrupt <- corrupt[which(corrupt)] # only keep TRUE values
#corrupt

#while(any(corrupt)) {
  # find file names
 # files <- substr(x = names(corrupt),
  #                start = nchar("C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jan/") + 1,
   #               stop = nchar(names(corrupt)))
  
 # years <- substr(files,
                 # start = nchar(files) - nchar('yyyymmdd_cyyyymmddhhmmss.nc') + 1,
                 # stop = nchar(files) - nchar('mmdd_cyyyymmddhhmmss.nc'))
  
  # re-download the corrupt NDVi rasters
#  urls <- paste0('https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2010/',
#                 files)
  
#  lapply(1:length(urls), function(.i){
#    path <- paste0("C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jan/", files[.i])
#    try(download.file(urls[.i], destfile = path))
#  })
  
  # check again what files are corrupt
#  corrupt <- sapply(names(corrupt),
#                    function(filename) {
#                      .r <- tryCatch(rast(filename),
#                                     error = function(e) return(as.character(e)))
#                      return(is.character(.r))
#                    }) %>%
#    suppressWarnings()
#  corrupt <- corrupt[which(corrupt)] # only keep TRUE values
#}

# b <- raster::brick(jan_2010ndvi) + 0
# r <- rast(b)

# sizes <- file.size(feb_2010ndvi) / 1e6
# hist(sizes, xlab = 'Approximate file size in MB')
# feb_2010ndvi[sizes < 50]
# plot(rast(feb_2010ndvi[sizes < 50][1]))


# trying to ope an nc file
# library(ncdf4)
# nc_data <- terra::rast('2010ndvi/2010_jan/AVHRR-Land_v005_AVH13C1_NOAA-19_20100110_c20170406112342.nc')
