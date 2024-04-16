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
library(xml2)
library(rvest)
library(raster)
library(tidyterra) #plot the map with geom_spatraster

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

# save HWI_parks ----
write.csv(HWI_parks, "C:/Users/grace/Documents/GitHub/HWI_parks/results/hwi_parks.csv", row.names=FALSE)
HWI_parks <- read.csv("results/hwi_parks.csv")

#Count number of incidents by park
incident_count <- HWI_parks %>% 
  count(HWI_parks$park)

# import Canada shape and extract boundaries only
canadashape <- st_as_sf(PROV) %>%  
  st_geometry()

# import coordinates of all national parks, coordinates obtained from Google Maps
park_coordinates <- read.csv("data/park_coordinates.csv")

# convert coordinates into spatial data
park_location <- SpatialPoints(park_coordinates[, c("longitude", "latitude")])

# plot parks
plot(canadashape)
sp::plot(park_location, add = TRUE, col = 'coral', pch = 19, cex = 0.5) 

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

# plot species sightings ----
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
                #Adjust for a random effect of park, done
                ti(month, park, k = 8, bs = "fs"), 
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


# Group species by higher taxonomic groups ---- 
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

#END OF EXPLORATION ----

# importing shape files of Canadian national parks (visualisation only)----
CAshape <- vect("data/CLAB_CA_2023-09-08/CLAB_CA_2023-09-08.shp")
plot(CAshape)

# importing polygons with sf ----

ABpolygon <- st_read("data/CLAB_AB_2023-09-08/CLAB_AB_2023-09-08.shp")
plot(ABpolygon)
saveRDS(ABpolygon,file ="rds/ABpolygon.rds")

BCpolygon <- st_read("data/CLAB_BC_2023-09-08/CLAB_BC_2023-09-08.shp")
plot(BCpolygon)
saveRDS(BCpolygon,file ="rds/BCpolygon.rds")

MBpolygon <- st_read("data/CLAB_MB_2023-09-08/CLAB_MB_2023-09-08.shp")
plot(MBpolygon)
saveRDS(MBpolygon,file ="rds/MBpolygon.rds")

NBpolygon <- st_read("data/CLAB_NB_2023-09-08/CLAB_NB_2023-09-08.shp")
plot(NBpolygon)
saveRDS(NBpolygon,file ="rds/NBpolygon.rds")

NLpolygon <- st_read("data/CLAB_NL_2023-09-08/CLAB_NL_2023-09-08.shp")
plot(NLpolygon)
saveRDS(NLpolygon,file ="rds/NLpolygon.rds")

NSpolygon <- st_read("data/CLAB_NS_2023-09-08/CLAB_NS_2023-09-08.shp")
plot(NSpolygon)
saveRDS(NSpolygon,file ="rds/NSpolygon.rds")

NTpolygon <- st_read("data/CLAB_NT_2023-09-08/CLAB_NT_2023-09-08.shp")
plot(NTpolygon)
saveRDS(NTpolygon,file ="rds/NTpolygon.rds")

NUpolygon <- st_read("data/CLAB_NU_2023-09-08/CLAB_NU_2023-09-08.shp")
plot(NUpolygon)
saveRDS(NUpolygon,file ="rds/NUpolygon.rds")

ONpolygon <- st_read("data/CLAB_ON_2023-09-08/CLAB_ON_2023-09-08.shp")
plot(ONpolygon)
saveRDS(ONpolygon,file ="rds/ONpolygon.rds")

PEpolygon <- st_read("data/CLAB_PE_2023-09-08/CLAB_PE_2023-09-08.shp")
plot(PEpolygon)
saveRDS(PEpolygon,file ="rds/PEpolygon.rds")

QCpolygon <- st_read("data/CLAB_QC_2023-09-08/CLAB_QC_2023-09-08.shp")
plot(QCpolygon)
saveRDS(QCpolygon,file ="rds/QCpolygon.rds")

SKpolygon <- st_read("data/CLAB_SK_2023-09-08/CLAB_SK_2023-09-08.shp")
plot(SKpolygon)
saveRDS(SKpolygon,file ="rds/SKpolygon.rds")

YTpolygon <- st_read("data/CLAB_YT_2023-09-08/CLAB_YT_2023-09-08.shp")
plot(YTpolygon)
saveRDS(YTpolygon,file ="rds/YTpolygon.rds")

# fitering for my 30 parks out of all the parks in each polygon ----

#AB

waterton_lakes <- ABpolygon[ABpolygon$CLAB_ID == "WATE", ]
plot(waterton_lakes)
saveRDS(waterton_lakes,file ="rds/waterton_lakes.rds")

elk_island <- ABpolygon[ABpolygon$CLAB_ID == "ELKI", ]
plot(elk_island)
saveRDS(elk_island,file ="rds/elk_island.rds")

jasper <- ABpolygon[ABpolygon$CLAB_ID == "JASP", ]
plot(jasper)
saveRDS(jasper,file ="rds/jasper.rds")

wood_buffalo <- ABpolygon[ABpolygon$CLAB_ID == "WOOD", ]
plot(wood_buffalo)
saveRDS(wood_buffalo,file ="rds/wood_buffalo.rds")

banff <- ABpolygon[ABpolygon$CLAB_ID == "BANF", ]
plot(banff)
saveRDS(banff,file ="rds/banff.rds")

# no polygon for grasslands

# BC

yoho <- BCpolygon[BCpolygon$CLAB_ID == "YOHO", ]
plot(yoho)
saveRDS(yoho,file ="rds/yoho.rds")

kootenay <- BCpolygon[BCpolygon$CLAB_ID == "KOOT", ]
plot(kootenay)
saveRDS(kootenay,file ="rds/kootenay.rds")

mount_revelstoke <- BCpolygon[BCpolygon$CLAB_ID == "REVE", ]
plot(mount_revelstoke)
saveRDS(mount_revelstoke,file ="rds/mount_revelstoke.rds")

pacific_rim <- BCpolygon[BCpolygon$CLAB_ID == "PRIM", ]
plot(pacific_rim)
saveRDS(pacific_rim,file ="rds/pacific_rim.rds")

glacier <- BCpolygon[BCpolygon$CLAB_ID == "GLAC", ]
plot(glacier)
saveRDS(glacier,file ="rds/glacier.rds")

# MB

wapusk <- MBpolygon[MBpolygon$CLAB_ID == "WAPU", ]
plot(wapusk)
saveRDS(wapusk,file ="rds/wapusk.rds")

# no polygon for prince of wales fort

# NB

fundy <- NBpolygon[NBpolygon$CLAB_ID == "FUND", ]
plot(fundy)
saveRDS(fundy,file ="rds/fundy.rds")

kouchibouguac <- NBpolygon[NBpolygon$CLAB_ID == "KOUC", ]
plot(kouchibouguac)
saveRDS(kouchibouguac,file ="rds/kouchibouguac.rds")

# NL

terra_nova <- NLpolygon[NLpolygon$CLAB_ID == "NOVA", ]
plot(terra_nova)
saveRDS(terra_nova,file ="rds/terra_nova.rds")

# NS

kejimkujik <- NSpolygon[NSpolygon$CLAB_ID == "KEJI", ]
plot(kejimkujik)
saveRDS(kejimkujik,file ="rds/kejimkijik.rds")

# no polygon on sable island

# NT

aulavik <- NTpolygon[NTpolygon$CLAB_ID == "AULA", ]
plot(aulavik)
saveRDS(aulavik,file ="rds/aulavik.rds")

nahanni <- NTpolygon[NTpolygon$CLAB_ID == "NAHA", ]
plot(nahanni)
saveRDS(nahanni,file ="rds/nahanni.rds")

# no polygon for grizzly bear

# no NU parks in my data

#ON

fathom_five <- ONpolygon[ONpolygon$CLAB_ID == "FIVE", ]
plot(fathom_five)
saveRDS(fathom_five,file ="rds/fathom_five.rds")

point_pelee <- ONpolygon[ONpolygon$CLAB_ID == "PELE", ]
plot(point_pelee)
saveRDS(point_pelee,file ="rds/point_pelee.rds")

georgian_bay_islands <- ONpolygon[ONpolygon$CLAB_ID == "GBIS", ]
plot(georgian_bay_islands)
saveRDS(georgian_bay_islands,file ="rds/georgian_bay_islands.rds")

thousand_islands <- ONpolygon[ONpolygon$CLAB_ID == "THIS", ]
plot(thousand_islands)
saveRDS(thousand_islands,file ="rds/thousand_islands.rds")

# no polygon for bruce peninsula

# PE

prince_edward_island <- PEpolygon[PEpolygon$CLAB_ID == "PEIS", ]
plot(prince_edward_island)
saveRDS(prince_edward_island,file ="rds/prince_edward_island.rds")

# QC

forillon <- QCpolygon[QCpolygon$CLAB_ID == "FORI", ]
plot(forillon)
saveRDS(forillon,file ="rds/forillon.rds")

# SK

prince_albert <- SKpolygon[SKpolygon$CLAB_ID == "PALB", ]
plot(prince_albert)
saveRDS(prince_albert,file ="rds/prince_albert.rds")

# YT

ivvavik <- YTpolygon[YTpolygon$CLAB_ID == "IVVA", ]
plot(ivvavik)
saveRDS(ivvavik,file ="rds/ivvavik.rds")

# END OF POLYGON ----

# need polygons for 5 more parks 

# # merge all park polygons into one shape file ----
# parks_polygon <- dplyr::bind_rows(list(waterton_lakes,elk_island,jasper, wood_buffalo, banff, yoho, kootenay, mount_revelstoke, pacific_rim, glacier, wapusk, fundy, kouchibouguac, terra_nova, kejimkujik, aulavik, nahanni, fathom_five, point_pelee, georgian_bay_islands, thousand_islands, prince_edward_island, forillon, prince_albert, ivvavik))
# plot(parks_polygon)
# plot(parks_polygon$geometry)
# parks_geometry <- parks_polygon$geometry
# plot(parks_geometry)
# st_write(parks_polygon,"../../sf/parks_polygon.shp",driver = "ESRI Shapefile")

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






# Ryan's code to crop parks (works) ---- 

library(terra)
library(sf)
library(raster)
library(xml2)

# read parks
# parks = st_read(parks_polygon)

#setwd("C:/Users/grace/Documents/GitHub/HWI_parks")
# parks = st_read('sf/parks_polygon.shp')
nc.dir = "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jan" #assign variable
setwd(nc.dir) #setwd, only way this is working
dat.dir = list.files(pattern="*.nc", all.files=TRUE, #list all files in there 
                     full.names=FALSE)  

# # trial to varname in selecting NDVI in 1 file
# library(ncdf4)
# nc.file <- nc_open("AVHRR-Land_v005_AVH13C1_NOAA-19_20100101_c20170406091314.nc")
# #names(nc.file$var) #check layers
# nc.file <- raster("AVHRR-Land_v005_AVH13C1_NOAA-19_20100101_c20170406091314.nc", varname = "NDVI")
# #plot(nc.file)


jan = lapply(dat.dir, raster) #to files in the directory, make jan files raster layers (varname "NDVI" was automatically chosen)
jan = stack(jan) #turn it into a raster stack 
jan = rast(jan) #turn them into spatrasters
#saveRDS(jan,file ="../../rds/jan2010raster.rds")
#jan <- readRDS("../../rds/jan2010raster.rds")
names(jan) #turns jan into a list --> only NDVI now :)
#plot(jan)

jasper_shape <- readRDS("../../rds/jasper.rds")


# jan.crop = crop(jan2010_reproject, jasper_shape, mask = TRUE) #my jasper shape is appearing but looks weird
#crop and mask similar, mask is cut pieces that fell outside
jan.crop = crop(jan, jasper_shape, mask = TRUE) # this one is only my jasper shape
# jan.mask = mask(jan2010_reproject, jasper_rast) 
# plot(jan.mask) #mask didn't work --> everything's gone when I mask them
plot(jan.crop) 
#saveRDS(jan.crop,file ="../../rds/jan2010crop.rds")
#jan2010crop <- readRDS("../../rds/jan2010crop.rds")


#jan_reproject <- spTransform(jan, crs(jasper)) X work
#jasper_shape <- readRDS("../../rds/jasper.rds")
#jasper <- crop(nc.file, jasper_shape)
#jasper_reproj <- terra::resample(jasper, "+proj=longlat +datum=WGS84 +no_defs")

# reproject 2010 jan into park projection ----
jan_reproject <- terra::project(jan.crop, crs(jasper_shape), method="near") # error:invalid latitude?
plot(jan_reproject) 
saveRDS(jan_reproject,file ="../../rds/jan2010reproject.rds")
jan2010_reproject <- readRDS("../../rds/jan2010reproject.rds")
plot(jan2010_reproject)

# taking mean for 2010 jan ----
jan.crop.mean = app(jan_reproject, mean, na.rm = TRUE) #get the mean for 2010 jan
names(jan.crop.mean) # turn it into a list
plot(jan.crop.mean) 
saveRDS(jan.crop.mean,file ="../../rds/jan2010mean.rds")
jan2010mean <- readRDS("../../rds/jan2010mean.rds")
plot(jan2010mean) #got the mean for 2010 jan --> change units for ndvi 

# rescale the NDVI values from [0, 2500] to [-1, 1]
mean_ndvi_jasper_scaled <- jan2010mean * 0.0001
plot(mean_ndvi_jasper_scaled)
saveRDS(mean_ndvi_jasper_scaled,file ="../../rds/jan2010mean_scaled.rds")
JASPjan2010mean_scaled <- readRDS("../../rds/jan2010mean_scaled.rds")
plot(JASPjan2010mean_scaled)

#................................................

# build for loops for all months in 2010 in jasper ----
# feb
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_feb" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the feb2010 directory
for (file_path in dat.dir) {
  feb <- lapply(dat.dir, raster)
  feb <- stack(feb)
  feb <- rast(feb)
  feb.crop <- crop(feb, jasper_shape, mask = TRUE)
  feb_reproject <- terra::project(feb.crop, crs(jasper_shape), method = "near")
  feb.crop.mean <- app(feb_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- feb.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010feb_scaled.rds")
  JASPfeb2010mean_scaled <- readRDS("../../rds/jasper_2010feb_scaled.rds")
  
}

# mar
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_mar" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the mar2010 directory
for (file_path in dat.dir) {
  mar <- lapply(dat.dir, raster)
  mar <- stack(mar)
  mar <- rast(mar)
  mar.crop <- crop(mar, jasper_shape, mask = TRUE)
  mar_reproject <- terra::project(mar.crop, crs(jasper_shape), method = "near")
  mar.crop.mean <- app(mar_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- mar.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010mar_scaled.rds")
  JASPmar2010mean_scaled <- readRDS("../../rds/jasper_2010mar_scaled.rds")
  plot(JASPmar2010mean_scaled)
}

# apr
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_apr" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the apr2010 directory
for (file_path in dat.dir) {
  apr <- lapply(dat.dir, raster)
  apr <- stack(apr)
  apr <- rast(apr)
  apr.crop <- crop(apr, jasper_shape, mask = TRUE)
  apr_reproject <- terra::project(apr.crop, crs(jasper_shape), method = "near")
  apr.crop.mean <- app(apr_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- apr.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010apr_scaled.rds")
  JASPapr2010mean_scaled <- readRDS("../../rds/jasper_2010apr_scaled.rds")
  plot(JASPapr2010mean_scaled)
}

# may
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_may" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the may2010 directory
for (file_path in dat.dir) {
  may <- lapply(dat.dir, raster)
  may <- stack(may)
  may <- rast(may)
  may.crop <- crop(may, jasper_shape, mask = TRUE)
  may_reproject <- terra::project(may.crop, crs(jasper_shape), method = "near")
  may.crop.mean <- app(may_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- feb.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010may_scaled.rds")
  JASPmay2010mean_scaled <- readRDS("../../rds/jasper_2010may_scaled.rds")
  plot(JASPmay2010mean_scaled)
}

# jun
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jun" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the jun2010 directory
for (file_path in dat.dir) {
  jun <- lapply(dat.dir, raster)
  jun <- stack(jun)
  jun <- rast(jun)
  jun.crop <- crop(jun, jasper_shape, mask = TRUE)
  jun_reproject <- terra::project(jun.crop, crs(jasper_shape), method = "near")
  jun.crop.mean <- app(jun_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- jun.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010jun_scaled.rds")
  JASPjun2010mean_scaled <- readRDS("../../rds/jasper_2010jun_scaled.rds")
  plot(JASPjun2010mean_scaled)
}

# jul
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jul" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the jul2010 directory
for (file_path in dat.dir) {
  jul <- lapply(dat.dir, raster)
  jul <- stack(jul)
  jul <- rast(jul)
  jul.crop <- crop(jul, jasper_shape, mask = TRUE)
  jul_reproject <- terra::project(jul.crop, crs(jasper_shape), method = "near")
  jul.crop.mean <- app(jul_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- jul.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010jul_scaled.rds")
  JASPjul2010mean_scaled <- readRDS("../../rds/jasper_2010jul_scaled.rds")
  plot(JASPjul2010mean_scaled)
}

# aug
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_aug" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the aug2010 directory
for (file_path in dat.dir) {
  aug <- lapply(dat.dir, raster)
  aug <- stack(aug)
  aug <- rast(aug)
  aug.crop <- crop(aug, jasper_shape, mask = TRUE)
  aug_reproject <- terra::project(aug.crop, crs(jasper_shape), method = "near")
  aug.crop.mean <- app(aug_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- aug.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010aug_scaled.rds")
  JASPaug2010mean_scaled <- readRDS("../../rds/jasper_2010aug_scaled.rds")
  plot(JASPaug2010mean_scaled)
}

# sep
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_sep" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the sep2010 directory
for (file_path in dat.dir) {
  sep <- lapply(dat.dir, raster)
  sep <- stack(sep)
  sep <- rast(sep)
  sep.crop <- crop(sep, jasper_shape, mask = TRUE)
  sep_reproject <- terra::project(sep.crop, crs(jasper_shape), method = "near")
  sep.crop.mean <- app(sep_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- sep.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010sep_scaled.rds")
  JASPsep2010mean_scaled <- readRDS("../../rds/jasper_2010sep_scaled.rds")
  plot(JASPsep2010mean_scaled)
}

# oct
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_oct" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the oct2010 directory
for (file_path in dat.dir) {
  oct <- lapply(dat.dir, raster)
  oct <- stack(oct)
  oct <- rast(oct)
  oct.crop <- crop(oct, jasper_shape, mask = TRUE)
  oct_reproject <- terra::project(oct.crop, crs(jasper_shape), method = "near")
  oct.crop.mean <- app(oct_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- oct.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010oct_scaled.rds")
  JASPoct2010mean_scaled <- readRDS("../../rds/jasper_2010oct_scaled.rds")
  plot(JASPoct2010mean_scaled)
}

# nov
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_nov" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the nov2010 directory
for (file_path in dat.dir) {
  nov <- lapply(dat.dir, raster)
  nov <- stack(nov)
  nov <- rast(nov)
  nov.crop <- crop(nov, jasper_shape, mask = TRUE)
  nov_reproject <- terra::project(nov.crop, crs(jasper_shape), method = "near")
  nov.crop.mean <- app(nov_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- nov.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010nov_scaled.rds")
  JASPnov2010mean_scaled <- readRDS("../../rds/jasper_2010nov_scaled.rds")
  plot(JASPnov2010mean_scaled)
}

# dec
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_dec" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the dec2010 directory
for (file_path in dat.dir) {
  dec <- lapply(dat.dir, raster)
  dec <- stack(dec)
  dec <- rast(dec)
  dec.crop <- crop(dec, jasper_shape, mask = TRUE)
  dec_reproject <- terra::project(dec.crop, crs(jasper_shape), method = "near")
  dec.crop.mean <- app(dec_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- dec.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2010dec_scaled.rds")
  JASPdec2010mean_scaled <- readRDS("../../rds/jasper_2010dec_scaled.rds")
  plot(JASPdec2010mean_scaled)
}

#take a look at all the 2010 jasper mean plots
par(mfrow = c(3, 4))
plot(JASPjan2010mean_scaled)
plot(JASPfeb2010mean_scaled)
plot(JASPmar2010mean_scaled)
plot(JASPapr2010mean_scaled)
plot(JASPmay2010mean_scaled)
plot(JASPjun2010mean_scaled)
plot(JASPjul2010mean_scaled)
plot(JASPaug2010mean_scaled)
plot(JASPsep2010mean_scaled)
plot(JASPoct2010mean_scaled)
plot(JASPnov2010mean_scaled)
plot(JASPdec2010mean_scaled)









# next step: extract each value to parks? and do the same for the other 11 months 

# download 2011-2021 data ----
# loop for extracting all links for 2011 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2011/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2012 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2012/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2012ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2013 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2013/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2013ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2014 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2014/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2014ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2015 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2015/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2015ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2016 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2016/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2016ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2017 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2017/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2017ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2018 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2018/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2018ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2019 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2019/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2019ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2020 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2020/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2020ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2021 ---- DONE
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2021/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2021ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}


# loop for extracting all links for 2022 ---- 
url <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2022/"
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_parks/2022ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

#......................................................................
#2011 jasper ----
# build for loops for all months in 2010 in jasper ----
# jan
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_jan" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the feb2011 directory
for (file_path in dat.dir) {
  jan <- lapply(dat.dir, raster)
  jan <- stack(jan)
  jan <- rast(jan)
  jan.crop <- cropjan_reproject <- terra::project(jan.crop, crs(jasper_shape), method = "near")(jan, jasper_shape, mask = TRUE)
  
  jan.crop.mean <- app(jan_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- jan.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011jan_scaled.rds")
  JASPjan2011mean_scaled <- readRDS("../../rds/jasper_2011jan_scaled.rds")
  plot(JASPjan2011mean_scaled)
  
}


# feb
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_feb" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the feb2011 directory
for (file_path in dat.dir) {
  feb <- lapply(dat.dir, raster)
  feb <- stack(feb)
  feb <- rast(feb)
  feb.crop <- crop(feb, jasper_shape, mask = TRUE)
  feb_reproject <- terra::project(feb.crop, crs(jasper_shape), method = "near")
  feb.crop.mean <- app(feb_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- feb.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011feb_scaled.rds")
  JASPfeb2011mean_scaled <- readRDS("../../rds/jasper_2011feb_scaled.rds")
  plot(JASPfeb2011mean_scaled)
  
}

# mar
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_mar" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the mar2010 directory
for (file_path in dat.dir) {
  mar <- lapply(dat.dir, raster)
  mar <- stack(mar)
  mar <- rast(mar)
  mar.crop <- crop(mar, jasper_shape, mask = TRUE)
  mar_reproject <- terra::project(mar.crop, crs(jasper_shape), method = "near")
  mar.crop.mean <- app(mar_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- mar.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011mar_scaled.rds")
  JASPmar2011mean_scaled <- readRDS("../../rds/jasper_2011mar_scaled.rds")
  plot(JASPmar2011mean_scaled)
}

# apr
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_apr" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the apr2011 directory
for (file_path in dat.dir) {
  apr <- lapply(dat.dir, raster)
  apr <- stack(apr)
  apr <- rast(apr)
  apr.crop <- crop(apr, jasper_shape, mask = TRUE)
  apr_reproject <- terra::project(apr.crop, crs(jasper_shape), method = "near")
  apr.crop.mean <- app(apr_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- apr.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011apr_scaled.rds")
  JASPapr2011mean_scaled <- readRDS("../../rds/jasper_2011apr_scaled.rds")
  plot(JASPapr2011mean_scaled)
}

# may
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_may" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the may2011 directory
for (file_path in dat.dir) {
  may <- lapply(dat.dir, raster)
  may <- stack(may)
  may <- rast(may)
  may.crop <- crop(may, jasper_shape, mask = TRUE)
  may_reproject <- terra::project(may.crop, crs(jasper_shape), method = "near")
  may.crop.mean <- app(may_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- feb.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011may_scaled.rds")
  JASPmay2011mean_scaled <- readRDS("../../rds/jasper_2011may_scaled.rds")
  plot(JASPmay2011mean_scaled)
}

# jun
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_jun" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the jun2011 directory
for (file_path in dat.dir) {
  jun <- lapply(dat.dir, raster)
  jun <- stack(jun)
  jun <- rast(jun)
  jun.crop <- crop(jun, jasper_shape, mask = TRUE)
  jun_reproject <- terra::project(jun.crop, crs(jasper_shape), method = "near")
  jun.crop.mean <- app(jun_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- jun.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011jun_scaled.rds")
  JASPjun2011mean_scaled <- readRDS("../../rds/jasper_2011jun_scaled.rds")
  plot(JASPjun2011mean_scaled)
}

# jul
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_jul" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the jul2011 directory
for (file_path in dat.dir) {
  jul <- lapply(dat.dir, raster)
  jul <- stack(jul)
  jul <- rast(jul)
  jul.crop <- crop(jul, jasper_shape, mask = TRUE)
  jul_reproject <- terra::project(jul.crop, crs(jasper_shape), method = "near")
  jul.crop.mean <- app(jul_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- jul.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011jul_scaled.rds")
  JASPjul2011mean_scaled <- readRDS("../../rds/jasper_2011jul_scaled.rds")
  plot(JASPjul2011mean_scaled)
}

# aug
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_aug" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the aug2011 directory
for (file_path in dat.dir) {
  aug <- lapply(dat.dir, raster)
  aug <- stack(aug)
  aug <- rast(aug)
  aug.crop <- crop(aug, jasper_shape, mask = TRUE)
  aug_reproject <- terra::project(aug.crop, crs(jasper_shape), method = "near")
  aug.crop.mean <- app(aug_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- aug.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011aug_scaled.rds")
  JASPaug2010mean_scaled <- readRDS("../../rds/jasper_2011aug_scaled.rds")
  plot(JASPaug2011mean_scaled)
}

# sep
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_sep" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the sep2011 directory
for (file_path in dat.dir) {
  sep <- lapply(dat.dir, raster)
  sep <- stack(sep)
  sep <- rast(sep)
  sep.crop <- crop(sep, jasper_shape, mask = TRUE)
  sep_reproject <- terra::project(sep.crop, crs(jasper_shape), method = "near")
  sep.crop.mean <- app(sep_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- sep.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011sep_scaled.rds")
  JASPsep2011mean_scaled <- readRDS("../../rds/jasper_2011sep_scaled.rds")
  plot(JASPsep2011mean_scaled)
}

# oct
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_oct" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the oct2011 directory
for (file_path in dat.dir) {
  oct <- lapply(dat.dir, raster)
  oct <- stack(oct)
  oct <- rast(oct)
  oct.crop <- crop(oct, jasper_shape, mask = TRUE)
  oct_reproject <- terra::project(oct.crop, crs(jasper_shape), method = "near")
  oct.crop.mean <- app(oct_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- oct.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011oct_scaled.rds")
  JASPoct2011mean_scaled <- readRDS("../../rds/jasper_2011oct_scaled.rds")
  plot(JASPoct2011mean_scaled)
}

# nov
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_nov" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the nov2011 directory
for (file_path in dat.dir) {
  nov <- lapply(dat.dir, raster)
  nov <- stack(nov)
  nov <- rast(nov)
  nov.crop <- crop(nov, jasper_shape, mask = TRUE)
  nov_reproject <- terra::project(nov.crop, crs(jasper_shape), method = "near")
  nov.crop.mean <- app(nov_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- nov.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011nov_scaled.rds")
  JASPnov2011mean_scaled <- readRDS("../../rds/jasper_2011nov_scaled.rds")
  plot(JASPnov2011mean_scaled)
}

# dec
# Set the directory containing the nc files
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011_dec" 
setwd(nc.dir)

# List all files in the directory with the specified pattern
dat.dir <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# for loop for all the files in the dec2011 directory
for (file_path in dat.dir) {
  dec <- lapply(dat.dir, raster)
  dec <- stack(dec)
  dec <- rast(dec)
  dec.crop <- crop(dec, jasper_shape, mask = TRUE)
  dec_reproject <- terra::project(dec.crop, crs(jasper_shape), method = "near")
  dec.crop.mean <- app(dec_reproject, mean, na.rm = TRUE)
  mean_ndvi_scaled <- dec.crop.mean * 0.0001
  saveRDS(mean_ndvi_scaled, file = "../../rds/jasper_2011dec_scaled.rds")
  JASPdec2011mean_scaled <- readRDS("../../rds/jasper_2011dec_scaled.rds")
  plot(JASPdec2011mean_scaled)
}

#.............................................................................
# for loop trial ----

# import all Canada parks 
#CAshape <- st_read("data/CLAB_CA_2023-09-08/CLAB_CA_2023-09-08.shp")

# subsetting the 25 parks I need 
#test_parks <- CAshape$CLAB_ID %in% c("WATE", "ELKI", "JASP", "WOOD", "BANF", "YOHO", "KOOT", "REVE", "PRIM", "GLAC", "WAPU", "FUND", "KOUC", "NOVA", "KEJI", "AULA", "NAHA", "FIVE", "PELE", "GBIS", "THIS", "PEIS", "FORI", "PALB", "IVVA")

# Subset the spatial dataset based on the condition
#my_parks <- CAshape[test_parks, ]

#test <- "C:/Users/grace/Documents/GitHub/HWI_parks/ndvi/2011ndvi/2011_aug/AVHRR-Land_v005_AVH13C1_NOAA-19_20110802_c20170407082138.nc"


# Set directory to folder containing all ndvi files (2010-2021) 
# loop over all years in the directory 

nc.year_dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/ndvi"
setwd(nc.year_dir)

# import all of the boundaries for Canadian parks
CAshape <- st_read("C:/Users/grace/Documents/GitHub/HWI_parks/data/CLAB_CA_2023-09-08")

# Create a list of the names of the 25 parks to be studied 
test_parks <- c("WATE", "ELKI", "JASP", "WOOD",
                "BANF", "YOHO", "KOOT", "REVE",
                "PRIM", "GLAC", "WAPU", "FUND",
                "KOUC", "NOVA", "KEJI", "AULA",
                "NAHA", "FIVE", "PELE", "GBIS",
                "THIS", "PEIS", "FORI", "PALB", "IVVA")


# import JASP shapefile
jasper_shape <- readRDS("../rds/jasper.rds")

# Define the CRS
CRS_canada <- crs(jasper_shape)

# List all year folders in the ndvi directory 
year_folders <- list.dirs(path = nc.year_dir, full.names = TRUE, recursive = FALSE)

RESULTS <- list()

# Loop through each year folder
for (i in 1:length(year_folders)) { #length(year_folders)
  
  year <- gsub(pattern = "ndvi",
               replacement = "",
               x = gsub(pattern = "C:/Users/grace/Documents/GitHub/HWI_parks/ndvi/",
                        replacement = "",
                        x = year_folders[i]))
  
  # List all month folders in the year folder
  month_folders <- list.dirs(path = year_folders[i],
                             full.names = TRUE,
                             recursive = FALSE)
  
  # Generate an empty list for storing daily results
  res_month <- list()
  
  # Loop through each month folder
  for (j in 1:length(month_folders)) {
    
    # Make a list of the files in the month directory
    nc.files <- list.files(path = month_folders[j],
                           pattern = "*.nc",
                           full.names = TRUE)
    
    # Generate an empty list for storing daily results
    res_day <- list()
    
    # Loop through all the ndvi files for the current month
    for (k in 1:length(nc.files)) {
      
      
      # make the spatrasters
      spat <- rast(nc.files[k]) 
      spat <- spat[[which(names(spat) == "NDVI")]]
      
      # Reproject the raster to the CRS of jasper_shape
      reprojected_spat <- terra::project(spat,
                                         CRS_canada,
                                         method = "near")
      
      # Generate an empty list for storing results
      res <- list()
      
      #Loop over the vector of park names to extract the NDVI information
      for(l in 1:length(test_parks)){
        
        #Extract the desired park contour
        PARK <- CAshape[CAshape$CLAB_ID %in% test_parks[l],]
        
        # crop the NDVI raster to the park
        cropped_spat <- crop(reprojected_spat, PARK, mask = TRUE) 
        
        # Get mean and variance in NDVI
        NDVI <- mean(values(cropped_spat), na.rm = TRUE)
        #NDVI_var <- var(cropped_spat)
        NDVI_var <- var(values(cropped_spat), na.rm = TRUE)
        
        # Store as a data frame in the list
        res[[l]] <- data.frame(park = test_parks[l],
                               date = paste(year,j,k,sep = "_"),
                               ndvi = NDVI,
                               ndvi_var = NDVI_var)
        
      } #close the loop over parks
      
      #clean up the results over the days of the month
      res_day[[k]] <- do.call(rbind,res) 
      
      
    } #close of day loop
    
    #clean up the results over the months of the year
    res_month[[j]] <- do.call(rbind,res_day) 
    
  } # close of the month loop
  
  #clean up the results over the months of the year
  RESULTS[[i]] <- do.call(rbind,res_month) 
} # close of year loop

# convert the final list to a data frame (daily ndvi mean and var)
RESULTS <- do.call(rbind,RESULTS) 

#SAVE AS RDA OR CSV
#save(RESULTS, file = "test.rda")
write.csv(RESULTS, "C:/Users/grace/Documents/GitHub/HWI_parks/results/parksndvi.csv", row.names=FALSE)
RESULTS_df <- read.csv("results/parksndvi.csv")

# close loop of all the years 

#file <- raster("../cropped_2011_ndvi/2011_jan/AVHRR-Land_v005_AVH13C1_NOAA-19_20110103_c20170407000513.tif")
#plot(file) # testing by rasterising and plot

# 2000-2009 NDVI ----
# in another script: 2000-2009_parks_ndvi in HWI 


# ................................................................
# results dataframe (2010-2021)---- 

#Create data frame by grouping park means according to months and years


#convert to calendar dates
RESULTS_df$date <- as.Date(RESULTS_df$date, format = "%Y_%m_%d")

#extract year
RESULTS_df$year <- lubridate::year(RESULTS_df$date)

#extract month
RESULTS_df$month <- lubridate::month(RESULTS_df$date)

#rename columns
names(RESULTS_df)[3] <- "ndvi_daily_mean"
names(RESULTS_df)[4] <- "ndvi_daily_variance"

# Create a new dataframe for analysis for monthly ndvi mean to be grouped by park and year
data_ndvi_mean <- aggregate(ndvi_daily_mean ~ month + year + park, data = RESULTS_df, FUN = mean, na.rm = TRUE)

#rename columns
names(data_ndvi_mean)[4] <- "ndvi_monthly_mean"

#save datafram as a csv
write.csv(data_ndvi_mean, "C:/Users/grace/Documents/GitHub/HWI_parks/results/monthly_mean_ndvi.csv", row.names=FALSE)
mean_ndvi_df <- read.csv("results/monthly_mean_ndvi.csv")

# Create a new dataframe for analysis for monthly ndvi variance to be grouped by park and year
# data_ndvi_var <- aggregate(ndvi_daily_variance ~ month + year + park, data = RESULTS_df, FUN = mean, na.rm = TRUE)
# 
# #rename columns
# names(data_ndvi_var)[4] <- "ndvi_monthly_var"

# did the same for 2000-2009 NDVI data (separate script - 2000-200_parks_ndvi) ----


# NDVI & parks GAM! ----

# rescale ndvi in dataframe
mean_ndvi_df <- mean_ndvi_df %>% 
  mutate((ndvi_monthly_mean+1)/2)

# rename columns 
names(mean_ndvi_df)[5] <- "scaled_mean_ndvi"

# change month and year to numeric
as.numeric(mean_ndvi_df$month)
as.numeric(mean_ndvi_df$year)

# GAM
test <-
  gam(
    scaled_mean_ndvi ~ #scale ndvi from 0 to 1 to fit beta distribution
      # fixed effects
      park +
      # global smooths
      s(month, bs = "cc", k = 4) + #month effect
      s(year, k = 8) + #year effect
      ti(month, year, k = 6), #month/ year interaction
    family = "betar",
    #beta location scale distribution for the data
    data = mean_ndvi_df,
    method = 'fREML'
  )


summary(test)
plot(test, pages = 1)

# residuals of model 1
residuals(test)

# add the residuals as a new column into the HWI_grouped_species dataframe ----
mean_ndvi_df$residuals <- residuals(test)

# looking at the distribution of the residuals 
hist(mean_ndvi_df$residuals)

# did the same thing for all years in 2000-2021 on separate script ----
# END OF 2010-2021 NDVI ----

# matching park IDs ----
HWI_parks$park[HWI_parks$park == "Banff"]<- "BANF"
HWI_parks$park[HWI_parks$park == "Pacific_Rim"]<- "PRIM"
HWI_parks$park[HWI_parks$park == "Waterton_Lakes"]<- "WATE"
HWI_parks$park[HWI_parks$park == "Kejimkujik"]<- "KEJI"
HWI_parks$park[HWI_parks$park == "Jasper"]<- "JASP"
HWI_parks$park[HWI_parks$park == "Forillon"]<- "FORI"
HWI_parks$park[HWI_parks$park == "Prince_Albert"]<- "PALB"
HWI_parks$park[HWI_parks$park == "Kootenay"]<- "KOOT"
HWI_parks$park[HWI_parks$park == "Glacier"]<- "GLAC"
HWI_parks$park[HWI_parks$park == "Wapusk"]<- "WAPU"
#HWI_parks$park[HWI_parks$park == "Grasslands"]<- "Grasslands"
#HWI_parks$park[HWI_parks$park == "Bruce_Peninsula"]<- "Bruce_Peninsula"
HWI_parks$park[HWI_parks$park == "Yoho"]<- "YOHO"
HWI_parks$park[HWI_parks$park == "Terra_Nova"]<- "NOVA"
HWI_parks$park[HWI_parks$park == "Mount_Revelstoke"]<- "REVE"
HWI_parks$park[HWI_parks$park == "Elk_Island"]<- "ELKI"
HWI_parks$park[HWI_parks$park == "Georgian_Bay_Islands"]<- "GBIS"
#HWI_parks$park[HWI_parks$park == "Prince_of_Wales_Fort"]<- "Prince_of_Wales_Fort"
HWI_parks$park[HWI_parks$park == "Point_Pelee"]<- "PELE"
HWI_parks$park[HWI_parks$park == "Thousand_Islands"]<- "THIS"
HWI_parks$park[HWI_parks$park == "Wood_Buffalo"]<- "WOOD"
HWI_parks$park[HWI_parks$park == "Prince_Edward_Island"]<- "PEIS"
HWI_parks$park[HWI_parks$park == "Ivvavik"]<- "IVVA"
HWI_parks$park[HWI_parks$park == "Kouchibouguac"]<- "KOUC"
#HWI_parks$park[HWI_parks$park == "Grizzly_Bear_Mountain"]<- "Grizzly_Bear_Mountain"
HWI_parks$park[HWI_parks$park == "Fundy"]<- "FUND"
HWI_parks$park[HWI_parks$park == "Nahanni"]<- "NAHA"
HWI_parks$park[HWI_parks$park == "Aulavik"]<- "AULA"
#HWI_parks$park[HWI_parks$park == "Sable_Island"]<- "Sable_Island"
HWI_parks$park[HWI_parks$park == "Fathom_Five"]<- "FIVE"
#HWI_parks$park[HWI_parks$park == "Fort_Walsh"]<- "Fort_Walsh"

# drop parks without polygons ----
HWI_dropped <- subset(HWI_parks, park %in% c("WATE", "ELKI", "JASP", "WOOD",
                                             "BANF", "YOHO", "KOOT", "REVE",
                                             "PRIM", "GLAC", "WAPU", "FUND",
                                             "KOUC", "NOVA", "KEJI", "AULA",
                                             "NAHA", "FIVE", "PELE", "GBIS",
                                             "THIS", "PEIS", "FORI", "PALB", "IVVA"))

# merging HWI and NDVI 2000-2021 dataframes ----
HWI_NDVI <- merge(all_ndvi, HWI_dropped, by = c("park", "month", "year"))

# new data frame with aggregate by HWI number 
hwi_ndvi <- aggregate(HWI ~ month + year + park + ndvi_monthly_mean + scaled_mean_ndvi + residuals, data = HWI_NDVI, FUN = "length")

# creating a year_month column
hwi_ndvi$year_month <- paste(hwi_ndvi$year, hwi_ndvi$month, sep = "-")

# save the data frame
saveRDS(hwi_ndvi,file ="rds/hwi_ndvi.rds")

#plot the trend of residuals by year_month ----
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = hwi_ndvi, aes(x = year_month, y = residuals, col = park)) +
  xlab("Year_Month") +
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


# plot HWI with NDVI residuals ----
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = hwi_ndvi, aes(x = residuals, y = HWI, col = park)) +
  #geom_smooth(data = hwi_ndvi, aes(x = residuals, y = HWI, col = park),method = "lm") +
  xlab("Residuals") +
  ylab("HWI") +
  scale_y_log10() +
  scale_colour_manual(name="Region",
                      values = manual_colors) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.3) +
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

# library(lme4)
# library(glmmTMB)
# 
# # model for HWI as a function of residuals ----
# fit <- glmmTMB(HWI + 1 ~ residuals + (1|park), family = Gamma(link = "log"), data = hwi_ndvi)
# 

#ENF OF ALL NDVI ----

# new map ----

# import coordinates of all national parks, coordinates obtained from Google Maps
park_coordinates <- read.csv("data/park_coordinates.csv")

# remove the dropped parks x5
parks_to_drop <- c("Grasslands National Park of Canada", "Bruce Peninsula National Park of Canada", "Prince of Wales Fort National Historic Site of Canada", "Saoy\\xfa-?ehdacho National Historic Site of Canada", "Sable Island National Park Reserve", "Fort Walsh National Historic Site of Canada")

new_park_coordinates <- subset(park_coordinates, !(park %in% parks_to_drop))

# match coordinates name to park ID
new_park_coordinates$park[new_park_coordinates$park == "Banff National Park of Canada"]<- "BANF"
new_park_coordinates$park[new_park_coordinates$park == "Pacific Rim National Park Reserve of Canada"]<- "PRIM"
new_park_coordinates$park[new_park_coordinates$park == "Waterton Lakes National Park of Canada"]<- "WATE"
new_park_coordinates$park[new_park_coordinates$park == "Kejimkujik National Park and National Historic Site of Canada"]<- "KEJI"
new_park_coordinates$park[new_park_coordinates$park == "Jasper National Park of Canada"]<- "JASP"
new_park_coordinates$park[new_park_coordinates$park == "Forillon National Park of Canada"]<- "FORI"
new_park_coordinates$park[new_park_coordinates$park == "Prince Albert National Park of Canada"]<- "PALB"
new_park_coordinates$park[new_park_coordinates$park == "Kootenay National Park of Canada"]<- "KOOT"
new_park_coordinates$park[new_park_coordinates$park == "Glacier National Park of Canada"]<- "GLAC"
new_park_coordinates$park[new_park_coordinates$park == "Wapusk National Park of Canada"]<- "WAPU"
new_park_coordinates$park[new_park_coordinates$park == "Yoho National Park of Canada"]<- "YOHO"
new_park_coordinates$park[new_park_coordinates$park == "Terra Nova National Park of Canada"]<- "NOVA"
new_park_coordinates$park[new_park_coordinates$park == "Mount Revelstoke National Park of Canada"]<- "REVE"
new_park_coordinates$park[new_park_coordinates$park == "Elk Island National Park of Canada"]<- "ELKI"
new_park_coordinates$park[new_park_coordinates$park == "Georgian Bay Islands National Park of Canada"]<- "GBIS"
new_park_coordinates$park[new_park_coordinates$park == "Point Pelee National Park of Canada"]<- "PELE"
new_park_coordinates$park[new_park_coordinates$park == "Thousand Islands National Park of Canada"]<- "THIS"
new_park_coordinates$park[new_park_coordinates$park == "Wood Buffalo National Park of Canada"]<- "WOOD"
new_park_coordinates$park[new_park_coordinates$park == "Prince Edward Island National Park of Canada"]<- "PEIS"
new_park_coordinates$park[new_park_coordinates$park == "Ivvavik National Park of Canada"]<- "IVVA"
new_park_coordinates$park[new_park_coordinates$park == "Kouchibouguac National Park of Canada"]<- "KOUC"
new_park_coordinates$park[new_park_coordinates$park == "Fundy National Park of Canada"]<- "FUND"
new_park_coordinates$park[new_park_coordinates$park == "Nahanni National Park Reserve of Canada"]<- "NAHA"
new_park_coordinates$park[new_park_coordinates$park == "Aulavik National Park of Canada"]<- "AULA"
new_park_coordinates$park[new_park_coordinates$park == "Fathom Five National Marine Park of Canada"]<- "FIVE"


# convert coordinates into spatial data
#park_location <- SpatialPoints(select(park_coordinates, longitude, latitude))
new_park_location <- SpatialPoints(new_park_coordinates[, c("longitude", "latitude")])

parks_sf <- st_as_sf(new_park_coordinates, coords = c("longitude", "latitude"), crs = 4326)

#define the crs
esri_102001 <- st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# reproject parks coordinate
parks_esri <- st_transform(parks_sf, crs = esri_102001)

#convert back to dataframe 
parks_esri_df <- st_drop_geometry


coordinates <- st_coordinates(parks_esri)

# combine reprojected esri coordinates into original coordinates df
park_coordinates_esri <- cbind(coordinates, new_park_coordinates)

# renaming the columns
names(park_coordinates_esri)[1] <- "esri_long"
names(park_coordinates_esri)[2] <- "esri_lat"

# Canada and US map ----

library(geodata)
#level 0 = country; level 1 = province/state; level 2 = counties
provinces <- gadm(country="Canada", level=1, path = tempdir())

provinces <- readRDS("provinces.tif/gadm/gadm41_CAN_1_pk.rds")

#plot both shape files, layered
plot(provinces)

# import ndvi file
ndvi_bg <- "C:/Users/grace/Documents/GitHub/HWI_parks/ndvi/2021ndvi/2021_jun/VIIRS-Land_v001-preliminary_NPP13C1_S-NPP_20210630_c20220419155820.nc"
ndvi_bg <- terra::rast(ndvi_bg) #bg is 2021 jun 30
plot(ndvi_bg$NDVI)

# reproject NDVI to provinces crs
reprojected_bg <- terra::project(ndvi_bg,
                                 provinces,
                                 method = "near")

#crop reprojected ndvi bg to CanUS shape
cropped_provinces_ndvi <- crop(reprojected_bg, provinces, mask = TRUE) 
provinces_bg <- cropped_provinces_ndvi$NDVI
saveRDS(provinces_bg,file ="rds/Canmap.rds")
provinces_bg <- readRDS("rds/Canmap.rds")

#find the extent of the raster
ext(provinces_bg)

#set the bounding box
bbox <- ext(c(-141.006866, -52.6000041603337, 41.6999988824795, 83.0999985311861))

#crop the ndvi
bg_crop <- crop(provinces_bg, bbox)

#write raster
writeRaster(bg_crop, "figures/bg_crop.tif", overwrite = TRUE)

#REPROJECT BG_CROP 
bg_reproject <- terra::project(bg_crop,
                               "ESRI:102001")

plot(bg_crop)
saveRDS(bg_crop,file ="rds/bg_crop.rds")
bg_crop <- readRDS("rds/bg_crop.rds")

#crop the map
Can_crop <- crop(provinces, bbox)
saveRDS(Can_crop,file ="rds/Can_crop.rds")
Can_crop <- readRDS("rds/Can_crop.rds")

plot(Can_crop)

# Define manual color scale due to adding national parks ----
manual_colors <- c("WATE"= "#560133", "ELKI" = "#790149", "JASP" = "#9F0162", "WOOD" = "#C7007C",
                   "BANF" = "#EF0096", "YOHO" = "#FF5AAF", "KOOT" = "#FF9DCB", "REVE" = "#FFCFF2",
                   "PRIM" = "#450270", "GLAC" = "#65019F", "WAPU" = "#8400CD", "FUND" = "#A700FC",
                   "KOUC" = "#DA00FD", "NOVA" = "#FF3CFE", "KEJI" = "#FF92FD", "AULA" = "#FFCCFE",
                   "NAHA" = "#5A000F", "FIVE" = "#7E0018", "PELE" = "#A40122", "GBIS" = "#CD022D",
                   "THIS" = "#F60239", "PEIS" = "#FF6E3A", "FORI" = "#FFAC3B", "PALB" = "#FFDC3D", "IVVA" = "#FF4C30")


# Plotting the map 
provinces_sf <- st_as_sf(Can_crop)

#NDVI colour palette
NDVI_cols <- colorRampPalette(rev(c("#0f2902", "#1d3900","#193401","#274009","#2e4511",
                                    "#3d4f21", "#485921","#536321","#69761f","#868924",
                                    "#8d8e37","#aaa263","#b5a975","#c2b58c","#c7b995",
                                    "#cdbf9f","#e3d6c6","#e7dbce")))

reprojected_new_map <- 
  ggplot() +
  geom_spatraster(data = bg_reproject, alpha = 0.8, maxcell = 5e+08) + #ndvi bg
  scale_fill_gradientn(name = "Normalized Difference Vegetation Index",
                       colours = NDVI_cols(255),
                       na.value = NA,
                       breaks = c(11,  63.75, 127.50, 191.25, 254.00),
                       labels = c(-1.0, -0.5,  0.0,  0.5,  1.0)) +
  geom_sf(data = provinces_sf, fill = "transparent", color = "black", size = 1) + #map
  geom_point(data = park_coordinates_esri, aes(x = esri_long, y = esri_lat, col = park, shape = park), 
             size = 3, alpha = 0.8) +
  guides(col = guide_legend(override.aes = list(alpha=0.8,
                                                shape = rep(17,25))),
         shape = "none", alpha = "none") +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  scale_shape_manual(values = rep(17,25)) +
  #scale_alpha_manual(values = c(0.8,0.6)) +
  theme(#panel.background = element_blank(),
        panel.background = element_rect(fill="transparent"), #transparent panel bg
        plot.background = element_rect(fill="transparent", color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "none",
        legend.justification = "center",
        legend.direction = "vertical",
        #legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(-1,0,-1,0), "cm"),
        plot.title = element_text(vjust = -8.5, hjust = 0.03,
                                  size = 30, family = "sans", face = "bold")) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change

ggsave(reprojected_new_map, filename = "figures/new_map_reprojected.png", width = 6.86, height = 6, units = "in", dpi = 600, background ="transparent")

#END OF MAP ----


# gam for HWI and residuals -----------------------------


library(mgcv)


# HWI and NDVI model!----
hwi_ndvi$park <- as.factor(hwi_ndvi$park)

all_parks_model <- gam(HWI ~
               # global smooths
               s(residuals, park, bs = "fs", k = 6), #month effect
             #weights = Weights,
             family = nb(link = 'log'),
             data = hwi_ndvi,
             method = "REML")

summary(all_parks_model)
#plot(all_parks_model, pages = 1, col = manual_colors) #colour code this!! (y-axis is log scale)
#legend("right", legend = names(manual_colors), fill = manual_colors) # legend not matching??

saveRDS(all_parks_model,file ="rds/all_parks_model.rds")
save(all_parks_model, file = "all_parks_model.rda")

# plot to colour code ----
library(mgcViz)
library(gratia)
library(ggplot2)
library(dplyr)
library(viridis)
library(gridExtra)

# extract the data
parks = smooth_estimates(all_parks_model, select = "s(residuals,park)") #extract the data
# add confidence intervals
parks = add_confint(parks)
# rename a value, but not necessary
parks$smooth = 'park'

gam_plot <- 
ggplot(parks, aes(x = residuals)) + 
  geom_vline(aes(xintercept = 0), col = "grey70", linetype = "dashed") +
  geom_line(data = parks, aes(x = residuals, y = .estimate, color = park), linewidth = 0.5) +
  # set the colors
  scale_color_manual(name = "Park", values = manual_colors) + # c("#333BFF", "#CC6600", "#9633FF")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  scale_y_continuous(limits = c(-2, 3.5), expand = c(0,0.01))+
  scale_x_continuous(limits = c(-2, 2), expand = c(0,0.01),
                     breaks = c(-2, 0, 2),
                     labels = c(-2, 0, 2))+
  labs(x = "NDVI Residuals", y = "log(monthly HWIs)") #tag = 'a)')

ggsave(gam_plot, filename = "figures/new_gam_plot2_ppt.png", width = 6, height = 5, units = "in", dpi = 600)

# END OF HWI NDVI GAM----



# plot the HWI predictions for one park: example is BANFF ----
# seq -2 to 2 is the x-axis range, 0.01 is distance between data points
testt <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                             
                                             park = "BANF"),
                 type = "response", # to see how HWI responds to NDVI residuals
                 se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = testt$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = testt$fit + 1.96*testt$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = testt$fit - 1.96*testt$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "BANF"),])

# # residuals of model 
# residuals(test2)
# 
# # add the residuals as a new column into the mean_ndvi_df dataframe ----
# mean_ndvi_df$tweedie_residuals <- residuals(test2)
# 
# # merging HWI and NDVI dataframes
# HWI_NDVI <- merge(mean_ndvi_df, HWI_dropped)
# 
# # new data frame with aggregate by HWI number 
# new_hwi_ndvi <- aggregate(HWI ~ month + year + year_month+ park + ndvi_monthly_mean + scaled_mean_ndvi + tweedie_residuals, data = HWI_NDVI, FUN = "length")
# 
# 
# # plot HWI with tweedie NDVI residuals ----
# ggplot() +
#   geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
#   geom_point(data = new_hwi_ndvi, aes(x = tweedie_residuals, y = HWI, col = park)) +
#   geom_smooth(data = new_hwi_ndvi, aes(x = tweedie_residuals, y = HWI, col = park),method = "lm") +
#   xlab("Tweedie Residuals") +
#   ylab("HWI") +
#   scale_y_log10() +
#   scale_colour_manual(name="Region",
#                       values = manual_colors) +
#   theme_bw() +
#   geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.3) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.y = element_text(size=12, family = "sans", face = "bold"),
#         axis.title.x = element_text(size=12, family = "sans", face = "bold"),
#         axis.text.y = element_text(size=10, family = "sans"),
#         axis.text.x  = element_text(size=10, family = "sans"),
#         legend.position = "right",
#         legend.title = element_text(face = "bold"),
#         legend.background = element_blank(),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# test <-
#   gam(
#     scaled_mean_ndvi ~ #scale ndvi from 0 to 1 to fit beta distribution
#       # fixed effects
#       park +
#       # global smooths
#       s(month, bs = "cc", k = 4) + #month effect
#       s(year, k = 8) + #year effect
#       ti(month, year, k = 6), #month/ year interaction
#     family = "betar",
#     #beta location scale distribution for the data
#     data = mean_ndvi_df,
#     method = 'fREML'
#   )

#ignore below ----







corrupt <- sapply("2012ndvi",
                  function(filename) {
                    .r <- tryCatch(rast(filename),
                                   error = function(e) return(as.character(e)))
                    return(is.character(.r))
                  }) %>%
  suppressWarnings()
corrupt
corrupt <- corrupt[which(corrupt)] # only keep TRUE values
corrupt

while(any(corrupt)) {
  #find file names
  files <- substr(x = names(corrupt),
                  start = nchar("C:/Users/grace/Documents/GitHub/HWI_parks/ndvi/2012ndvi/") + 1,
                  stop = nchar(names(corrupt)))
  
  years <- substr(files,
                  start = nchar(files) - nchar('yyyymmdd_cyyyymmddhhmmss.nc') + 1,
                  stop = nchar(files) - nchar('mmdd_cyyyymmddhhmmss.nc'))
}

re-download the corrupt NDVi rasters
urls <- paste0('https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2010/',
               files)

lapply(1:length(urls), function(.i){
  path <- paste0("C:/Users/grace/Documents/GitHub/HWI_parks/2010ndvi/2010_jan/", files[.i])
  try(download.file(urls[.i], destfile = path))
})

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




library("xml2")
library("rvest")
library("dplyr")
library("terra")



#extract all links for each year
url_path <- "https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/"
pg <- read_html(url_path)
linkys <- html_attr(html_nodes(pg, "a"), "href")

LINKS <- list()
for(i in 1:length(linkys)){
  link <- paste(url_path, linkys[i], sep = "")
  LINKS[i] <- link
}

LINKS <- do.call(rbind, LINKS)

#extract links for each file in each year
for(i in 6:length(LINKS)){
  url <- LINKS[i]
  pag <- read_html(url)
  ndvi_links <- paste(LINKS[i], html_attr(html_nodes(pag, "a"), "href"),  sep = "")
  filenames <- html_attr(html_nodes(pag, "a"), "href")
  
  for(j in 6:length(ndvi_links)){
    url_path <- ndvi_links[j]
    path <- paste("Canada/NDVI/NOAA_Files/",filenames[j], sep="")
    try(download.file(url_path, destfile = path))
    
    Sys.sleep(5)
  }
  
  Sys.sleep(5)
}

#test the files to see if they work
file1 <- "Canada/NDVI/NOAA_Files/AVHRR-Land_v005_AVH13C1_NOAA-07_19810624_c20170610041337.nc"
file2 <- "AVHRR-Land_v005_AVH13C1_NOAA-07_19810627_c20170610050500.nc"
NDVI <- terra::rast(file2)
plot(NDVI[[1]])

# raster ----
worldvi <- unique(list.files(path = '2012ndvi/2012_nov/', 
                             pattern = ".nc", full.names = T))



# corruption ----
corrupt <- sapply(worldvi,
                  function(filename) {
                    .r <- tryCatch(rast(filename),
                                   error = function(e) return(as.character(e)))
                    return(is.character(.r))
                  }) %>%
  suppressWarnings()
corrupt
corrupt <- corrupt[which(corrupt)] # only keep TRUE values
corrupt

while(any(corrupt)) {
  # find file names
  files <- substr(x = names(corrupt),
                  start = nchar('2012ndvi/2012_nov//') + 1,
                  stop = nchar(names(corrupt)))
  
  years <- substr(files,
                  start = nchar(files) - nchar('yyyymmdd_cyyyymmddhhmmss.nc') + 1,
                  stop = nchar(files) - nchar('mmdd_cyyyymmddhhmmss.nc'))
  
  # re-download the corrupt NDVi rasters
  urls <- paste0('https://www.ncei.noaa.gov/data/land-normalized-difference-vegetation-index/access/2012/',
                 years, '/', files)
  
  lapply(1:length(urls), function(.i){
    path <- paste0("Canada/NDVI/NOAA_Files/", files[.i])
    try(download.file(urls[.i], destfile = path))
  })
  
  # check again what files are corrupt
  corrupt <- sapply(names(corrupt),
                    function(filename) {
                      .r <- tryCatch(rast(filename),
                                     error = function(e) return(as.character(e)))
                      return(is.character(.r))
                    }) %>%
    suppressWarnings()
  corrupt <- corrupt[which(corrupt)] # only keep TRUE values
}


