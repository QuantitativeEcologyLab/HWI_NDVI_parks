# Loading packages ---- 
library(ggplot2) # for plots
library(dplyr) # for pipes
library(lubridate) #convert whole columns to dates
library(zoo) #dates as year month
library(canadianmaps) #import annotated map of Canada
library(sf) # spatial data
library(sp) #Spatial Points function
library(rstudioapi) #for creating colour palette
library(grDevices) #for creating colour palette
library(fBasics) #for creating colour palette
library(mgcv) #gam

# importing data
animals_involved <- read.csv("data/hwi/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")

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
write.csv(HWI_parks, "C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/old/hwi_parks.csv", row.names=FALSE)
HWI_parks <- read.csv("old/hwi_parks.csv")

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

