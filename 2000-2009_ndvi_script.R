# loading packages
library(terra) #shape file 
library(lubridate) #convert whole columns to dates
library(ggplot2) # for scatter plot

# NDVI 2000-2009 ----

nc.year_dir <- "D:/ndvi00-09"
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

RESULTS_2 <- list()

# Loop through each year folder
for (i in 1:length(year_folders)) { #length(year_folders)
  
  year <- gsub(pattern = "ndvi",
               replacement = "",
               x = gsub(pattern = "D:/ndvi00-09/",
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
  RESULTS_2[[i]] <- do.call(rbind,res_month) 
} # close of year loop

#SAVE AS RDA OR CSV
save(RESULTS_2, file = "test.rda")

# convert the final list to a data frame (daily ndvi mean and var)
RESULTS_2 <- do.call(rbind,RESULTS_2) 

write.csv(RESULTS_2, "C:/Users/grace/Documents/GitHub/HWI_parks/results/morendvi.csv", row.names=FALSE)
RESULTS2_df <- read.csv("results/morendvi.csv")

# close loop of all the years 

#Create data frame by grouping park means according to months and years ---- 


#convert to calendar dates
RESULTS2_df$date <- as.Date(RESULTS2_df$date, format = "%Y_%m_%d")

#extract year
RESULTS2_df$year <- lubridate::year(RESULTS2_df$date)

#extract month
RESULTS2_df$month <- lubridate::month(RESULTS2_df$date)

#rename columns
names(RESULTS2_df)[3] <- "ndvi_daily_mean"
names(RESULTS2_df)[4] <- "ndvi_daily_variance"

# Create a new dataframe for analysis for monthly ndvi mean to be grouped by park and year
more_data_ndvi_mean <- aggregate(ndvi_daily_mean ~ month + year + park, data = RESULTS2_df, FUN = mean, na.rm = TRUE)

#rename columns
names(more_data_ndvi_mean)[4] <- "ndvi_monthly_mean"

#save datafram as a csv
write.csv(more_data_ndvi_mean, "C:/Users/grace/Documents/GitHub/HWI_parks/results/more_monthly_mean_ndvi.csv", row.names=FALSE)
more_mean_ndvi_df <- read.csv("results/more_monthly_mean_ndvi.csv")

# preparing for GAM ----
# rescale ndvi in dataframe
more_mean_ndvi_df <- more_mean_ndvi_df %>% 
  mutate((ndvi_monthly_mean+1)/2)

# rename columns 
names(more_mean_ndvi_df)[5] <- "scaled_mean_ndvi"

# change month and year to numeric
as.numeric(more_mean_ndvi_df$month)
as.numeric(more_mean_ndvi_df$year)

# merge to form data frame with 2000-2021 data frame ----
NDVI_2000_2021 <- rbind(more_mean_ndvi_df,mean_ndvi_df)

#save datafram as a csv
write.csv(NDVI_2000_2021, "C:/Users/grace/Documents/GitHub/HWI_parks/results/NDVI_2000_2021.csv", row.names=FALSE)
NDVI_2000_2021 <- read.csv("results/NDVI_2000_2021.csv")
#Convert PARK ID to a factor
NDVI_2000_2021$park <- as.factor(NDVI_2000_2021$park)


# change year and month into dates --> add year_mon column 


# NDVI & parks GAM! ----

# BAM in parks, month, year
# predicts NDVI from 2000-2021 (based on these variables how do they affect NDVI)
# looks at NDVI changes
all_ndvi_gam <-
  bam(
    scaled_mean_ndvi ~ #scale ndvi from 0 to 1 to fit beta distribution
      # fixed effects
      park +
      # global smooths
      s(month, park, bs = "fs", k = 8, xt = list(bs = "cc")) + #based on month and park
      s(year, park, bs = "fs", k = 8) + #based on year and park 
      ti(month, year, k = 6), #based on month and year 
    family = "betar",
    #beta location scale distribution for the data
    data = NDVI_2000_2021,
    method = 'fREML'
  )


summary(all_ndvi_gam)
bam_plot <- plot(all_ndvi_gam, pages = 1, scheme = 2)


# residuals of model 1
residuals(all_ndvi_gam)

# add the residuals as a new column into the HWI_grouped_species dataframe ----
NDVI_2000_2021$residuals <- residuals(all_ndvi_gam)

#save datafram as a csv
write.csv(NDVI_2000_2021, "C:/Users/grace/Documents/GitHub/HWI_parks/results/NDVI_2000_2021_residual.csv", row.names=FALSE)
NDVI_2000_2021_residuals <- read.csv("results/NDVI_2000_2021_residual.csv")

# looking at the distribution of the residuals 
hist(NDVI_2000_2021$residuals)

# add a year_month column ----
# Create a new column combining year, month, and day
NDVI_2000_2021$date <- as.Date(paste(NDVI_2000_2021$year, NDVI_2000_2021$month, 1, sep="-"))

# Create a new column for year and month combination
NDVI_2000_2021$year_month <- format(NDVI_2000_2021$date, "%Y-%m")

# Convert the date column to Date format if necessary
NDVI_2000_2021$date <- as.Date(NDVI_2000_2021$date)

#save datafram as a csv
write.csv(NDVI_2000_2021, "C:/Users/grace/Documents/GitHub/HWI_parks/results/NDVI_2000_2021_residual_date.csv", row.names=FALSE)
all_ndvi <- read.csv("results/NDVI_2000_2021_residual_date.csv")


#plot the trend of residuals by year_month ----
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = all_ndvi, aes(x = year_month, y = residuals, col = park)) +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Region",
                     values = manual_colors) +
  xlab("Time") +
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


#NDVI trends in individual parks ----

#BANF ----
Banff_NDVI <- all_ndvi %>% 
  filter(park %in% c("BANF"))

#banf_ndvi <- 
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Banff_NDVI, aes(x = year, y = ndvi_monthly_mean, col = park), alpha = 0.4) +
  geom_smooth(data = Banff_NDVI, aes(x = year, y = ndvi_monthly_mean, col = park),
              method = "lm",
              se = T)  +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
  scale_y_continuous(limits = c(0, 0.25), expand = c(0,0.01))+
  scale_x_continuous(limits = c(2000, 2021), expand = c(0,0.01),
                     breaks = c(2000,2005,2010,2015,2020),
                     labels = c(2000,2005,2010,2015,2020))+
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
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) 
  
ggsave(banf_ndvi, filename = "figures/banf_ndvi_trend.png", width = 6, height = 4, units = "in", dpi = 600, background = "white")