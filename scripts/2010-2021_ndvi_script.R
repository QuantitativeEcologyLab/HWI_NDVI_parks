# loading packages ----
library(xml2)
library(rvest)
library(dplyr)
library(terra)
library(raster)
library(lubridate) #convert whole columns to dates
library(zoo) #dates as year month


# loop for extracting all links for 2010 ----
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2010ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}


#test the files to see if they can plot ndvi 
file1 <- "2010ndvi/2010_jan/AVHRR-Land_v005_AVH13C1_NOAA-19_20100101_c20170406091314.nc"
file2 <- "2010ndvi/2010_dec/AVHRR-Land_v005_AVH13C1_NOAA-19_20101231_c20170406211535.nc"
NDVI <- terra::rast(file1)
plot(NDVI$NDVI) 

# download 2011-2021 data ----
# loop for extracting all links for 2011 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2011ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2012 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2012ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2013 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2013ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2014 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2014ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2015 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2015ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2016 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2016ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2017 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2017ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2018 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2018ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2019 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2019ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2020 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2020ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

# loop for extracting all links for 2021 ---- 
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
  path <- paste("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2021ndvi/",linkys[j], sep="")
  try(download.file(url_path, destfile = path, mode = "wb")) #add mode = wb and now it works --> the probably won't have to run corrupt file unless things don't work
  
  Sys.sleep(5)
  
}

#..............................................................................................

# for loop for cropping all the ndvi data into the park polygons and take monthly mean
nc.year_dir <- "C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/ndvi"
# setwd(nc.year_dir)

# import all of the boundaries for Canadian parks
CAshape <- st_read("data/shapefiles/ca_provinces/CLAB_CA_2023-09-08")

# Create a list of the names of the 25 parks to be studied 
test_parks <- c("WATE", "ELKI", "JASP", "WOOD",
                "BANF", "YOHO", "KOOT", "REVE",
                "PRIM", "GLAC", "WAPU", "FUND",
                "KOUC", "NOVA", "KEJI", "AULA",
                "NAHA", "FIVE", "PELE", "GBIS",
                "THIS", "PEIS", "FORI", "PALB", "IVVA")


# import JASP shapefile
jasper_shape <- readRDS("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/shapefiles/parks_polygons/jasper.rds")

# Define the CRS
CRS_canada <- crs(jasper_shape)

# List all year folders in the ndvi directory 
year_folders <- list.dirs(path = nc.year_dir, full.names = TRUE, recursive = FALSE)

RESULTS <- list()

# Loop through each year folder
for (i in 1:length(year_folders)) { #length(year_folders)
  
  year <- gsub(pattern = "ndvi",
               replacement = "",
               x = gsub(pattern = "data/ndvi/",
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

#save as CSV
write.csv(RESULTS, "C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/models/model_results/parksndvi.csv", row.names=FALSE)
RESULTS_df <- read.csv("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/models/model_results/parksndvi.csv")

# close loop of all the years 

# 2000-2009 NDVI ----
# in another script: 2000-2009_ndvi_script


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
write.csv(data_ndvi_mean, "C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/models/model_results/monthly_mean_ndvi.csv", row.names=FALSE)
mean_ndvi_df <- read.csv("data/models/model_results/monthly_mean_ndvi.csv")

# rescale ndvi in dataframe to 0-1 for beta gam
mean_ndvi_df <- mean_ndvi_df %>% 
  mutate((ndvi_monthly_mean+1)/2)

# rename columns 
names(mean_ndvi_df)[5] <- "scaled_mean_ndvi"

# change month and year to numeric
as.numeric(mean_ndvi_df$month)
as.numeric(mean_ndvi_df$year)

# gam for 2010-2021 ndvi ----
# GAM
ndvi2010_2021_gam <-
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


summary(ndvi2010_2021_gam)
plot(ndvi2010_2021_gam, pages = 1)

# residuals of model 1
residuals(ndvi2010_2021_gam)

# add the residuals as a new column into the HWI_grouped_species dataframe ----
mean_ndvi_df$residuals <- residuals(ndvi2010_2021_gam)

# looking at the distribution of the residuals 
hist(mean_ndvi_df$residuals)