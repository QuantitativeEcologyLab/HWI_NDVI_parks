
# results gam code ----
test <-
  gam(
    scaled_mean_ndvi ~ #scale ndvi from 0 to 1 to fit beta distribution
      # fixed effects
      park +
      # global smooths
      s(month, bs = "cc", k = 4) + #month effect
      s(year, k = 8) + #year effect
      ti(month, year, k = 6), #month/ year interaction
    # # in/out level smooths
    # s(year, park, bs = 'fs', k = 15) + #yearly trends in parks
    #   s(doy, park, bs = 'fs', k = 12, xt = list(bs = 'cc')) + #seasonal trends in parks
    #   # tensor interaction terms
    #   ti(year, doy, bs = c('cr', 'cc'), k = c(15, 20)) + #yearly trends over time
    #   ti(long, lat, year, bs = c('ds', 'cr'), d = c(2, 1), k = c(20, 10)) + 
    #   ti(long, lat, doy, bs = c('ds', 'cc'), d = c(2, 1), k = c(20, 15)),
    family = "betar",
    #beta location scale distribution for the data
    data = mean_ndvi_df,
    method = 'fREML'
    # discrete = TRUE,
    #knots = list(doy = c(0.5, 366.5)),
    #control = gam.control(nthreads = 1, trace = TRUE)
  )






















test <-
  gam(
    scaled_mean_ndvi ~ #scale ndvi from 0 to 1 to fit beta distribution
      # fixed effects
      park +
      # global smooths
      s(month, bs = 'tp', k = 8) + #month effect
      s(year, bs = 'tp', k = 12) + #year effect
      s(month, year, bs = 'tp', k = 70) + #month/ year interaction
      # in/out level smooths
      #s(year, park, bs = 'fs', k = 15) + #yearly trends in parks
      # s(doy,park, bs = 'fs', k = 12, xt = list(bs = 'cc')) + #seasonal trends in parks
      # # tensor interaction terms
      # ti(year, doy, bs = c('cr', 'cc'), k = c(15, 20)) + #yearly trends over time
      # ti(
      #   long,
      #   lat,
      #   year,
      #   bs = c('ds', 'cr'),
      #   d = c(2, 1),
    #   k = c(20, 10)
    # ) +
    # ti(
    #   long,
    #   lat,
    #   doy,
    #   bs = c('ds', 'cc'),
    #   d = c(2, 1),
    #   k = c(20, 15)
    # ),
    family = betar,
    #beta location scale distribution for the data
    data = mean_ndvi_df,
    method = 'fREML'
    # discrete = TRUE,
    #knots = list(doy = c(0.5, 366.5)),
    #control = gam.control(nthreads = 1, trace = TRUE)
  )






















# Set directory to folder containing all ndvi files (2010-2021) 
# loop over all years in the directory 

nc.year_dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/ndvi"
setwd(nc.year_dir)

# Create a folder for the cropped NDVI files
output_year_dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_ndvi"
if (!dir.exists(output_year_dir)) {
  dir.create(output_year_dir)
}

# import JASP shapefile
jasper_shape <- readRDS("../rds/jasper.rds")

  # List all month folders in the year folder
  month_folders <- list.dirs(path = nc.year_dir, full.names = TRUE, recursive = FALSE)
  
  # Loop through each month folder
  for (nc.dir in month_folders) {
    # Extract month from the directory path
    file_month <- tools::file_path_sans_ext(basename(nc.dir))
    
    # Create a folder for the current month in the output directory
    output_month_dir <- file.path(output_year_dir, file_month)
    if (!dir.exists(output_month_dir)) {
      dir.create(output_month_dir)
    }
    
    # Make a list of the files in the month directory
    nc.files <- list.files(path = nc.dir, pattern = "*.nc", full.names = TRUE)
    
    # Loop through all the ndvi files for the current month
    for (file in nc.files) {
      
      # make them rasters
      #ndvi_files <- lapply(nc.files, rast) 
      
      # stack the rasters
      #stack <- stack(ndvi_files) 
      
      # make the spatrasters
      spat <- rast(nc.files)
      spat <- spat[[which(names(spat) == "NDVI")]]
      
      # Reproject the raster to the CRS of jasper_shape
      reprojected_spat <- terra::project(spat, crs(jasper_shape), method = "near")
      
      # crop and mask to JASP shapefile
      # loop over all park files 
      cropped_spat <- crop(reprojected_spat, jasper_shape, mask = TRUE)
      
      # make it rasters
      #c_raster <- as(c, "Raster") #plot this, this works
      
      # save the output 
      # update file name based on months and parks and year 
      output_file <- file.path(output_month_dir,paste0(file_month, "_NDVI_cropped.tif"))
      writeRaster(cropped_spat, filename = output_file, 
                  # options = c("COMPRESS=DEFLATE", "TFW=YES"), 
                  overwrite = FALSE)
    }
  }





























# cropping for loop trials ----
# for loop for cropping ndvi to JASP ----
# Set directory to folder containing all ndvi files (2010-2022)
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi/2011jan" 
setwd (nc.dir)

# Create a folder for the cropped NDVI files
output_dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_2011_ndvi"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# import JASP shapefile
jasper_shape <- readRDS("../../rds/jasper.rds")

# Make a list of the files in the directory
nc.files <- list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE)

# loop through all the ndvi files
for(i in 1:length(nc.files)){ 
  
  # make them rasters
  ndvi_files <- lapply(nc.files, raster) 
  
  # stack the rasters
  stack <- stack(ndvi_files) 
  
  # make the spatrasters
  spat <- rast(stack) # 1 day only?
  
  # crop and mask to JASP shapefile
  c <- crop(spat, jasper_shape, mask = TRUE)
  
  # make it rasters
  c_raster <- as(c, "Raster")
  
  # mask to JASP shapefile
  # m <- mask(c, jasper_shape) #can't mask, CRS X match 
  
  # save the output 
  
  # save the output 
  output_month_dir <- file.path(output_dir, file_month)
  if (!dir.exists(output_month_dir)) {
    dir.create(output_month_dir)
  }
  
  output_file <- file.path(output_month_dir, basename(nc.files[i]))
  writeRaster(c, filename = output_file, overwrite = TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES")) #Error: [writeRaster] cannot guess file type from filename
  
}

#writeRaster not working
# how to only select for nc files? .............................................

# Set directory to folder containing all ndvi files (2010-2022)
nc.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/2011ndvi" 
setwd(nc.dir)

# Create a folder for the cropped NDVI files
output_dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_2011_ndvi"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# Import JASP shapefile
jasper_shape <- readRDS("../rds/jasper.rds")

# Make a list of the directories in the main directory (assuming each directory represents a month)
month_directories <- list.dirs(path = nc.dir, full.names = TRUE, recursive = FALSE)

# Loop through each month directory
for (i in 1:length (month_directories)) { 
  # Extract month from the directory path
  month <- format(as.Date(month_dir, format = "%Y%m%d"), "%B")
  
  # Make a list of the files in the directory
  nc.files <- list.files(path = nc.dir, pattern = "*.nc", full.names = TRUE)
  
  # Create a folder for the current month in the output directory
  output_month_dir <- file.path(output_dir, month)
  if (!dir.exists(output_month_dir)) {
    dir.create(output_month_dir)
  }
  
  # Loop through all the ndvi files for the current month
  for (file in nc.files) {
    # make them rasters
    ndvi_file <- raster(file)
    
    # crop and mask to JASP shapefile
    c <- crop(ndvi_file, jasper_shape, mask = TRUE)
    
    # make it rasters
    c_raster <- as(c, "Raster")
    
    # save the output 
    output_file <- file.path(output_month_dir, basename(file))
    writeRaster(c_raster, filename = output_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TFW=YES"))
  }
}

# how to make it read my month folders inside the year folder and crop each month separately --> save each month into a separate folder?






# Set directory to folder containing all ndvi files (2010-2022)
# mean ndvi trials ----
cropped_ndvi.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_2011_ndvi/2011_apr" 
setwd (nc.dir)

# Create a folder for the cropped NDVI files
output_mean.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_mean_ndvi"
if (!dir.exists(output_mean.dir)) {
  dir.create(output_mean.dir)
}

# List all month folders in the year directory
month_folders <- list.dirs(path = nc.year_dir, full.names = TRUE, recursive = FALSE)

# Loop through each month folder
for (nc.dir in month_folders) {
  # Extract month from the directory path
  file_month <- tools::file_path_sans_ext(basename(nc.dir))
  
  # Create a folder for the current month in the output directory
  output_month_dir <- file.path(output_year_dir, file_month)
  if (!dir.exists(output_month_dir)) {
    dir.create(output_month_dir)
  }
  
  # Make a list of the files in the month directory
  nc.files <- list.files(path = nc.dir, pattern = "*.tif", full.names = TRUE)
  
  # make them rasters
  cropped_ndvi_files <- lapply(nc.files, raster) 
  
  # Initialize an empty raster stack to accumulate values for each file
  # raster_stack <- stack(cropped_ndvi_files)
  
  # Loop through all the ndvi files for the current month
  for (file in nc.files) {
    
    # Read the raster file
    crop.raster <- raster(cropped_ndvi_files)
    
    # Add the raster to the stack
    #raster_stack <- stack(raster_stack, crop.raster)
    crop.raster <- list()
  }
  
  # take the mean of the month
  #crop.mean <- mean(raster_stack, na.rm = TRUE)
  crop.mean <- app(crop.raster, mean, na.rm = TRUE)
  
  # save the output 
  output_file <- file.path(output_mean.dir, "all_months_mean.tif")
  # writeRaster(crop.mean, filename = output_file, format = "GTiff", #options = c("COMPRESS=DEFLATE", "TFW=YES")
  #  overwrite = TRUE)
  
}

# ...........................................................................

# Set directory to folder containing all ndvi files (2010-2022)
cropped_ndvi.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_2011_ndvi/2011_apr" 
setwd(cropped_ndvi.dir)  # Corrected the directory setting

# Create a folder for the cropped NDVI files
output_mean.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_mean_ndvi"
if (!dir.exists(output_mean.dir)) {
  dir.create(output_mean.dir)
}

# List all month folders in the year directory
month_folders <- list.dirs(path = cropped_ndvi.dir, full.names = TRUE, recursive = FALSE)

# Loop through each month folder
for (nc.dir in month_folders) {
  # Extract month from the directory path
  file_month <- tools::file_path_sans_ext(basename(nc.dir))
  
  # Create a folder for the current month in the output directory
  output_month_dir <- file.path(output_mean.dir, file_month)
  if (!dir.exists(output_month_dir)) {
    dir.create(output_month_dir)
  }
  
  # Make a list of the files in the month directory
  nc.files <- list.files(path = nc.dir, pattern = "*.tif", full.names = TRUE)
  
  # Make them rasters
  cropped_ndvi_files <- lapply(nc.files, raster) 
  
  # Initialize an empty raster stack to accumulate values for each file
  raster_stack <- stack(cropped_ndvi_files)
  
  # Loop through all the ndvi files for the current month
  for (file in nc.files) {
    # Read the raster file
    crop.raster <- raster(file)
    
    # Add the raster to the stack
    raster_stack <- addLayer(raster_stack, crop.raster)
  }
  
  # Take the mean of the month
  crop.mean <- mean(raster_stack, na.rm = TRUE)
  
  # Save the output 
  output_file <- file.path(output_month_dir, paste0(file_month, "_mean.tif"))  # Adjusted the output file name
  writeRaster(crop.mean, filename = output_file, format = "GTiff", options = c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = TRUE)
}

# Set directory to folder containing all ndvi files (2010-2022)
cropped_ndvi.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_2011_ndvi/2011_apr" 
setwd(cropped_ndvi.dir)  # Corrected the directory setting

# Create a folder for the cropped NDVI files
output_mean.dir <- "C:/Users/grace/Documents/GitHub/HWI_parks/cropped_mean_ndvi"
if (!dir.exists(output_mean.dir)) {
  dir.create(output_mean.dir)
}

# List all month folders in the year directory
month_folders <- list.dirs(path = cropped_ndvi.dir, full.names = TRUE, recursive = FALSE)

# Loop through each month folder
for (nc.dir in month_folders) {
  # Extract month from the directory path
  file_month <- tools::file_path_sans_ext(basename(nc.dir))
  
  # Create a folder for the current month in the output directory
  output_month_dir <- file.path(output_mean.dir, file_month)
  if (!dir.exists(output_month_dir)) {
    dir.create(output_month_dir)
  }
  
  # Make a list of the files in the month directory
  nc.files <- list.files(path = nc.dir, pattern = "*.tif", full.names = TRUE)
  
  # Make them rasters
  cropped_ndvi_files <- lapply(nc.files, raster) 
  
  # Initialize an empty raster stack to accumulate values for each file
  raster_stack <- stack(cropped_ndvi_files)
  
  # Loop through all the ndvi files for the current month
  for (file in nc.files) {
    # Read the raster file
    crop.raster <- raster(file)
    
    # Add the raster to the stack
    raster_stack <- addLayer(raster_stack, crop.raster)
  }
  
  # Take the mean of the month
  crop.mean <- mean(raster_stack, na.rm = TRUE)
  
  # Save the output 
  output_file <- file.path(output_month_dir, paste0(file_month, "_mean.tif"))  # Adjusted the output file name
  writeRaster(crop.mean, filename = output_file, format = "GTiff", options = c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = TRUE)
}





# old! first normal model trends ----
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

# old! import polygon as vectors ----
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

# 2000-2021 ndvi trends in indiv parks ----
#look at the ndvi trend in Jasper ----
Jasper_NDVI <- all_ndvi %>% 
  filter(park %in% c("JASP"))

ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Jasper_NDVI, aes(x = year_month, y = ndvi_monthly_mean, col = park)) +
  geom_smooth(data = Jasper_NDVI, aes(x = year_month, y = ndvi_monthly_mean, col = park),method = "lm") +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
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

# merge HWI data with all_ndvi 


















#look at the ndvi trend in Ivvavik ----
Ivvavik_NDVI <- all_ndvi %>% 
  filter(park %in% c("IVVA"))

ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Ivvavik_NDVI, aes(x = year_month, y = ndvi_monthly_mean, col = park)) +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Region",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
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


#look at the ndvi trend in Five Fathom ----
Five_NDVI <- all_ndvi %>% 
  filter(park %in% c("FIVE"))

ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Five_NDVI, aes(x = year_month, y = ndvi_monthly_mean, col = park)) +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Region",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
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


#look at the ndvi trend in Pacific Rim ----
Pacific_rim_NDVI <- all_ndvi %>% 
  filter(park %in% c("PRIM"))

ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Pacific_rim_NDVI, aes(x = year_month, y = ndvi_monthly_mean, col = park)) +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Region",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
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


#look at the ndvi trend in Waterton ----
Waterton_NDVI <- all_ndvi %>% 
  filter(park %in% c("WATE"))

ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Waterton_NDVI, aes(x = year_month, y = ndvi_monthly_mean, col = park)) +
  geom_smooth(method = "lm", se = TRUE, size = 1) +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
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

# ndvi trend across parks ----
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = NDVI_2000_2021, aes(x = year_month, y = ndvi_monthly_mean, col = park)) +
  geom_smooth(data = NDVI_2000_2021, aes(x = year_month, y = ndvi_monthly_mean, col = park),method = "lm") +
  #scale_x_continuous(limits = c(2010,2021), expand = c(0,1)) +
  scale_colour_manual(name="Region",
                      values = manual_colors) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
  theme_bw() +
  # geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.3) +
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
