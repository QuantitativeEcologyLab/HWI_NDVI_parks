# loading packages ----
library(mgcv)

PARKS <- c("WATE", "ELKI", "JASP", "WOOD",
           "BANF", "YOHO", "KOOT", "REVE",
           "PRIM", "GLAC", "WAPU", "FUND",
           "KOUC", "NOVA", "KEJI", "AULA",
           "NAHA", "FIVE", "PELE", "GBIS",
           "THIS", "PEIS", "FORI", "PALB", "IVVA")

RESIDUALS <- seq(-2, 2, 0.01)

RESULTS3 <- list()
max <- list()
for(i in 1:length(PARKS)){
  
  all_parks_predict <- predict(all_parks_model, newdata = data.frame(residuals = RESIDUALS,
                                                               
                                                               park = PARKS[i]),
                         type = "response", # to see how HWI responds to NDVI residuals
                         se = TRUE # standard error = true --> for looking at confidence interval
  )
  
  RESULTS3[[i]] <- data.frame(park = PARKS[i],
                                residual = RESIDUALS,
                                prediction = all_parks_predict$fit,
                                SE = all_parks_predict$se.fit)
  
  # find inflection point (max)
  max_index <- which(RESULTS3[[i]]$prediction == max(RESULTS3[[i]]$prediction))
  max[[i]] <- RESULTS3[[i]][max_index, ]
  
  
  
  
}

RESULTS3 <- do.call(rbind, RESULTS3)
max <- do.call(rbind, max)

saveRDS(RESULTS3, file = "data/models/model_results/RESULTS3")
saveRDS(max, file = "data/models/model_results/max")

# ..........................................................................................
# plotting the model results for each individual park


# AULA ----
AULApredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "AULA"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)


# plot the line for the park 
plot(y = AULApredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = AULApredict$fit + 1.96*AULApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = AULApredict$fit - 1.96*AULApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "AULA"),])
# find inflection point (max)
which(AULApredict$fit == max(AULApredict$fit)) #401, 401


# FUND ----
FUNDpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "FUND"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = FUNDpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = FUNDpredict$fit + 1.96*FUNDpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = FUNDpredict$fit - 1.96*FUNDpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "FUND"),])
# find inflection point (max)
which(FUNDpredict$fit == max(FUNDpredict$fit)) #401, 401



# KEJI ----

RESIDUALS <- seq(-2, 2, 0.01)

KEJIpredict <- predict(all_parks_model, newdata = data.frame(residuals = RESIDUALS,
                                                             
                                                             park = "KEJI"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = KEJIpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = KEJIpredict$fit + 1.96*KEJIpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = KEJIpredict$fit - 1.96*KEJIpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "KEJI"),])
# find inflection point (max)
which(KEJIpredict$fit == max(KEJIpredict$fit)) #401, 401



# PALB ----
PALBpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "PALB"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = PALBpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = PALBpredict$fit + 1.96*PALBpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = PALBpredict$fit - 1.96*PALBpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "PALB"),])
# find inflection point (max)
which(PALBpredict$fit == max(PALBpredict$fit)) #189, 189


# THIS ----
THISpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "THIS"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = THISpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = THISpredict$fit + 1.96*THISpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = THISpredict$fit - 1.96*THISpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "THIS"),])
# find inflection point (max)
which(THISpredict$fit == max(THISpredict$fit)) #401, 401


# BANF ----
BANFpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "BANF"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

png(file = "figures/case_study_banff/BANFF_final.png", width = 6, height = 4, units = "in", res = 600)
# make axis title bigger
par(cex.lab = 1.2)
# plot the line for the park 
plot(y = BANFpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250), xlab = "NDVI Residuals", ylab = "Monthly HWIs", col = "#EF0096", font.lab = 2)
# upper conf interval
lines(y = BANFpredict$fit + 1.96*BANFpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = BANFpredict$fit - 1.96*BANFpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[hwi_ndvi$park == "BANF",], pch = 16, col = alpha("#EF0096",0.3))
# find inflection point (max)
which(BANFpredict$fit == max(BANFpredict$fit)) #154, 154
dev.off()

# GBIS ----
GBISpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "GBIS"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = GBISpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = GBISpredict$fit + 1.96*GBISpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = GBISpredict$fit - 1.96*GBISpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "GBIS"),])
# find inflection point (max)
which(GBISpredict$fit == max(GBISpredict$fit)) #1, 1


# KOOT ----
KOOTpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "KOOT"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = KOOTpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = KOOTpredict$fit + 1.96*KOOTpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = KOOTpredict$fit - 1.96*KOOTpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "KOOT"),])
# find inflection point (max)
which(KOOTpredict$fit == max(KOOTpredict$fit)) #1, 1


# PEIS ----
PEISpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "PEIS"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = PEISpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = PEISpredict$fit + 1.96*PEISpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = PEISpredict$fit - 1.96*PEISpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "PEIS"),])
# find inflection point (max)
which(PEISpredict$fit == max(PEISpredict$fit)) #266, 266



# WAPU ----
WAPUpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "WAPU"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = WAPUpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = WAPUpredict$fit + 1.96*WAPUpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = WAPUpredict$fit - 1.96*WAPUpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "WAPU"),])
# find inflection point (max)
which(WAPUpredict$fit == max(WAPUpredict$fit)) #401, 401



# ELKI ----
ELKIpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "ELKI"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = ELKIpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = ELKIpredict$fit + 1.96*ELKIpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = ELKIpredict$fit - 1.96*ELKIpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "ELKI"),])
# find inflection point (max)
which(ELKIpredict$fit == max(ELKIpredict$fit)) #278, 278



# GLAC ----
GLACpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "GLAC"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = GLACpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = GLACpredict$fit + 1.96*GLACpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = GLACpredict$fit - 1.96*GLACpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "GLAC"),])
# find inflection point (max)
which(GLACpredict$fit == max(GLACpredict$fit)) #401, 401



# KOUC ----
KOUCpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "KOUC"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = KOUCpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = KOUCpredict$fit + 1.96*KOUCpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = KOUCpredict$fit - 1.96*KOUCpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "KOUC"),])
# find inflection point (max)
which(KOUCpredict$fit == max(KOUCpredict$fit)) #401, 401



# PELE ----
PELEpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "PELE"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = PELEpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = PELEpredict$fit + 1.96*PELEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = PELEpredict$fit - 1.96*PELEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "PELE"),])
# find inflection point (max)
which(PELEpredict$fit == max(PELEpredict$fit)) #401, 401



# WATE ----
WATEpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "WATE"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = WATEpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = WATEpredict$fit + 1.96*WATEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = WATEpredict$fit - 1.96*WATEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "WATE"),])
# find inflection point (max)
which(WATEpredict$fit == max(WATEpredict$fit)) #401, 401


# FIVE ----
FIVEpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "FIVE"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = FIVEpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = FIVEpredict$fit + 1.96*FIVEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = FIVEpredict$fit - 1.96*FIVEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "FIVE"),])
# find inflection point (max)
which(FIVEpredict$fit == max(FIVEpredict$fit)) #294, 294



# IVVA ----
IVVApredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "IVVA"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = IVVApredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = IVVApredict$fit + 1.96*IVVApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = IVVApredict$fit - 1.96*IVVApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "IVVA"),])
# find inflection point (max)
which(IVVApredict$fit == max(IVVApredict$fit)) #401, 401



# NAHA ----
NAHApredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "NAHA"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = NAHApredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = NAHApredict$fit + 1.96*NAHApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = NAHApredict$fit - 1.96*NAHApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "NAHA"),])
# find inflection point (max)
which(NAHApredict$fit == max(NAHApredict$fit)) #401, 401


# PRIM ----
PRIMpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "PRIM"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = PRIMpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = PRIMpredict$fit + 1.96*PRIMpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = PRIMpredict$fit - 1.96*PRIMpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "PRIM"),])
# find inflection point (max)
which(PRIMpredict$fit == max(PRIMpredict$fit)) #401, 401


# WOOD ----
WOODpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "WOOD"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = WOODpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = WOODpredict$fit + 1.96*WOODpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = WOODpredict$fit - 1.96*WOODpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "WOOD"),])
# find inflection point (max)
which(WOODpredict$fit == max(WOODpredict$fit)) #66, 66



# FORI ----
FORIpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "FORI"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = FORIpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = FORIpredict$fit + 1.96*FORIpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = FORIpredict$fit - 1.96*FORIpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "FORI"),])
# find inflection point (max)
which(FORIpredict$fit == max(FORIpredict$fit)) #401, 401



# JASP ----
JASPpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "JASP"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = JASPpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = JASPpredict$fit + 1.96*JASPpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = JASPpredict$fit - 1.96*JASPpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "JASP"),])
# find inflection point (max)
which(JASPpredict$fit == max(JASPpredict$fit)) #165, 165


# NOVA ----
NOVApredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "NOVA"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = NOVApredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = NOVApredict$fit + 1.96*NOVApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = NOVApredict$fit - 1.96*NOVApredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "NOVA"),])
# find inflection point (max)
which(NOVApredict$fit == max(NOVApredict$fit)) #401, 401



# REVE ----
REVEpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "REVE"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = REVEpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = REVEpredict$fit + 1.96*REVEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = REVEpredict$fit - 1.96*REVEpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "REVE"),])
# find inflection point (max)
which(REVEpredict$fit == max(REVEpredict$fit)) #401, 401



# YOHO ----
YOHOpredict <- predict(all_parks_model, newdata = data.frame(residuals = seq(-2, 2, 0.01),
                                                             
                                                             park = "YOHO"),
                       type = "response", # to see how HWI responds to NDVI residuals
                       se = TRUE # standard error = true --> for looking at confidence interval
)

# plot the line for the park 
plot(y = YOHOpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250))
# upper conf interval
lines(y = YOHOpredict$fit + 1.96*YOHOpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = YOHOpredict$fit - 1.96*YOHOpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[which(hwi_ndvi$park == "YOHO"),])
# find inflection point (max)
which(YOHOpredict$fit == max(YOHOpredict$fit)) #401, 401

