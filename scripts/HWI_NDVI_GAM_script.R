# loading packages ----
library(mgcv)
library(mgcViz) #visualise gam 
library(gratia) #plot gam smooths with ggplot2
library(ggplot2)
library(dplyr)




# gam for HWI and NDVI residuals ----
hwi_ndvi <- readRDS("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/hwi_ndvi.rds")

hwi_ndvi$park <- as.factor(hwi_ndvi$park)

all_parks_model <- gam(HWI ~
                         # global smooths
                         s(residuals, park, bs = "fs", k = 6), #month effect
                       #weights = Weights,
                       family = nb(link = 'log'),
                       data = hwi_ndvi,
                       method = "REML")

summary(all_parks_model)

saveRDS(all_parks_model,file ="data/models/all_parks_model.rds")
save(all_parks_model, file = "data/models/all_parks_model.rda")
all_parks_model <- readRDS("data/models/all_parks_model.rds")

#........................................................................................

# plot to colour code model results by park ----
# extract the data
parks = smooth_estimates(all_parks_model, select = "s(residuals,park)") #extract the data
# add confidence intervals
parks = add_confint(parks)
# rename a value, but not necessary
parks$smooth = 'park'

# plot the results ----
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

ggsave(gam_plot, filename = "figures/park_hwi_ndvi_trends/gam_plot.png", width = 6, height = 5, units = "in", dpi = 600)

