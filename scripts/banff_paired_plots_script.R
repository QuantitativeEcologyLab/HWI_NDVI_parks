# loading packages
library(ggplot2)
library(gridExtra) # for multi-panel plotting 
library(dplyr)

# importing data
hwi_ndvi <- readRDS("C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/hwi_ndvi.rds")


# BANFF hwi over time ----
Banff_HWI <- hwi_ndvi %>% 
  filter(park %in% c("BANF"))

banf_hwi_gam <- 
  ggplot() +
  geom_point(data = Banff_HWI, aes(x = year, y = HWI, col = park), alpha = 0.4) +
  geom_smooth(data = Banff_HWI, aes(x = year, y = HWI, col = park),
              method = "gam",
              formula = y ~ s(x, bs = "tp", k = 5),
              method.args = list(family = "poisson"),
              se = F) +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  ggtitle("A") +
  xlab("Time") +
  ylab("Monthly HWI") +
  scale_y_continuous(limits = c(0, 750), expand = c(0,2))+
  scale_x_continuous(limits = c(2010, 2021), expand = c(0,0.01),
                     breaks = c(2010,2012,2015,2017,2020),
                     labels = c(2010,2012,2015,2017,2020))+
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

ggsave(banf_hwi_gam, filename = "figures/case_study_banff/banf_hwi_gam_trend.png", width = 6, height = 4, units = "in", dpi = 600, background = "white")

# ...................................................................................................

#BANF ----
Banff_NDVI <- all_ndvi %>% 
  filter(park %in% c("BANF"))

banf_ndvi <- 
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Banff_NDVI, aes(x = year, y = ndvi_monthly_mean, col = park), alpha = 0.4) +
  geom_smooth(data = Banff_NDVI, aes(x = year, y = ndvi_monthly_mean, col = park),
              method = "lm",
              se = T)  +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  ggtitle("B") +
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

ggsave(banf_ndvi, filename = "figures/case_study_banff/banf_ndvi_trend.png", width = 6, height = 4, units = "in", dpi = 600, background = "white")

#.........................................................................................
# Add the results for Banff 
# make axis title bigger
BANFpredict <- readRDS("data/models/model_results/BANFpredict.rds")

par(cex.lab = 1.2)
# plot the line for the park 
plot(y = BANFpredict$fit, x = seq(-2, 2, 0.01), type = "l", ylim = c(0,250), xlab = "NDVI Residuals", ylab = "Monthly HWIs", col = "#EF0096", font.lab = 2)
# upper conf interval
lines(y = BANFpredict$fit + 1.96*BANFpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# lower conf interval
lines(y = BANFpredict$fit - 1.96*BANFpredict$se.fit, x = seq(-2, 2, 0.01), type = "l", col = "grey60")
# adding the real data into the plots of predictions 
points(HWI ~residuals, data = hwi_ndvi[hwi_ndvi$park == "BANF",], pch = 16, col = alpha("#EF0096",0.3))

#.........................................................................................

# Plotting them side by side 
banff_paired_plots <- grid.arrange(banf_hwi_gam, banf_ndvi,
                     ncol=2)

ggsave(banff_paired_plots, filename = "figures/case_study_banff/banf_paired.png", width = 8, height = 4, units = "in", dpi = 600, background = "white")
 