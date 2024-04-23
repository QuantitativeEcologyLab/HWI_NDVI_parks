# loading packages ----
library(ggplot2) # for scatter plot

# matching park IDs in HWI data to NDVI data----
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
HWI_parks$park[HWI_parks$park == "Yoho"]<- "YOHO"
HWI_parks$park[HWI_parks$park == "Terra_Nova"]<- "NOVA"
HWI_parks$park[HWI_parks$park == "Mount_Revelstoke"]<- "REVE"
HWI_parks$park[HWI_parks$park == "Elk_Island"]<- "ELKI"
HWI_parks$park[HWI_parks$park == "Georgian_Bay_Islands"]<- "GBIS"
HWI_parks$park[HWI_parks$park == "Point_Pelee"]<- "PELE"
HWI_parks$park[HWI_parks$park == "Thousand_Islands"]<- "THIS"
HWI_parks$park[HWI_parks$park == "Wood_Buffalo"]<- "WOOD"
HWI_parks$park[HWI_parks$park == "Prince_Edward_Island"]<- "PEIS"
HWI_parks$park[HWI_parks$park == "Ivvavik"]<- "IVVA"
HWI_parks$park[HWI_parks$park == "Kouchibouguac"]<- "KOUC"
HWI_parks$park[HWI_parks$park == "Fundy"]<- "FUND"
HWI_parks$park[HWI_parks$park == "Nahanni"]<- "NAHA"
HWI_parks$park[HWI_parks$park == "Aulavik"]<- "AULA"
HWI_parks$park[HWI_parks$park == "Fathom_Five"]<- "FIVE"

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
saveRDS(hwi_ndvi,file ="data/hwi_ndvi.rds")

#.......................................................................................
# visualisations ----

#visualise the trend of residuals by year_month ----
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


# visualise HWI with NDVI residuals ----
ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = hwi_ndvi, aes(x = residuals, y = HWI, col = park)) +
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

# visualise HWI over time ---- model as a gam!
hwi_trend <-
  ggplot() +
  geom_point(data = hwi_ndvi, aes(x = year, y = HWI, col = park)) +
  geom_smooth(data = hwi_ndvi, aes(x = year, y = HWI, col = park),
              method = "gam",
              formula = y ~ s(x, bs = "tp", k = 5),
              method.args = list(family = "poisson"),
              se = F) +
  xlab("Year") +
  ylab("Monthly HWIs") +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position = "none",
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  scale_y_continuous(limits = c(0, 800), expand = c(0,2))+
  scale_x_continuous(limits = c(2010, 2021), expand = c(0,0.2),
                     breaks = c(2010,2012,2015,2017,2020),
                     labels = c(2010,2012,2015,2017,2020))

ggsave(hwi_trend, filename = "figures/supplementary/hwi_trend.png", width = 7, height = 5, units = "in", dpi = 600, background = "white")

# monthly HWI with monthly mean NDVI
monthly_hwi_mean_ndvi <-
ggplot() +
  geom_point(data = hwi_ndvi, aes(x = ndvi_monthly_mean, y = HWI, col = park)) +
  xlab("Monthly Mean NDVI") +
  ylab("monthly HWI") +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
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
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(monthly_hwi_mean_ndvi, filename = "figures/supplementary/hwi_and_ndvi_trend.png", width = 7, height = 5, units = "in", dpi = 600, background = "white")

# NDVI changes over 21 years
ndvi2000_2021 <-
ggplot() +
  geom_point(data = all_ndvi, aes(x = year, y = ndvi_monthly_mean, col = park), alpha = 0.1) +
  geom_smooth(data = all_ndvi, aes(x = year, y = ndvi_monthly_mean, col = park),method = "gam", se = FALSE) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
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
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(ndvi2000_2021, filename = "figures/supplementary/ndvi_trend.png", width = 7, height = 5, units = "in", dpi = 600, background = "white")
