# HWI over time ---- model as a gam!
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

ggsave(hwi_trend, filename = "figures/new_hwi_trend_ppt.png", width = 7, height = 5, units = "in", dpi = 600, background = "white")







# NDVI and HWI ----
ggplot() +
  geom_point(data = hwi_ndvi, aes(x = ndvi_monthly_mean, y = HWI, col = park)) +
  xlab("Monthly Mean NDVI") +
  ylab("HWI") +
  # using heat palette for parks
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
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# NDVI over time
ggplot() +
  geom_point(data = hwi_ndvi, aes(x = year, y = ndvi_monthly_mean, col = park), alpha = 0.1) +
  geom_smooth(data = hwi_ndvi, aes(x = year, y = ndvi_monthly_mean, col = park),method = "lm", se = FALSE) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
  # using heat palette for parks
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
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# NDVI changes over 21 years
ggplot() +
  geom_point(data = all_ndvi, aes(x = year, y = ndvi_monthly_mean, col = park), alpha = 0.1) +
  geom_smooth(data = all_ndvi, aes(x = year, y = ndvi_monthly_mean, col = park),method = "lm", se = TRUE) +
  xlab("Time") +
  ylab("Monthly Mean NDVI") +
  # using heat palette for parks
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
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


# BANFF hwi over time ----
Banff_HWI <- hwi_ndvi %>% 
  filter(park %in% c("BANF"))

banf_hwi_gam <- 
  ggplot() +
  #geom_hline(aes(yintercept = 0), col = "grey70", linetype = "dashed") +
  geom_point(data = Banff_HWI, aes(x = year, y = HWI, col = park), alpha = 0.4) +
  geom_smooth(data = Banff_HWI, aes(x = year, y = HWI, col = park),
              method = "gam",
              formula = y ~ s(x, bs = "tp", k = 5),
              method.args = list(family = "poisson"),
              se = F) +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
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

ggsave(banf_hwi_gam, filename = "figures/banf_hwi_gam_trend.png", width = 6, height = 4, units = "in", dpi = 600, background = "white")
