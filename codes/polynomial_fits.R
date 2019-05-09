
# Just polynomial
tsv <- sb_tbl_upt %>%
  ggplot(aes(x = .[,2], y = .[,3])) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = F) +
  xlab(names(drivers_index)[i]) +
  ylab("Sustainability aggregated score") +
  theme(legend.title = element_blank()) +
  theme_bw() +
  theme(axis.title   = element_text(size = 20),
        axis.text    = element_text(size = 15),
        legend.title = element_blank(),
        legend.text  = element_text(size = 15))

ggsave(filename = paste0("../SFS_indicators/_graphs/_relations_drivers_vs_SFS_index/relation_SFS_index_vs_", names(drivers_index)[i], "_removing_outliers_poly.png"),
       plot = tsv,
       device = "png",
       units = "in",
       width = 8,
       height = 8)

# Both sides
tsv <- sb_tbl_upt %>%
  ggplot(aes(x = .[,2], y = .[,3], colour = factor(.[,4]))) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = F) +
  xlab(names(drivers_index)[i]) +
  ylab("Sustainability aggregated score") +
  theme(legend.title = element_blank()) +
  theme_bw() +
  theme(axis.title   = element_text(size = 20),
        axis.text    = element_text(size = 15),
        legend.title = element_blank(),
        legend.text  = element_text(size = 15))

# Just one side
tsv <- sb_tbl_upt %>%
  ggplot(aes(x = chg_empl_services, y = SFS_index), colour = "red") +
  geom_point() +
  stat_smooth(data = sb_tbl_upt %>% dplyr::filter(Aux == "Positive"), method = "lm", formula = y ~ poly(x, 2), size = 1, se = F) +
  xlab(names(drivers_index)[i]) +
  ylab("Sustainability aggregated score") +
  theme(legend.title = element_blank()) +
  theme_bw() +
  theme(axis.title   = element_text(size = 20),
        axis.text    = element_text(size = 15),
        legend.title = element_blank(),
        legend.text  = element_text(size = 15))

tsv <- sb_tbl_upt %>%
  ggplot(aes(x = chg_serv_trd, y = SFS_index), colour = "red") +
  geom_point() +
  stat_smooth(data = sb_tbl_upt %>% dplyr::filter(Aux == "Positive"), method = "lm", formula = y ~ poly(x, 2), size = 1, se = F) +
  xlab("Change over time in merchandise and services trade\n(US$ dollars per capita)") +
  ylab("Country food system sustainability scores") +
  theme(legend.title = element_blank()) +
  theme_bw() +
  theme(axis.title   = element_text(size = 20),
        axis.text    = element_text(size = 15),
        legend.title = element_blank(),
        legend.text  = element_text(size = 15))

# Same for all
ggsave(filename = paste0("../SFS_indicators/old/_graphs/_relations_drivers_vs_SFS_index/relation_SFS_index_vs_", names(drivers_index)[i], "_removing_outliers_pos_poly.png"),
       plot = tsv,
       device = "png",
       units = "in",
       width = 8,
       height = 8)
