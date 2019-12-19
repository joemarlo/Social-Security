require(tidyverse)

#themes for ggplot
seashell.theme <- theme(legend.position = "none",
                        panel.grid.minor = element_line(color = NA),
                        panel.grid.major = element_line(color = "seashell"),
                        panel.background = element_rect(fill = "#f5e9e1"),
                        plot.background = element_rect(fill = "seashell",
                                                       color = NA),
                        axis.title = element_text(color = "gray30"),
                        axis.ticks = element_line(color = NA),
                        strip.background = element_rect(fill = NA),
                        strip.text = element_text(color = "gray30",
                                                  size = 11,
                                                  face = "bold"),
                        plot.title = element_text(color = "gray30",
                                                  face = "bold"),
                        plot.subtitle = element_text(size = 10),
                        text = element_text(family = "Courier"))

light.theme <- theme(legend.position = "none",
                     panel.grid.minor.y = element_line(color = NA),
                     panel.grid.major.y = element_line(color = "gray95"),
                     panel.grid.minor.x = element_line(color = NA),
                     panel.grid.major.x = element_line(color = NA),
                     panel.background = element_rect(fill = NA),
                     plot.background = element_rect(fill = NA,
                                                    color = "gray95",
                                                    size = 10),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title = element_text(color = "gray30"),
                     axis.ticks = element_line(color = NA),
                     strip.background = element_rect(fill = "gray95"),
                     strip.text = element_text(color = "gray30",
                                               size = 11,
                                               face = "bold"),
                     plot.title = element_text(color = "gray30",
                                               face = "bold"),
                     plot.subtitle = element_text(size = 10,
                                                  color = "gray30"),
                     text = element_text(family = "Helvetica"))

# notes -------------------------------------------------------------------

# green color is #2b7551

# ggsave(filename = "LRC_winners.svg",
#        plot = last_plot(),
#        path = project.path,
#        device = "svg",
#        width = 9,
#        height = 5)