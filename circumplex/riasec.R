library(ggplot2)
library(tidyverse)
library(ggforce)
# source("https://ajthurston.com/theme")


okabeito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")



riasec <- data.frame(
  code = c("R", "I", "A", "S", "E", "C"),
  label = c("Realistic", "Investigative", "Artistic",
            "Social", "Enterprising", "Conventional"),
  angle = seq(0, 300, by = 60) # 360 degrees / 6
)

riasec <- riasec %>%
  mutate(
    angle_rad = (angle - 90) * pi/180,  # rotate so R starts at top
    x = cos(angle_rad),
    y = sin(angle_rad)*-1
  )

riasec_closed <- rbind(riasec, riasec[1,])

# Circumplex plot
p <- ggplot(riasec_closed, aes(x, y)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "#666666") +
  geom_polygon(fill = NA, color = "#666666") +
  annotate("segment",
           x = -1.25, y = 1.25/sqrt(3),
           xend = 1.25, yend = -1.25/sqrt(3),
           arrow = arrow(length = unit(0.15, "cm"), type = "closed", ends = "both"),
           color = "#0072B2"
           )+
  annotate("segment",
           x = -1.25 / sqrt(3),
           y = -1.25,
           xend = 1.25 / sqrt(3),
           yend = 1.25,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed", ends = "both"),
           color = "#D55E00"
  )+
  annotate("segment",
           x = -1.4,
           y = 0,
           xend = 1.4,
           yend = 0,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed", ends = "both"), color = "#666666"
  )+
  annotate("segment",
           y = -1.4,
           x = 0,
           yend = 1.4,
           xend = 0,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed", ends = "both"), color = "#666666"
  )+
  annotate("text",
           y = 1.5,
           x = 0,
           label = "Things",
           size = 9,
           family = "jetbrains", color = "#666666"
           
  )+
  annotate("text",
           y = -1.5,
           x = 0,
           label = "People",
           size = 9,
           family = "jetbrains", color = "#666666"
           
  )+
  annotate("text",
           y = 0,
           x = -1.5,
           label = "Data",
           size = 9,
           family = "jetbrains",
           hjust  = 1, color = "#666666"
           
  )+
  annotate("text",
           y = 0,
           x = 1.5,
           label = "Ideas",
           size = 9,
           family = "jetbrains",
           hjust = 0, color = "#666666"
           
  )+
  annotate("text",
           x = -1.25, y = 1.4/sqrt(3),
           label = "High\nConformity",
           size = 9,
           family = "jetbrains",
           vjust = 0,
           color = "#0072B2",
           lineheight = 0.33
           
  )+
  annotate("text",
           x = 1.4, y = -1.3/sqrt(3),
           label = "Low\nConformity",
           size = 9,
           family = "jetbrains",
           vjust = 1,
           color = "#0072B2",
           lineheight = 0.33
           
  )+
  annotate("text",
           x = -1, y = -1.35,
           label = "High\nSociability",
           size = 9,
           family = "jetbrains",
           color = "#D55E00",
           lineheight = 0.33
           
  )+
  annotate("text",
           x = 1, y = 1.5,
           label = "Low\nSociability",
           size = 9,
           family = "jetbrains",
           color = "#D55E00",
           lineheight = 0.33
           
  )+
  scale_y_continuous(limits = c(-1.75,1.75)) +
  scale_x_continuous(limits = c(-1.75,1.75)) +
  geom_point(size = 5, color = "#666666") +
  geom_text(aes(label = code), size = 9, color = "#FFFFFC", family = "jetbrains") +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFC", color = NA)
  )

ggsave(
  plot = p,
  file = "~/Desktop/riasec.png",
  units = "in",
  width = 3,
  height = 3,
  dpi = 300
)

