library(ggplot2)
library(dplyr)
library(showtext)

# Load JetBrains Mono (Google font)
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()

# Data
dat <- data.frame(
  Sex = c("Female","Female","Male","Male"),
  Veteran = c("No","Yes","No","Yes"),
  Count = c(93,16,197,17)
)

# make Veteran a factor so levels are stable
dat <- dat |>
  mutate(Veteran = factor(Veteran, levels = unique(Veteran))) |>
  mutate(prop = Count / sum(Count),
         xpos = as.numeric(Veteran),
         xshift = xpos - 0.5 + (prop * 0.98) / 2)

ggplot(dat, aes(x = xpos, y = Sex)) +
  geom_tile(aes(x = xshift, width = prop , height = 0.5),
            fill = "#FDB515", alpha = 1) +
  geom_tile(aes(width = 1, height = 1),
            color = "black", fill = NA, linewidth = .5) +
  geom_text(aes(label = Count, x = xpos),
            family = "jetbrains", size = 6) +
  
  # relabel x axis dynamically
  scale_x_continuous(
    breaks = seq_along(levels(dat[[2]])),
    labels = levels(dat[[2]]),
    position = "top"
  ) +
  
  labs(x = names(dat)[2], y = names(dat)[1]) +
  
  coord_equal() +
  theme_minimal(base_size = 16, base_family = "jetbrains") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(angle = 0, vjust = .5),
    axis.text.x = element_text(size = 16, vjust = -0.5, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.ticks = element_blank()
  )
