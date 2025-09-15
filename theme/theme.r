theme_custom <- function(base_size = 16, base_family = "jetbrains") {
  
  library(ggplot2)
  library(showtext)
  
  # Add JetBrains Mono from Google Fonts
  font_add_google("JetBrains Mono", "jetbrains")
  showtext_auto()
  
  theme(
    text = element_text(size = base_size, family = base_family),
    plot.background = element_rect(fill = "#FFFFFC", color = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_blank()
  )
}
