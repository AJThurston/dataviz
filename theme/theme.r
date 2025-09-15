suppressMessages({
  suppressWarnings({
    library(showtext)
    font_add_google("JetBrains Mono", "jetbrains")
    showtext_auto()
  })
})

theme_custom <- function(base_size = 12, base_family = "jetbrains") {
  theme(
    text = element_text(size = base_size, family = base_family),
    plot.background = element_rect(fill = "#FBFBFB", color = NA),
    plot.margin = unit(c(1,1,.5,.5), "cm"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_blank()
  )
}