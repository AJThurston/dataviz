suppressMessages({
  suppressWarnings({
    library(showtext)
    font_add_google("JetBrains Mono", "jetbrains")
    showtext_auto()
  })
})

theme_custom <- function(base_size = 16, base_family = "jetbrains") {
  list(  
    labs(caption = "@AJThurston"),
    theme(
      text = element_text(size = base_size, family = base_family),
      plot.background = element_rect(fill = "#FFFFFC", color = NA),
      plot.margin = unit(c(1,1,.5,.5), "cm"),
      plot.caption = element_text(color = "#CCCCCF", size = base_size/2, margin = margin(t = 25)),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(margin = margin(t = 10)),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.ticks = element_blank()
      )
  )
}