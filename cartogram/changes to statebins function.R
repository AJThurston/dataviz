function (state_data, state_col = "state", value_col = "value", 
          dark_label = "black", light_label = "white", na_label = "white", 
          font_size = 3, state_border_col = "white", state_border_size = 2, 
          round = FALSE, radius = grid::unit(6, "pt"), ggplot2_scale_function = ggplot2::scale_fill_distiller, 
          ...) 
{
  state_data <- data.frame(state_data, stringsAsFactors = FALSE)
  if (max(nchar(state_data[, state_col])) <= 3) {
    merge.x <- "abbrev"
  }
  else {
    merge.x <- "state"
  }
  state_data <- validate_states(state_data, state_col, merge.x)
  st.dat <- merge(state_coords, state_data, by.x = merge.x, 
                  by.y = state_col, all.y = TRUE, sort = TRUE)
  gg <- ggplot()
  if (round) {
    gg <- gg + geom_rtile(data = st.dat, radius = radius, 
                          aes_string(x = "col", y = "row", fill = value_col), 
                          color = state_border_col, size = state_border_size)
  }
  else {
    gg <- gg + geom_tile(data = st.dat, aes_string(x = "col", 
                                                   y = "row", fill = value_col), color = state_border_col, 
                         size = state_border_size)
  }
  gg <- gg + scale_y_reverse()
  gg <- gg + ggplot2_scale_function(...)
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL)
  gb <- ggplot2::ggplot_build(gg)
  gg <- gg + geom_text(data = st.dat, aes_string(x = "col", 
                                                 y = "row", label = "abbrev"), vjust = -.25, angle = 0, 
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, 
                                          na_label), size = font_size+1)
  gg <- gg + geom_text(data = st.dat, aes_string(x = "col", 
                                                 y = "row", label = value_col), vjust = 1.25, angle = 0, 
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, 
                                          na_label), size = font_size+1)
  gg
}
