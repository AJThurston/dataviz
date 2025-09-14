library(ggplot2)
library(maps)
# install.packages("ggmap")
library(ggmap)

z = votes.repub[,"1900"]

states <- map_data("state")


usa <- ggplot() + geom_polygon(data = states, 
                        aes(x=long, y = lat, group = group),
                        fill = "#666666",
                        color = "black") + 
  coord_fixed(1.3) +
  theme_nothing()
usa


alaska <- ggplot(data = usa) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 

                                                                       
                                                                       
install.packages("statebins")
library(statebins)
data(USArrests)
                                                                       
USArrests$state <- rownames(USArrests)
statebins(USArrests, value_col="Assault", name = "Assault", round=TRUE) +
theme_statebins(legend_position="right")

install.packages("statebins", repos = "https://cinc.rud.is")

library(statebins)
library(cdcfluview)
library(hrbrthemes)
library(tidyverse)

adat <- read_csv(system.file("extdata", "wapostates.csv", package="statebins"))

mutate(
  adat, 
  share = cut(avgshare94_00, breaks = 4, labels = c("0-1", "1-2", "2-3", "3-4"))
) %>% 
  statebins(
    value_col = "share", 
    ggplot2_scale_function = scale_fill_brewer,
    name = "Share of workforce with jobs lost or threatened by trade"
  ) +
  labs(title = "1994-2000") +
  theme_statebins()


# ----

# getAnywhere(statbins)
getAnywhere(validate_states)
getAnywhere(merge.x)
state_coords <- getAnywhere(state_coords)

# validate_states <- function (state_data, state_col, merge.x, ignore_dups = FALSE) 
# {
#   good_states <- state_data[, state_col] %in% state_coords[, 
#                                                            merge.x]
#   if (any(!good_states)) {
#     invalid <- state_data[, state_col][which(!good_states)]
#     state_data <- state_data[which(good_states), ]
#     warning("Found invalid state values: ", invalid)
#   }
#   if (!ignore_dups) {
#     dups <- duplicated(state_data[, state_col])
#     if (any(dups)) {
#       state_data <- state_data[which(!dups), ]
#       warning("Removing duplicate state rows")
#     }
#   }
#   return(state_data)
# }



#rewriting the function to get the values on the map
statebins_values <- function (state_data, state_col = "state", value_col = "value", 
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
                                                 y = "row", label = "abbrev"), angle = 0, color = .sb_invert(gb$data[[1]]$fill, 
                                                                                                             dark_label, light_label, na_label), size = font_size)
  gg
}

statebins_values(USArrests, value_col="Assault", name = "Assault", round=TRUE)

geom_text(data = st.dat, aes_string(x = "col", 
                                    y = "row", label = value_col), angle = 0, color = .sb_invert(gb$data[[1]]$fill, 
                                                                                                dark_label, light_label, na_label), size = font_size)

fixInNamespace(statebins, pos="package:statebins")
library(statebins)


library(tidyverse)

mutate(
  adat, 
  share = cut(avgshare94_00, breaks = 4, labels = c("0-1", "1-2", "2-3", "3-4"))
) %>% 
  statebins(
    value_col = "share", 
    ggplot2_scale_function = scale_fill_brewer,
    name = "Share of workforce with jobs lost or threatened by trade"
  ) +
  labs(title = "1994-2000") +
  theme_statebins()

statebins_values <- statebins:::statebins
detach("package:statebins", unload = TRUE)
library("statebins")
trace(statebins, edit=TRUE)
trace(theme_statebins, edit = TRUE)

paste("abbrev",value_col,sep = "\n")



statebins(adat, value_col = "avgshare08_12", palette = "Purples") +
  labs(x="2008-2010") +
  theme_statebins(legend_position = "none") 



library(tidyverse)
library(statebins)
library(viridis)

trace(statebins, edit=TRUE)

# Copy/paste everything from this break to the next into the trace:
# ----
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
                                          na_label), size = font_size+2)
  gg <- gg + geom_text(data = st.dat, aes_string(x = "col", 
                                                 y = "row", label = value_col), vjust = 1.25, angle = 0, 
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, 
                                          na_label), size = font_size+2)
  gg
}


# ----

poprep2017$state <- poprep2017$State
statebins(poprep2017, value_col="Ratio", name = "Ratio", ggplot2_scale_function = viridis::scale_fill_viridis) +
  labs(title="Non-Prior Service (NPS) Active Component Enlisted Accessions\nRepresentation Ratio (2017)") +
  coord_equal(expand = FALSE) +
  theme_statebins(legend_position=c(0,.85)) +
  theme(
    legend.text=element_text(size=14), 
    legend.title = element_text(size = 14), 
    legend.direction = "horizontal",
    legend.key.height = unit(1,"cm"),
    legend.key.width = unit(1.5,"cm"))

ggsave(filename = "C:\\Users\\Owner\\Desktop\\poprep2017cartogram.png", dpi = 300, type = "cairo")
getwd()