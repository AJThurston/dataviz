library(sf)
library(ggplot2)

circle_poly <- function(cx, cy, r, n = 720) {
  theta <- seq(0, 2*pi, length.out = n)
  x <- cx + r * cos(theta)
  y <- cy + r * sin(theta)
  pts <- cbind(x, y)
  pts <- rbind(pts, pts[1, ])  # close polygon
  st_polygon(list(pts))
}

# 1) Four circles (y, x1, x2, x3)
y_sf  <- st_sf(set = "y",  geometry = st_sfc(circle_poly(0,    0.0, 1)))
x1_sf <- st_sf(set = "x1", geometry = st_sfc(circle_poly(-.8, -.8, 1)))
x2_sf <- st_sf(set = "x2", geometry = st_sfc(circle_poly(0,   -.8, 1)))
x3_sf <- st_sf(set = "x3", geometry = st_sfc(circle_poly(.8, -.8, 1)))

# 2) ALL disjoint regions

# y-only, x1-only, x2-only, x3-only
y_only  <- st_difference(y_sf,  st_union(x1_sf, x2_sf, x3_sf))
x1_only <- st_difference(x1_sf, st_union(y_sf,  x2_sf, x3_sf))
x2_only <- st_difference(x2_sf, st_union(y_sf,  x1_sf, x3_sf))
x3_only <- st_difference(x3_sf, st_union(y_sf,  x1_sf, x2_sf))

# Pairwise-only
y_x1_only  <- st_difference(st_intersection(y_sf,  x1_sf), st_union(x2_sf, x3_sf))
y_x2_only  <- st_difference(st_intersection(y_sf,  x2_sf), st_union(x1_sf, x3_sf))
y_x3_only  <- st_difference(st_intersection(y_sf,  x3_sf), st_union(x1_sf, x2_sf))
x1_x2_only <- st_difference(st_intersection(x1_sf, x2_sf), st_union(y_sf,  x3_sf))
x1_x3_only <- st_difference(st_intersection(x1_sf, x3_sf), st_union(y_sf,  x2_sf))
x2_x3_only <- st_difference(st_intersection(x2_sf, x3_sf), st_union(y_sf,  x1_sf))

# Triple-only
y_x1_x2_only  <- st_difference(st_intersection(st_intersection(y_sf,  x1_sf), x2_sf), x3_sf)
y_x1_x3_only  <- st_difference(st_intersection(st_intersection(y_sf,  x1_sf), x3_sf), x2_sf)
y_x2_x3_only  <- st_difference(st_intersection(st_intersection(y_sf,  x2_sf), x3_sf), x1_sf)
x1_x2_x3_only <- st_difference(st_intersection(st_intersection(x1_sf, x2_sf), x3_sf), y_sf)

# Four-way
y_x1_x2_x3 <- st_intersection(st_intersection(st_intersection(y_sf, x1_sf), x2_sf), x3_sf)

# 3) Plot (CMY theory; y = white)
ggplot() +
  geom_sf(data = y_only,         fill = "#CCCCCC", alpha = .5, color = NA) +
  geom_sf(data = x1_only,        fill = "#00FFFF", alpha = .5, color = NA) + # cyan
  geom_sf(data = x2_only,        fill = "#FF00FF", alpha = .5, color = NA) + # magenta
  geom_sf(data = x3_only,        fill = "#FFFF00", alpha = .5, color = NA) + # yellow
  
  geom_sf(data = y_x1_only,      fill = "#00FFFF", alpha = 1, color = NA) +
  geom_sf(data = y_x2_only,      fill = "#FF00FF", alpha = 1, color = NA) +
  geom_sf(data = y_x3_only,      fill = "#FFFF00", alpha = 1, color = NA) +
  
  geom_sf(data = x1_x2_only,     fill = "#0000FF", alpha = .5, color = NA) + # C+M = blue
  geom_sf(data = x1_x3_only,     fill = "#00FF00", alpha = .5, color = NA) + # C+Y = green
  geom_sf(data = x2_x3_only,     fill = "#FF0000", alpha = .5, color = NA) + # M+Y = red
  
  geom_sf(data = y_x1_x2_only,   fill = "#0000FF", alpha = 1, color = NA) +
  geom_sf(data = y_x1_x3_only,   fill = "#00FF00", alpha = 1, color = NA) +
  geom_sf(data = y_x2_x3_only,   fill = "#FF0000", alpha = 1, color = NA) +
  
  geom_sf(data = x1_x2_x3_only,  fill = "#000000", alpha = .5, color = NA) + # C+M+Y = black
  geom_sf(data = y_x1_x2_x3,     fill = "#000000", alpha = 1, color = NA) + # white doesn't change it
  
  geom_sf(data = y_sf,  fill = NA, color = "#000000", linewidth = 0.8) +
  geom_sf(data = x1_sf, fill = NA, color = "#000000", linewidth = 0.8) +
  geom_sf(data = x2_sf, fill = NA, color = "#000000", linewidth = 0.8) +
  geom_sf(data = x3_sf, fill = NA, color = "#000000", linewidth = 0.8) +
  coord_sf(expand = FALSE) +
  theme_void()
