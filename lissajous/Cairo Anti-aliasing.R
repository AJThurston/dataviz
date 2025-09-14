library(tidyverse)
library(reshape2)
library(Cairo)
library(ggplot2)

# t <- as.data.frame(seq(0,360,1))
# y <- as.data.frame(sin(t))
# z <- as.data.frame(cos(t))
# 
# dat <-  as.data.frame(c(x,y,z))
# colnames(dat) <- c("t","sin","cos")
# rm(x,y,z)
# # dat <- melt(dat, id.vars = "sin")
# 
# p <- ggplot(dat, aes(t,cos)) +
#      geom_line(size=4) +
#      theme_void()
# p
# 
# ggsave("alias.png", plot = p, height = 3, width = 3, units = "in", dpi = 150)
# ggsave("antialias.png", plot = p, height = 3, width = 3, units = "in", dpi = 150, type ="cairo-png")

tt <- seq(0,2*pi,length.out = 360)


circle <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(7*tt)
  yy <- center[2] + r * sin(6*tt)
  return(data.frame(x = xx, y = yy))
}

dat <- circle(c(1,-1),2.3,npoints = 100)
#geom_path will do open circles, geom_polygon will do filled circles
q <- ggplot(dat,aes(x,y)) + 
     geom_path(size=1, color="#336666") +
     theme_void()
q

ggsave("alias.png", plot = q, height = 3, width = 3, units = "in", dpi = 150)
ggsave("antialias.png", plot = q, height = 3, width = 3, units = "in", dpi = 150, type ="cairo-png")

ggsave("alias1x2.png", plot = q, height = 3, width = 6, units = "in", dpi = 150)
ggsave("antialias1x2.png", plot = q, height = 3, width = 6, units = "in", dpi = 150, type ="cairo-png")

