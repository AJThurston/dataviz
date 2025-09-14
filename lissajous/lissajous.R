library(tidyverse)
library(reshape2)
library(Cairo)
library(ggplot2)

tt <- seq(0,2*pi,length.out = 360)

# plotting a circle taken from here: https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# adapted to plot the sign and cosign of values to generate 

x <- c(0:7)
y <- c(0:7)
values <- expand.grid(x,y)
colnames(values) <- c("x","y")

circle <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(1*tt)
  yy <- center[2] + r * sin(1*tt)
  return(data.frame(x = xx, y = yy))
}

ddat <- as.list(rep("", 64)) 
for(i in 1:length(values)) { 
  ddat[[i]] <- data.frame(ivec = 1:i) 
  dat <- circle(c(1,-1),2.3,npoints = 100)
} 

dat <- circle(c(1,-1),2.3,npoints = 100)

q <- ggplot(dat,aes(x,y)) + 
     geom_path(size=1, color="#336666") +
     theme_void()
q

# ggsave("alias.png", plot = q, height = 3, width = 3, units = "in", dpi = 150)
# ggsave("antialias.png", plot = q, height = 3, width = 3, units = "in", dpi = 150, type ="cairo-png")

