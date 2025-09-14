library(ggplot2)
library(tidyr)

# I love this colors

d=data.frame(xmin=c(1,1,1,1), 
             xmax=c(6,6,6,6), 
             ymin=c(1,2,3,4), 
             ymax=c(2,3,4,5), 
             t=c('#333333',"#336666",'#339999','#33cccc'))
# within(d,l <- paste("Background:",t," Text: ",rev(t),sep=''))

p = ggplot() + 
  scale_x_continuous(name=NULL, expand = c(-1,0)) + 
  scale_y_continuous(name=NULL, expand = c(-1,0)) +
  geom_rect(data=d, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=d$t) +
  scale_color_identity() +
  geom_text(data=d, aes(x=xmin+(xmax-xmin)/2, y=ymin+(ymax-ymin)/2, label=t), size=8, color = rev(d$t)) +
  theme_void() + 
  theme(legend.position="none")
p

setwd("C:\\Users\\Owner\\Desktop")

ggsave("plot.png",plot=p, width = 2, height = 1, units = "in", dpi = 512,bg = "transparent") # 1024 x 512

# also these colors for improvement/decrements respectively #99ffcc, #ff9999
