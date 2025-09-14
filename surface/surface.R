library(plotly)
library(tidyr)
library(reshape)

#coefficients
INT = 5.825
Xco = 0.197
Yco = -0.293
XSQ = -0.056
X.Y = 0.276
YSQ = 0.035

#X and Y z-values for simulation
x = c(-30:30)/10
y = c(-30:30)/10

function.z <- function(x,y) 
{
  INT+(Xco*x)+(Yco*y)+(XSQ*(x^2))+(X.Y*(x*y))+(YSQ*(y^2))
}

data = outer(x,y,"function.z")
row.names(data) = x
colnames(data) = y

data2=melt(data)
colnames(data2) = c("x","y","z")

#data = as.data.frame(data)
#data = mapply(data, MARGIN=c(1,2), func)

p <- plot_ly(z = ~data) %>% 
  add_surface()
p

#ds <- diamonds[sample(1:nrow(diamonds), size = 1000),]

f1 <- list(
  size = 14,
  color = "black")

f2 <- list(
  size = 14,
  color = "black")

axx <- list(
  titlefont = f1,
  tickfont = f2,
  title='Mentor Conscientiousness',
  nticks = 7,
  range = c(-3,3)
)

axy <- list(
  titlefont = f1,
  tickfont = f2,
  title='Mentee Conscientiousness',
  nticks = 7,
  range = c(-3,3)
)

axz <- list(
  titlefont = f1,
  tickfont = f2,
  title='Perceived Relationship Quality',
  nticks = 10,
  range = c(1,9)
)

#p <- plot_ly() %>% 
#  add_surface(z = ~data) %>%
#  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
#p

#volcano

y <- x <- as.numeric(rownames(data))
# More rows than column, and the x and y should have the same size

p <- plot_ly(z = data, x= x, y=y, type = "surface") %>%
  layout(
    scene=list(
      xaxis=axx,
      yaxis=axy,
      zaxis=axz,
      camera = list(eye = list(x = -1.5, y = -1.5, z = 1.5))))
p
