library(xlsx)
library(reshape)
library(ggplot2)
library(formattable)
windowsFonts(Times=windowsFont("TT Times New Roman"))

RIASEC = as.data.frame(
  read.xlsx("C:/Users/AJ Thurston/Desktop/RIASEC data.xlsx", 
            sheetName = "Sheet1"))
RIASEC_Long = melt(data=RIASEC, id="Participant")
RIASEC_Long <- rename(RIASEC_Long, c("variable"="Variable", "value"="Score"))

RIASECplot = ggplot(RIASEC_Long,aes(Variable,Score)) +
geom_jitter(width = 0.3, aes(colour = Variable)) +
  scale_y_continuous(name="Score", limits = c(10,50)) +
  
  theme(
    text = element_text(size = 14, family = "Times"),
    axis.title.y = element_text(face = "bold", size = 14, angle = 90),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, face = "bold", hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.ticks = element_blank(),
    legend.position="none",
    panel.background = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")
  )
RIASECplot

ggsave("C:/Users/AJ Thurston/Desktop/RIASEC.png", 
       plot = RIASECplot, 
       
       scale = 1, 
       width = 6.5, 
       height = 4, 
       units = "in",
       dpi = 300)

