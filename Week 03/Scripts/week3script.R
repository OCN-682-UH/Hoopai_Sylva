#week 3 homework, making a penguin plot

#all the packages used
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
library(here)


#making boxplot
hwplot1<-ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species))+
  geom_boxplot(lwd=1.2)+
  geom_point(alpha=.5)+
  theme_classic(base_size=8)+
  xlab("Bill Depth (mm)")+ylab("Bill Length (mm)")+
  scale_color_manual(values=c("red2","yellow3","green3"))+
  scale_x_continuous(breaks=seq(15,22,2))

ggsave(here("Week 03","Output","penguin.png"))


  
         



