#Hanalei Hoʻopai-sylva
#9-24-24

# the packages needed
library(tidyverse)
library(here)
library(cowplot)
library(lubridate)

# The Challenge #
CondData<-read_csv(here("Week 05","Data", "CondData.csv")) # read in CondData csv

GoodDates<-CondData%>% # make a new df
  mutate(Date = mdy_hms(date))%>% # change into standard datetime
  select(-date) #get rid of old date column

# Homework # 
DepthData<-read_csv(here("Week 05","Data", "DepthData.csv")) # read in Depth data csv  

CnD<-GoodDates%>%
  mutate(date = round_date(Date,"10 mins"))%>% #round up to 10 min
  select(-Date)%>% #get rid of old "Date" column
  left_join(.,DepthData)%>% # join two datasets together
  drop_na()%>% # drop NAs
  select(date, everything())%>% # move "date" column in front
  pivot_longer(cols = Temperature:Depth, # the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", # the names of the new cols with all the column names 
               values_to = "Values")%>%
  group_by(date, Variables)%>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE))%>% # find ave of values and call new column "mean_vals"
  filter(Variables!="Depth")%>%filter(Variables!="AbsPressure")%>%filter(Variables!="Serial")%>% #filter out unneeded columns
  pivot_wider(names_from = Variables,
              values_from = mean_vals)%>% #change back into wide data to be able to plot both columns
  write_csv(here("Week 05","Output","Cond+Depth.csv"))%>% #save dataframe
  ggplot(aes(date))+ # make only "date" column as x-axis
  geom_smooth(aes(y=Salinity), method="loess", col="blue") + # y-axis 1
  geom_smooth(aes(y=Temperature), method="loess", col="red")+ #y-axis 2
  scale_y_continuous(name="Salinity", sec.axis=sec_axis(~., name="Temperature (C)"))+ # make two different axis scales for two different variables
  theme_light(base_size=8)+ # aesthetic changes
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank());CnD
    ggsave(here("Week 05","Output","Salinity+Temp.png")) #save figure
    
    



  




           