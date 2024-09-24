#Hanalei Hoopai-Sylva
# 2024-09-17
#learning data wrangling and tidyverse functions

#library loader
library(palmerpenguins)
library(tidyverse)
library(here)
library(cowplot)

#analysis
glimpse(penguins)

# homework #1 analysis - mean & variance of body mass by species, island, sex + drop NAs

#part 1.1
hw1 <- penguins%>%
  drop_na(species, island, sex)%>% #drop NAs in columns
  group_by(species, island, sex)%>% #group by these columns
  summarise(average_body_mass = mean(body_mass_g),
            body_mass_variance = var(body_mass_g))%>% #find average body mass and variance
  write.csv(here("Week 04","Data","ave+variance_of_bodymass.csv")) #save um



#part 1.2 - comparing log body mass of bird species by island
hw2 <- penguins%>%
  filter(sex!="male")%>% #filter out all "males" in dataset
  mutate(log_body_mass = log(body_mass_g))%>% #find log body mass, make a new column of it
  select(species, island, sex, log_body_mass)%>% #select only these columns moving forward
  ggplot(.)+
  geom_boxplot(aes(island,log_body_mass, color=island),size=1)+
  geom_point(alpha=0.3,aes(island,log_body_mass, color=island),size=1)+
  facet_wrap(~species,ncol=3)+ 
  theme_light(base_size=8)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8))+
  xlab("")+
  ylab("log Body Mass")+
  ggtitle("Log Body Mass between Bird Species")+
  coord_cartesian(ylim = c (7.85,8.6))+
  scale_color_manual(values=c("#E89F17A1","#F06C60","#E0AAC8E6"),labels=c("Biscoe","Dream","Torgersen"),name="Island")+
  ggsave(here("Week 04","Output","log_body_mass.png")) #save da fig

# homework 2 analysis - reading in chem data, learning new functions, makin figs

ChemData<-read_csv(here("Week 04","Data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

ChemData_clean<-ChemData%>%
  drop_na()%>% #filters out everything that is not a complete row
  separate(col = Tide_time, # choose the tide time col
           into = c("Tide","Time"), # separate it into two columns Tide and time
           sep = "_", # separate by _
           remove = FALSE)%>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", # the names of the new cols with all the column names 
               values_to = "Values")%>% # names of the new column with all the values 
  group_by(Variables, Site, Time)%>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE))%>% #find mean values of variables
  filter(Site!="BP")%>% #get rid of site ʻBPʻ in dataset
  write_csv(here("Week 04","Data","chemdata_homework.csv"))%>% #save um
  ggplot(.)+
  geom_col(aes(Time,mean_vals,fill=Time))+ 
  facet_wrap(~Variables, scales = "free")+
  theme_light(base_size=8)+
  scale_fill_brewer(palette = "Set2")+
  xlab("Time")+
  ylab("Average Values");ChemData_clean+
  ggsave(here("Week 04","Output","ChemFacetFig.png")) #save da fig
  



