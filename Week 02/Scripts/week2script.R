## this code is for week 2 hw of MBIO 612
## created by Hanalei on 2024-09-03

##library loader
library(tidyverse)
library(here)

## read in da data
weightdata<-read_csv(here("Week 02","Data","weightdata.csv"))

## data analysis
head(weightdata) #looks at top of df
tail(weightdata) #looks at bottom of df

