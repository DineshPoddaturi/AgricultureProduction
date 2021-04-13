rm(list=ls())
setwd("E:/Dropbox/Research/Covid/")

#########################################################################
##
##   Create gif of US covid deaths
##   Aaron Smith, 4/12/21
##   https://asmith.ucdavis.edu
##
#########################################################################



# If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed 
# from CRAN and then loaded.
pacman::p_load(tidyverse,lubridate,RColorBrewer,scales,gganimate)

# Read data
data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") 


df <- data %>%
  select(location,date,total_deaths_per_million,new_deaths_smoothed_per_million,new_deaths_per_million,new_cases_smoothed_per_million,total_cases_per_million,hosp_patients_per_million,people_vaccinated_per_hundred,people_fully_vaccinated_per_hundred) %>%
  mutate(date = ymd(date)) 
  
dmin<- "2020-02-01"
dmax<- "2021-04-19"

# Make US plot
plot_US_animate <- data %>%
  select(location,date,total_deaths,new_deaths_smoothed,new_deaths,new_cases_smoothed,total_cases,hosp_patients,people_vaccinated_per_hundred,people_fully_vaccinated_per_hundred) %>%
  mutate(date = ymd(date)) %>%
  filter(location %in% c("United States")) %>%
  filter(date>=as_date(dmin)&date<=as_date(dmax)) %>%
  rename(cases=new_cases_smoothed,deaths=new_deaths_smoothed,deathstoday=new_deaths,hospital=hosp_patients,vaccines=people_fully_vaccinated_per_hundred) %>%
  select(date,cases,hospital,deaths,deathstoday,vaccines) %>%
  mutate(deaths=deaths) %>%
  mutate(deathstoday=deathstoday) %>%
  mutate(hospital=hospital/50) %>%
  mutate(vaccines=vaccines*100) %>%
  pivot_longer(-date,names_to="variable") %>%
  mutate(date=as_date(ifelse(variable=="hospital",date-0,date))) %>%
  mutate(date=as_date(ifelse(variable=="deaths",date,date))) %>%
  pivot_wider(names_from = variable,values_from=value) %>%
  drop_na(deaths) %>%
  ggplot(aes(x = date))+
    geom_line(aes(y=`vaccines`),size=1.5,color="blue") +
    geom_line(aes(y=`deaths`),size=1.5,color="black") +
    geom_text(aes(label=paste0(floor(`vaccines`/10)/10,"% \n Fully Vaccinated"),x=date,y=`vaccines`+200), 
              position=position_nudge(0.1), color="blue", size=7, hjust=0.6, show.legend=FALSE) +
    geom_text(aes(label=sprintf("%1.0f",deaths),x=date,y=deaths), 
              position=position_nudge(0.1), color="black", size=8, hjust=-0.2, show.legend=FALSE) +
    transition_reveal(date) +
    labs(x = "Date")+
    ggtitle(paste0("US COVID-19 Daily Deaths"))+
    theme_minimal() +
    scale_y_continuous(name = "Ave. Daily Deaths",breaks= pretty_breaks(),limits = c(0, NA) ) +
    scale_x_date(labels = date_format("%b-%y")) +
    theme(axis.title.y = element_text(color = "black", size=24),
          axis.text.y=element_text(colour="black"),
          plot.title = element_text(hjust = 0.5,size=36),
          text = element_text(size=24))

animate(plot_US_animate,height = 500, width = 1000,fps=5,start_pause=5,end_pause=20)
anim_save("covid.gif")
