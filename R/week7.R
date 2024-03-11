#cript Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(readr)
library(tidyverse)
library(GGally)

#Data Import and Cleaning
week7_tbl<-read.csv("../data/week3.csv") %>% 
  mutate(timeStart=ymd_hms(timeStart),
         timeEnd=ymd_hms(timeEnd),
         condition=factor(condition, level=c("A","B","C"), labels=c("Block A", "Block B", "control")),
         gender=factor(gender, levels=c("M","F"), labels=c("Male", "Female"))) %>% 
  filter(q6==1) %>%
  select(-q6) %>% 
  mutate(timeSpent=difftime(timeEnd, timeStart, unit="min"))




#Visualization
week7_tbl %>% 
  select(q1:q10) %>% 
  ggpairs()

(week7_tbl %>% 
  ggplot(aes(x=timeStart, y=q1))+
  geom_point()+
  labs(x="Date of Experiment", y="Q1 Score")) %>% 
  ggsave(filename="../figs/fig1.png", width=6, height=3)

(week7_tbl %>% 
    ggplot(aes(x=q1, y=q2, color=gender))+
    geom_jitter()+
    labs(color="Participant Gender")) %>% 
  ggsave(filename="../figs/fig2.png", width=6, height=3)



