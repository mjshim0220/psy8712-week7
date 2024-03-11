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
  mutate(timeSpent=difftime(timeEnd, timeStart, unit="min")) %>% 
  mutate(timeSpent=as.numeric(timeSpent))



#Visualization
week7_tbl %>% 
  select(q1:q10) %>% 
  ggpairs()

(ggplot(week7_tbl, aes(x=timeStart, y=q1))+
  geom_point()+
  labs(x="Date of Experiment", y="Q1 Score")) %>% 
  ggsave(filename="../figs/fig1.png", width=6, height=3)

(ggplot(week7_tbl,aes(x=q1, y=q2, color=gender))+
    geom_jitter()+
    labs(color="Participant Gender")) %>% 
  ggsave(filename="../figs/fig2.png", width=6, height=3)

(ggplot(week7_tbl,aes(x=q1, y=q2))+
    geom_jitter()+
    facet_grid(~gender)+
    labs(color="Participant Gender", x="Score on Q1", y="Score on Q2")) %>%
  ggsave(filename="../figs/fig3.png", width=8, height=4)


(ggplot(week7_tbl, aes(x=gender, y=timeSpent))+
    geom_boxplot()+
    labs(x="Gender", y="Time Elapsed (mins)")) %>% 
  ggsave(filename="../figs/fig4.png", width=8, height=4)


(ggplot(week7_tbl, aes(x=q5, y=q7, color=condition))+
  geom_jitter()+
  geom_smooth(method="lm", se=F)+
  labs(x="Score on Q5", y="Score on Q7", color="Experimental Condition")+
  theme(legend.position = "bottom", 
        legend.background = element_rect(fill="lightgray"))) %>% 
  ggsave(filename="../figs/fig5.png", width=8, height=4)

  