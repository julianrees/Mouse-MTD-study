#library(drc)
library(drfit)
library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)

mice <- read.xlsx("../Data/testMice.xlsx", header = TRUE, sheetIndex = 1)
mice <- mice[,-1]
mice <- mice[, -ncol(mice)]

# remove premature casualties
mice <- mice[-36,]

# calculate the doses that each mouse received based on the dosing sheet
day0dose <- cut(mice$day0weight, breaks = 15.8+1.5*0:7, 
                labels = 0.1+1:7/100)
day1dose <- cut(mice$day1weight, breaks = 15.8+1.5*0:7, 
                labels = 0.1+1:7/100)
mice_dose <- cbind(mice[,1:3], day0dose, day1dose)

# create factor for dose radionuclide
agent <- cut(as.numeric(mice$group), breaks = c(0,5,9,10,11),
             labels = c('Ac225','Lu177','Chelator','Sentinal'))
mice <- cbind(mice, agent)


mmice <- melt(mice, id = c("group", "agent"))

ggplot(mmice, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group)) + 
  theme_bw() +
  facet_wrap(~agent)
  
