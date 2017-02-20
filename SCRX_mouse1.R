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

# drop day 0 and modify col names for day to be numeric
mice <- mice[,-2]
colnames(mice) <- c('group', '1', '3', '6')

# create factor for dose radionuclide
agent <- cut(as.numeric(mice$group), breaks = c(0,5,9,10,11),
             labels = c('Ac225','Lu177','Chelator','Sentinal'))
mice <- cbind(mice, agent)

# melt the data to plot 
mmice <- melt(mice, id = c("group", "agent"))

ggplot(mmice, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group)) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight", fill = "Group") +
  scale_x_discrete(limits=1:6) +
  facet_wrap(~agent)
  
# replot as a change in weight, take difference and melt
dmice <- mice
for(i in seq(ncol(dmice)-2)){
  dmice[,i+1] = dmice[,i+1] - mice[,2]
}
mdmice <- melt(dmice, id = c("group", "agent"))

ggplot(mdmice, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group)) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group, group = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight Change", fill = "Group") +
  scale_x_discrete(limits=1:6) +
  facet_wrap(~agent)
