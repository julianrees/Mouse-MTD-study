library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)

# import the data
mice <- read.xlsx("../Data/SCRXmouse1Weights18.xlsx", header = TRUE, sheetIndex = 1)

# convert row names to mouse number and convert mouse number column to factor
rownames(mice) <- mice$Mouse..
mice$Mouse.. <- as.factor(rownames(mice))

# calculate the doses that each mouse received based on the dosing sheet
day0dose <- cut(mice$Day.0, breaks = 15.8+1.5*0:7, 
                labels = 0.1+1:7/100)
day1dose <- cut(mice$Day.1, breaks = 15.8+1.5*0:7, 
                labels = 0.1+1:7/100)
mice_dose <- cbind(mice[,1:3], day0dose, day1dose)

# modify col names for day to be numeric
colnames(mice) <- c('group', 'mousenum', 
                    unlist(strsplit(colnames(mice)[3:ncol(mice)], '.', fixed = TRUE))[1:(ncol(mice)-2)*2])

# create factor for dose radionuclide
agent <- cut(as.numeric(mice$group), breaks = c(0,5,9,10,11),
             labels = c('Ac225','Lu177','Chelator','Sentinal'))
mice <- cbind(mice, agent)

# remove the sentinals from the data
mice <- mice[which(mice$agent != 'Sentinal'), ]

# compute the differences from day 0
dmice0 <- mice
refday <- 0
for(i in seq(ncol(dmice0)-3)){
  dmice0[,i+2] = dmice0[,i+2] - mice[,3+refday]
}

# compute the differences from day 1
dmice1 <- mice
refday <- 1
for(i in seq(ncol(dmice1)-3)){
  dmice1[,i+2] = dmice1[,i+2] - mice[,3+refday]
}

# compute the differences as % weight from day 0
dmice0 <- mice
refday <- 0
for(i in seq(ncol(dmice0)-3)){
  dmice0[,i+2] = (dmice0[,i+2] - mice[,3+refday])*(100/mice[,3+refday])
}


# compute the differences as % weight from day 1
dmice1 <- mice
refday <- 1
for(i in seq(ncol(dmice1)-3)){
  dmice1[,i+2] = (dmice1[,i+2] - mice[,3+refday])*(100/mice[,3+refday])
}




# trim day 0 data
dmice1$`0` <- NULL

#---------- END OF DATA MANIPULATION !-!-! PLOTTING STARTS ----------------
w = 0.65
fwid = 9
fhei = 6
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#------ Plots of weight vs group by day -------

dat <- melt(mice, id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), width = w) + guides(color=FALSE) + 
  geom_boxplot(aes(fill = variable), width = w, outlier.color = NA) + 
  scale_x_discrete() +
  ggtitle('Statistical Mouse Weights') +
  labs(x = "Group", y = "Weight (g)", fill = "Day") + 
  ggsave(filename = 'Rfigs/weight_byDay.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dcast(dat, group ~ variable, mean), id = 'group')
ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_bar(aes(fill = variable), position = 'dodge', stat = 'identity', width = w) + 
  scale_x_discrete() +
  ggtitle('Average Mouse Weights') +
  labs(x = "Group", y = "Weight (g) ", fill = "Day") + 
  ggsave(filename = 'Rfigs/weight_byDay_mean.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice0, id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), width = w) + guides(color=FALSE) + 
  geom_boxplot(aes(fill = variable), width = w, outlier.color = NA) + 
  scale_x_discrete() +
  ggtitle('Statistical Mouse Weights') +
  labs(x = "Group", y = "Weight Change from Day 0 (%)", fill = "Day") + 
  ggsave(filename = 'Rfigs/weightDiff0_byDay.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dcast(dat, group ~ variable, mean), id = 'group')
ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_bar(aes(fill = variable), position = 'dodge', stat = 'identity', width = w) + 
  scale_x_discrete() +
  ggtitle('Average Mouse Weights') +
  labs(x = "Group", y = "Weight Change from Day 0 (%)", fill = "Day") + 
  ggsave(filename = 'Rfigs/weightDiff0_byDay_mean.png', 
         width = fwid, height = fhei, units = "in")


dat <- melt(dmice1, id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), width = w) + guides(color=FALSE) + 
  geom_boxplot(aes(fill = variable), width = w, outlier.color = NA) + 
  scale_x_discrete() +
  ggtitle('Statistical Mouse Weights') +
  labs(x = "Group", y = "Weight Change from Day 1 (%)", fill = "Day") + 
  ggsave(filename = 'Rfigs/weightDiff1_byDay.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dcast(dat, group ~ variable, mean), id = 'group')
ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_bar(aes(fill = variable), position = 'dodge', stat = 'identity', width = w) + 
  scale_x_discrete() +
  ggtitle('Average Mouse Weights') +
  labs(x = "Group", y = "Weight Change from Day 1 (%)", fill = "Day") + 
  ggsave(filename = 'Rfigs/weightDiff1_byDay_mean.png', 
         width = fwid, height = fhei, units = "in")


#------- Plots of Weight vs Day by group ----------

dat <- melt(mice, id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  ggtitle('Statistical Mouse Trajectories') +
  labs(x = "Day", y = "Weight", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~agent) + 
  ggsave(filename = 'Rfigs/weight_byGroup.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice0, id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) +
  ggtitle('Statistical Mouse Trajectories') +
  labs(x = "Day", y = "Weight Change from Day 0 (%)", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~agent) + 
  ggsave(filename = 'Rfigs/weightDiff0_byGroup.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice1, id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  ggtitle('Statistical Mouse Trajectories') +
  labs(x = "Day", y = "Weight Change from Day 1 (%)", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~agent) + 
  ggsave(filename = 'Rfigs/weightDiff1_byGroup.png', 
         width = fwid, height = fhei, units = "in")

#---------- Plots of Weight vs Day by Group - per Agent ------------

dat <- melt(dmice0[ which(dmice0$agent == 'Ac225' | dmice0$agent == 'Sentinal' | dmice0$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  ggtitle('Statistical Mouse Trajectories, Ac225 + Control') +
  labs(x = "Day", y = "Weight Change from Day 0 (%)", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) + 
  ggsave(filename = 'Rfigs/weightDiff0_byGroup_Ac225.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice0[ which(dmice0$agent == 'Lu177' | dmice0$agent == 'Sentinal' | dmice0$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  ggtitle('Statistical Mouse Trajectories, Lu177 + Control') +
  labs(x = "Day", y = "Weight Change from Day 0 (%)", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) + 
  ggsave(filename = 'Rfigs/weightDiff0_byGroup_Lu177.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice1[ which(dmice1$agent == 'Ac225' | dmice1$agent == 'Sentinal' | dmice1$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  ggtitle('Statistical Mouse Trajectories, Ac225 + Control') +
  labs(x = "Day", y = "Weight Change from Day 1 (%)", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) + 
  ggsave(filename = 'Rfigs/weightDiff1_byGroup_Ac225.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice1[ which(dmice1$agent == 'Lu177' | dmice1$agent == 'Sentinal' | dmice1$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  ggtitle('Statistical Mouse Trajectories, Lu177 + Control') +
  labs(x = "Day", y = "Weight Change from Day 1 (%)", fill = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) +
  ggsave(filename = 'Rfigs/weightDiff1_byGroup_Lu177.png', 
         width = fwid, height = fhei, units = "in")

#---------- Plots of Weight Change vs Day by Group - per Agent - Individual Mice ------------

dat <- melt(dmice0[ which(dmice0$agent == 'Ac225' | dmice0$agent == 'Sentinal' | dmice0$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
dat$variable <- as.numeric(levels(dat$variable)[dat$variable])
ggplot(dat, aes(x = variable, y = value, by = mousenum)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) +
  ggtitle('Individual Mouse Trajectories, Ac225 + Control') +
  labs(x = "Day", y = "Weight Change from Day 0 (%)", color = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) + 
  ggsave(filename = 'Rfigs/weightDiff0_byGroup_Ac225_indivMice.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice0[ which(dmice0$agent == 'Lu177' | dmice0$agent == 'Sentinal' | dmice0$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
dat$variable <- as.numeric(levels(dat$variable)[dat$variable])
ggplot(dat, aes(x = variable, y = value, by = mousenum)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) +
  ggtitle('Individual Mouse Trajectories, Lu177 + Control') +
  labs(x = "Day", y = "Weight Change from Day 0 (%)", color = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) +
  ggsave(filename = 'Rfigs/weightDiff0_byGroup_Lu177_indivMice.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice1[ which(dmice1$agent == 'Ac225' | dmice1$agent == 'Sentinal' | dmice1$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
dat$variable <- as.numeric(levels(dat$variable)[dat$variable])
ggplot(dat, aes(x = variable, y = value, by = mousenum)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) +
  ggtitle('Individual Mouse Trajectories, Ac225 + Control') +
  labs(x = "Day", y = "Weight Change from Day 1 (%)", color = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) +
  ggsave(filename = 'Rfigs/weightDiff1_byGroup_Ac225_indivMice.png', 
         width = fwid, height = fhei, units = "in")

dat <- melt(dmice1[ which(dmice1$agent == 'Lu177' | dmice1$agent == 'Sentinal' | dmice1$agent == 'Chelator'), ],
            id = c("group", "agent", "mousenum"))
dat$variable <- as.numeric(levels(dat$variable)[dat$variable])
ggplot(dat, aes(x = variable, y = value, by = mousenum)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) +
  ggtitle('Individual Mouse Trajectories, Lu177 + Control') +
  labs(x = "Day", y = "Weight Change from Day 1 (%)", color = "Group") +
  #scale_x_discrete(limits=1:11) +
  facet_wrap(~group, nrow=1) +
  ggsave(filename = 'Rfigs/weightDiff1_byGroup_Lu177_indivMice.png', 
         width = fwid, height = fhei, units = "in")

