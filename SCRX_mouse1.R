#library(drc)
#library(drfit)
library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)

# import the data
mice <- read.xlsx("../Data/SCRXmouse1Weights.xlsx", header = TRUE, sheetIndex = 1)

# convert row names to mouse number and drop mouse number column
rownames(mice) <- mice$Mouse..
mice <- mice[, -2]

# calculate the doses that each mouse received based on the dosing sheet
day0dose <- cut(mice$Day.0, breaks = 15.8+1.5*0:7, 
                labels = 0.1+1:7/100)
day1dose <- cut(mice$Day.1, breaks = 15.8+1.5*0:7, 
                labels = 0.1+1:7/100)
mice_dose <- cbind(mice[,1:3], day0dose, day1dose)

# modify col names for day to be numeric
colnames(mice) <- c('group', '0', '1', '4', '7', '11')

# create factor for dose radionuclide
agent <- cut(as.numeric(mice$group), breaks = c(0,5,9,10,11),
             labels = c('Ac225','Lu177','Chelator','Sentinal'))
mice <- cbind(mice, agent)

# melt the data to plot 
mmice <- melt(mice, id = c("group", "agent"))

fig_weight <- ggplot(mmice, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(color = group)) + guides(color=FALSE) +
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight", fill = "Group") +
  scale_x_discrete(limits=1:12) +
  facet_wrap(~agent)
fig_weight


# compute the differences based on ref day 0
dmice <- mice
refday <- 0

for(i in seq(ncol(dmice)-2)){
  dmice[,i+1] = dmice[,i+1] - mice[,2+refday]
}
mdmice <- melt(dmice, id = c("group", "agent"))

fig_day <- ggplot(mdmice, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), position = 'dodge') + 
  geom_bar(aes(fill = variable), position = 'dodge', stat = 'identity') +
  theme_bw() +
  labs(x = "Group", y = "Weight Change (Day 0)", fill = "Group", color = NULL)

fig_day

update_geom_defaults("point", list(color = NULL))
fig_day <- ggplot(mdmice, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), position = 'dodge') + guides(color = FALSE) +
  geom_boxplot(aes(fill = variable), position = 'dodge', outlier.color = NA) + 
  theme_bw() +
  labs(x = "Group", y = "Weight Change (Day 0)", fill = "Group", color = NULL)

fig_day












# compute the differences based on ref day 1
dmice <- mice
refday <- 1

for(i in seq(ncol(dmice)-2)){
  dmice[,i+1] = dmice[,i+1] - mice[,2+refday]
}
mdmice <- melt(dmice, id = c("group", "agent"))



update_geom_defaults("point", list(color = NULL))
fig_day <- ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), position = 'dodge') +
  guides(color = FALSE) +
  geom_boxplot(aes(fill = variable), position = 'dodge', outlier.color = NA) + 
  theme_bw() +
  labs(x = "Group", y = "Weight Change (Day 1)", fill = "Group", color = NULL)

fig_day



dat <- mdmice[ which(mdmice$variable != 0), ]

update_geom_defaults("point", list(color = NULL))
fig_day <- ggplot(dat, aes(x = group, y = value, by = variable)) + 
  geom_boxplot(aes(color = variable), position = 'dodge') +
  guides(color = FALSE) +
  geom_boxplot(aes(fill = variable), position = 'dodge', outlier.color = NA) + 
  theme_bw() +
  labs(x = "Group", y = "Weight Change (Day 1)", fill = "Group", color = NULL)

fig_day





fig_weight_diff <- ggplot(mdmice, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group, color = group), outlier.color = NULL) + 
  geom_boxplot(aes(fill = group), outlier.color = NA) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group, group = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight Change", fill = "Group") +
  scale_x_discrete(limits=1:11) +
  facet_wrap(~agent)
fig_weight_diff


fig_weight_diff <- ggplot(mdmice, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group), outlier.color = NULL) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group, group = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight Change", fill = "Group") +
  scale_x_discrete(limits=1:11) +
  facet_wrap(~group)
fig_weight_diff


dat <- mdmice [which(mdmice$agent == 'Ac225' | mdmice$agent == 'Sentinal'), ]

fig_weight_diff <- ggplot(dat, aes(x = variable, y = value, by = group)) + 
  geom_boxplot(aes(fill = group), outlier.color = NULL) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group, group = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight Change", fill = "Group") +
  scale_x_discrete(limits=1:11) +
  facet_wrap(~group)
fig_weight_diff


 # add the mouse number back to data

micenum <- cbind(dmice, rownames(dmice))
colnames(micenum) <- c(colnames(micenum)[-ncol(micenum)], 'mousenumber')
mdmicenum <- melt(micenum, id = c("group", "agent", "mousenumber"))



dat <- mdmicenum[ which(mdmicenum$agent == 'Ac225' & mdmicenum$variable != 0), ]
dat <- mdmicenum[ which(mdmicenum$variable != 0), ]
dat$variable <- as.numeric(levels(dat$variable)[dat$variable])

ggplot(dat, aes(x = variable, y = value, by = mousenumber)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight Change", fill = "Group") +
  scale_x_discrete(limits=1:11) +
  facet_wrap(~group)




fig_weight_diff <- ggplot(mdmicenum, aes(x = variable, y = value, by = mousenumber)) + 
  geom_line(aes(color = group)) + 
  #geom_line(aes(x = as.numeric(as.character(variable)), color = group, group = group)) +
  theme_bw() +
  labs(x = "Day", y = "Weight Change", fill = "Group") +
  scale_x_discrete(limits=1:11) +
  facet_wrap(~agent)
fig_weight_diff





# count up the NA values (dead mice)
# 
# survivingmice2 <- ddply(mmice, .(group, variable, agent), summarize, 
#       alive = is.finite(value))
# survivingmice2 <- subset(survivingmice2, alive == TRUE)
# 
# fig_alive_count <- ggplot(survivingmice2, aes(x = variable, 
#                                              by = group)) + 
#   geom_bar(aes(fill = group), position = position_dodge(width = 1.1)) + 
#   labs(x = "Day", y = "Surviving Mice", fill = "Group") +
#   theme_bw() +
#   scale_x_discrete(limits=1:6) +
#   facet_wrap(~agent)
# fig_alive_count


# STOP HERE FOR COPYING TO REPORT - ALTERNATE SURVIVAL FIGURES BELOW

# survivingmice <- ddply(mmice, .(group, variable, agent), summarize,
#                        dead = sum(is.na(value)), 
#                        live = 5 - dead)
# 
# # this gives large filled in areas
# fig_dead_count <- ggplot(survivingmice, aes(x = as.numeric(as.character(variable)), 
#                                             y = live, by = group)) + 
#   geom_area(aes(fill = group), position = position_dodge(width = 0.2), alpha = 0.2) + 
#   labs(x = "Day", y = "Survival (%)", color = "Group") +
#   theme_bw() +
#   scale_x_discrete(limits=1:6) +
#   facet_wrap(~agent)
# fig_dead_count
# 
# # this gives just vertical bars
# fig_dead_count <- ggplot(survivingmice, aes(x = variable, 
#                                             y = live, by = group)) + 
#   geom_area(aes(color = group), position = position_dodge(width = 0.2)) + 
#   labs(x = "Day", y = "Survival (%)", color = "Group") +
#   theme_bw() +
#   scale_x_discrete(limits=1:6) +
#   facet_wrap(~agent)
# fig_dead_count
# 
# 
# 
# # working on improving vertical bars
# 
# 
# # this is survival tracked with lines and area fill
# fig_survival <- ggplot(survivingmice, aes(x = as.numeric(as.character(variable)), 
#                                           y = live*20, by = group)) + 
#   geom_line(aes(color = group)) + 
#   geom_point(aes(color = group)) + 
#   geom_area(aes(fill = group), position = position_dodge(width = 0.2), alpha = 0.1) + 
#   labs(x = "Day", y = "Survival (%)", color = "Group") +
#   scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
#   theme_bw() +
#   facet_wrap(~agent)
# fig_survival
