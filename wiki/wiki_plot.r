# Plot the content in wiki.pairs.added.local.rds
# Yang Xu
# 1/27/2016

library(ggplot2)
library(data.table)
library(lme4)

dt = readRDS('wiki.utters.rds')

# remove NAs in distSelf and distOther, and isAdmin
dt = subset(dt, !is.na(distSelf) & !is.na(distOther) & !is.na(isAdmin))

# convert isAdmin to character
dt$isAdmin = as.character(dt$isAdmin)
dt[dt$isAdmin == '1',]$isAdmin = 'admins'
dt[dt$isAdmin == '0',]$isAdmin = 'non-admins'

# add a convLen column, indicating the length of a conversaion (number of utterances)
setkey(dt, convRoot)
dt.convLen = dt[, .(convLen = .N), by = convRoot]
dt = dt[dt.convLen]

# summary convLen
summary(dt.convLen$convLen) # mean 2.781
hist(dt.convLen$convLen)


# use wiki.survey.rds to select active users only
df.survey = readRDS('wiki.aggByUser.rds')
summary(df.survey$utterNum) # mean 15
hist(df.survey$utterNum)

# select active users
active_users = subset(df.survey, utterNum >= 15)$user

# remove localID = 1 from dt
dt = subset(dt, localID > 1)


# plot
# & user %in% active_users
p = ggplot(subset(dt, localID <= 10 & user %in% active_users), aes(x = localID, y = distSelf, group = isAdmin)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', aes(color = isAdmin, lty = isAdmin)) + 
    stat_summary(fun.y = mean, geom = 'line', aes(color = isAdmin, lty = isAdmin)) + 
    scale_x_continuous(breaks = 1:10)
pdf('distSelf_vs_localID.pdf')
plot(p)
dev.off()

p1 = ggplot(subset(dt, localID <= 10 & user %in% active_users), aes(x = localID, y = distOther, group = isAdmin)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', aes(color = isAdmin, lty = isAdmin)) + 
    stat_summary(fun.y = mean, geom = 'line', aes(color = isAdmin, lty = isAdmin)) + 
    scale_x_continuous(breaks = 1:10)
pdf('distOther_vs_localID.pdf')
plot(p1)
dev.off()


# examine effect of isAdmin
summary(lm(distSelf ~ isAdmin, dt)) # t = 16.79***
summary(lm(distOther ~ isAdmin, dt)) # t = 6.142***

# t test
t.test(subset(dt, isAdmin == 'admins')$distSelf,
    subset(dt, isAdmin == 'non-admins')$distSelf)
# admins have lower distSelf than non-admins

t.test(subset(dt, isAdmin == 'admins')$distOther,
    subset(dt, isAdmin == 'non-admins')$distOther)
# admins have lower distOther than non-admins


# examine effect of localID
summary(lmer(distSelf ~ localID + (1|convRoot), dt)) # t = 3.9