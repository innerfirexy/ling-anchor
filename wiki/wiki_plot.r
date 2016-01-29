# Plot the content in wiki.*.rds
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


# add angSelf, angOther columns
dt$angSelf = acos(dt$distSelf)/pi
dt$angOther = acos(dt$distOther)/pi
dt[is.nan(angSelf), angSelf:=0]

# plot angle vs localID
pa1 = ggplot(subset(dt, localID <= 10 & user %in% active_users), aes(x = localID, y = angSelf, group = isAdmin)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', aes(color = isAdmin, lty = isAdmin)) + 
    stat_summary(fun.y = mean, geom = 'line', aes(color = isAdmin, lty = isAdmin)) + 
    scale_x_continuous(breaks = 1:10)
plot(pa1)



# use wiki.survey.rds to select active users only
df.survey = readRDS('wiki.aggByUser.rds')
summary(df.survey$utterNum) # mean 15
hist(df.survey$utterNum)

# select active users
active_users = subset(df.survey, utterNum >= 15)$user

# remove localID = 1 from dt
# dt = subset(dt, localID > 1)


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



## effect of hedgeRatio, high vs low
# read df.high_hr.rds and df.low_hr.rds
df.high_hr = readRDS('wiki.high_hr.rds')
df.low_hr = readRDS('wiki.low_hr.rds')

dt.hr = subset(dt, user %in% c(df.high_hr$user, df.low_hr$user))

# add hr column: high vs low
dt.hr$hr = 'low'
dt.hr[dt.hr$user %in% df.high_hr$user, ]$hr = 'high'
dt.hr$hr = as.factor(dt.hr$hr)

# find all the convRoot that have two speakers, one of which is from high_hr, and the other from low_hr
df.pairs = readRDS('wiki.pairs.added.local.rds')

conv_roots1 = unique(subset(df.pairs, 
    ((primeUser %in% df.high_hr$user & targetUser %in% df.low_hr$user) | 
    (primeUser %in% df.low_hr$user & targetUser %in% df.high_hr$user)) &
    (primeIsAdmin == 0 & targetIsAdmin == 0))$convRoot)
dt.hr.sub1 = subset(dt.hr, convRoot %in% conv_roots1)

p4 = ggplot(subset(dt.hr.sub1, localID <= 6), aes(x = localID, y = distSelf, color = hr, lty = hr)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_continuous(breaks = 1:10)
pdf('distSelf_vs_localID_hrHighLow_nonadmin.pdf')
plot(p4)
dev.off()

p5 = ggplot(subset(dt.hr.sub1, localID <= 6), aes(x = localID, y = distOther, color = hr, lty = hr)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_continuous(breaks = 1:10)
pdf('distOther_vs_localID_hrHighLow_nonadmin.pdf')
plot(p5)
dev.off()


conv_roots2 = unique(subset(df.pairs, 
    ((primeUser %in% df.high_hr$user & targetUser %in% df.low_hr$user) | 
    (primeUser %in% df.low_hr$user & targetUser %in% df.high_hr$user)) &
    (primeIsAdmin == 1 & targetIsAdmin == 1))$convRoot)
dt.hr.sub2 = subset(dt.hr, convRoot %in% conv_roots2)

p6 = ggplot(subset(dt.hr.sub1, localID <= 6), aes(x = localID, y = distSelf, color = hr, lty = hr)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_continuous(breaks = 1:10)
pdf('distSelf_vs_localID_hrHighLow_admin.pdf')
plot(p6)
dev.off()

p7 = ggplot(subset(dt.hr.sub1, localID <= 6), aes(x = localID, y = distOther, color = hr, lty = hr)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_continuous(breaks = 1:10)
pdf('distOther_vs_localID_hrHighLow_admin.pdf')
plot(p7)
dev.off()



# models
summary(lm(distOther ~ localID*hr, subset(dt.hr.sub1, localID <= 2)))
# low hr has smaller distOther than high hr, t = -3.419***
# interaction t = 2.760**

summary(lm(distSelf ~ localID*hr, subset(dt.hr.sub1, localID <= 2)))
# low hr has smaller distSelf than high hr, t = -2.779**
# interaction t = 2.316*

summary(lm(distOther ~ localID*hr, subset(dt.hr.sub2, localID <= 2)))

summary(lm(distSelf ~ localID*hr, subset(dt.hr.sub2, localID <= 2)))





### examine effect of localID
summary(lmer(distSelf ~ localID + (1|convRoot), dt)) # t = 3.9