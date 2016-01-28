# Plot the content in wiki.pairs.added.local.rds
# Yang Xu
# 1/27/2016

library(ggplot2)

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


# plot
p = ggplot(subset(dt, localID <= 5), aes(x = localID, y = distSelf, group = isAdmin)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', aes(color = isAdmin, fill = isAdmin))
plot(p)

p1 = ggplot(subset(dt, localID <= 5), aes(x = localID, y = distOther, group = isAdmin)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', aes(color = isAdmin, fill = isAdmin))
plot(p1)