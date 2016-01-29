# compute the baseline linguistic style for each user in wikipedia
# Yang Xu
# 1/24/2016

library(data.table)
library(dplyr)

df.pairs = readRDS('df.pairs.rds')
dt.pairs = data.table(df.pairs)
setkey(dt.pairs, primeUtterID, targetUtterID)

dt.prime = select(dt.pairs, primeUtterID, primeUser, primeArt:primeQuant)
dt.target = select(dt.pairs, targetUtterID, targetUser, targetArt:targetQuant)
dt.all = rbindlist(list(dt.prime, dt.target))
dt.all = unique(dt.all)
colnames(dt.all) = c('utterID', 'user', 'art', 'auxv', 'conj', 'adv', 'ipron', 'ppron', 'prep', 'quant')

dt.agg = dt.all[, lapply(.SD, mean), by = user, .SDcols = 3:10]

# save to rds
saveRDS(dt.agg, 'wiki.user.baseline.rds')

# plot dt.agg
hist(dt.agg$auxv)