# Extract the unique utterances from wiki.pairs.added.local.rds
# Yang Xu
# 1/27/2016

library(data.table)
library(dplyr)

dt.pairs = readRDS('wiki.pairs.added.local.rds')

# extract prime and target turns
dt.prime = select(dt.pairs, convRoot, primeUtterID, primeUser, primeIsAdmin, primeDistSelf, primeDistOther, primeLocalID)
colnames(dt.prime) = c('convRoot', 'utterID', 'user', 'isAdmin', 'distSelf', 'distOther', 'localID')
setkey(dt.prime, utterID)

dt.target = select(dt.pairs, convRoot, targetUtterID, targetUser, targetIsAdmin, targetDistSelf, targetDistOther, targetLocalID)
colnames(dt.target) = colnames(dt.prime)
setkey(dt.target, utterID)

# merge and unique
dt.utters = rbindlist(list(dt.prime, dt.target))
setkey(dt.utters, utterID)
dt.utters = unique(dt.utters)

# save dt.utters to rds
saveRDS(dt.utters, 'wiki.utters.rds')