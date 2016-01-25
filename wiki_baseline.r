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

