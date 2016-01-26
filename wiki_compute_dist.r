# Compute each utterance's distance from the corresponding user's baseline position
# Yang Xu
# 1/25/2016

library(data.table)
library(dplyr)
library(lsa)

df.pairs = readRDS('wiki.pairs.rds')
dt.pairs = data.table(df.pairs, key = c('primeUtterID', 'primeUser', 'targetUtterID', 'targetUser'))

dt.ub = readRDS('wiki.user.baseline.rds')
setkey(dt.ub, key = user)


# for each line in dt.pairs, compute the distance between prime.vec and prime.base
# and between target.vec and target.base

# first, get all unique (utterID, user, art, auxv, ..., quant) vectors
prime.vec = select(dt.pairs, primeUtterID, primeUser, primeArt:primeQuant)
target.vec = select(dt.pairs, targetUtterID, targetUser, targetArt:targetQuant)

uniq.vec = unique(rbindlist(list(prime.vec, target.vec)))
uniq.vec = rename(uniq.vec, user = primeUser)
# colnames(uniq.vec) = c('utterID', 'user', 'art', 'auxv', 'conj', 'adv', 'ipron', 'ppron', 'prep', 'quant')
setkey(uniq.vec, user)

# join uniq.vec with dt.ub, and compute distance between vec and base
uniq.vec.join = uniq.vec[dt.ub]

# method 1
system.time(uniq.dist <- uniq.vec.join[, {
        vec = as.numeric(.SD[, 3:10, with = FALSE])
        base = as.numeric(.SD[, 11:18, with = FALSE])
        list(primeUtterID = primeUtterID, dist = cosine(vec, base)[1,1])
    }, by = 1:nrow(uniq.vec.join)])
# user  system elapsed 
# 384.287   2.786 392.986 

# method 2: use primeUtterID as the only key
uniq.dist = 

# method 3: vectorize, failed
# cosine cannot be vectorized


## merge the results back to dt.pairs, 