# Compute each utterance's distance from the corresponding user's baseline position
# Yang Xu
# 1/25/2016

library(data.table)
library(dplyr)
library(lsa)

df.pairs = readRDS('wiki.pairs.rds')
dt.pairs = data.table(df.pairs, key = c('primeUser', 'targetUser'))

dt.ub = readRDS('wiki.user.baseline.rds')
setkey(dt.ub, key = user)

# # run once
# ## save all rds files to julia-friendly csv files
# write.csv(df.pairs, file = 'wiki.pairs.csv', row.names = FALSE)
# write.csv(dt.ub, file = 'wiki.user.baseline.csv', row.names = FALSE)


# for each line in dt.pairs, compute the distance between prime.vec and prime.base
# and between target.vec and target.base

# first, get all unique (utterID, user, art, auxv, ..., quant) vectors
prime.vec = select(dt.pairs, primeUtterID, primeUser, primeArt:primeQuant, targetUser)
target.vec = select(dt.pairs, targetUtterID, targetUser, targetArt:targetQuant, primeUser)

# join prime and target baseline to prime.vec
setkey(prime.vec, primeUser)
prime.vec = prime.vec[dt.ub, nomatch = 0]
colnames(prime.vec) = c(colnames(prime.vec)[1:11], paste0(colnames(prime.vec)[12:19], '_pb'))

setkey(prime.vec, targetUser)
prime.vec = prime.vec[dt.ub, nomatch = 0]
colnames(prime.vec) = c(colnames(prime.vec)[1:19], paste0(colnames(prime.vec)[20:27], '_tb'))

# join prime and target baseline to target.vec
setkey(target.vec, targetUser)
target.vec = target.vec[dt.ub, nomatch = 0]
colnames(target.vec) = c(colnames(target.vec)[1:11], paste0(colnames(target.vec)[12:19], '_tb'))

setkey(target.vec, primeUser)
target.vec = target.vec[dt.ub, nomatch = 0]
colnames(target.vec) = c(colnames(target.vec)[1:19], paste0(colnames(target.vec)[20:27], '_pb'))



# method 1
system.time(prime.dist <- prime.vec[, {
        vec = as.numeric(.SD[, 3:10, with = FALSE])
        base_self = as.numeric(.SD[, 12:19, with = FALSE])
        base_other = as.numeric(.SD[, 20:27, with = FALSE])
        list(primeUtterID = primeUtterID, distSelf = cosine(vec, base_self)[1,1], distOther = cosine(vec, base_other)[1,1])
    }, by = 1:nrow(prime.vec)])
# benchmark on brain
# user  system elapsed 
# 274.392   0.547 275.423

system.time(target.dist <- target.vec[, {
        vec = as.numeric(.SD[, 3:10, with = FALSE])
        base_self = as.numeric(.SD[, 12:19, with = FALSE])
        base_other = as.numeric(.SD[, 20:27, with = FALSE])
        list(targetUtterID = targetUtterID, distSelf = cosine(vec, base_self)[1,1], distOther = cosine(vec, base_other)[1,1])
    }, by = 1:nrow(target.vec)])

prime.dist[, nrow := NULL]
setkey(prime.dist, primeUtterID)
prime.dist = rename(prime.dist, primeDistSelf = distSelf, primeDistOther = distOther)

target.dist[, nrow := NULL]
setkey(target.dist, targetUtterID)
target.dist = rename(target.dist, targetDistSelf = distSelf, targetDistOther = distOther)

# combine
pairs.dist = cbind(prime.dist, target.dist)



# method 2: use primeUtterID as the only key



## merge the results back to dt.pairs, 
setkey(dt.pairs, primeUtterID)
dt.pairs = dt.pairs[prime.dist]