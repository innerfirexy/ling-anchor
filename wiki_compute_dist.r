# Compute each utterance's distance from the corresponding user's baseline position
# Yang Xu
# 1/25/2016

library(data.table)
library(dplyr)

df.pairs = readRDS('wiki.pairs.rds')
dt.pairs = data.table(df.pairs, key = c('primeUtterID', 'targetUtterID'))

dt.ub = readRDS('wiki.user.baseline.rds')

