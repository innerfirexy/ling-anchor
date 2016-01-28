# Add turnID for each entry in wiki.pairs.added.rds (for primeUtterID and targetUtterID repectively)
# recording their local positions in conversations
# Yang Xu
# 1/27/2016

library(RMySQL)
library(data.table)
library(dplyr)

# db init
# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'wiki')
sql = 'SELECT utteranceID, conversationRoot, replyTo FROM conversations'
df.db = dbGetQuery(conn, sql)

# construct data.table
dt.db = data.table(df.db, key = c('utteranceID', 'conversationRoot', 'replyTo'))

# add localID to dt.db
dt.db.added = dt.db[, list(utterID = utteranceID, localID = 1:.N), by = conversationRoot]
dt.db.added[, conversationRoot:=NULL]
setkey(dt.db.added, utterID)

# read wiki.pairs.added.rds
dt.pairs = readRDS('wiki.pairs.added.rds')

# merge localID back to dt.pairs
setkey(dt.pairs, primeUtterID)
dt.pairs.tmp = dt.pairs[dt.db.added, nomatch = 0]
dt.pairs.tmp = rename(dt.pairs.tmp, primeLocalID = localID)

setkey(dt.pairs.tmp, targetUtterID)
dt.pairs.local = dt.pairs.tmp[dt.db.added, nomatch = 0]
dt.pairs.local = rename(dt.pairs.local, targetLocalID = localID)

# save to RDS
saveRDS(dt.pairs.local, 'wiki.pairs.added.local.rds')