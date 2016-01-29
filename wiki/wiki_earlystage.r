# 
# Yang Xu
# 1/28/2016

library(data.table)
library(dplyr)
library(ggplot2)

# read wiki.pairs
dt = readRDS('wiki.pairs.added.local.rds')

# replace the convRoot of those pairs whose convRoot is NA to 0
dt[is.na(convRoot), convRoot:=0]

# select those conversations that contain only two speakers, 
# and the two speakers show significant different in first pair, i.e., 
# one speaker contains hedge, and the other one does not
setkey(dt, convRoot)
conv_speaker_num = dt[, .(speakerNum = length(union(primeUser, targetUser))), by = convRoot]

dt.2sp = dt[convRoot %in% subset(conv_speaker_num, speakerNum == 2)$convRoot, ]

conv_sig_diff = dt.2sp[.(unique(convRoot)), {
        sig = (primeHedge > 0 & targetHedge == 0) | (primeHedge == 0 & targetHedge > 0)
        if (sig) {
            user = ifelse(primeHedge > 0, primeUser, targetUser)
        } else {
            user = 'UNKNOWN'
        }
        .(sig = sig, user = user)
    }, 
    mult = 'first', by = convRoot]
setkey(conv_sig_diff, convRoot, user)

summary(conv_sig_diff$sig) # 7508 TRUE
# i.e., there are 7508 conversations whose first pair contain one speaker having hedge, and the other not having hedge.

# read wiki.utters
dt.utters = readRDS('wiki.utters.rds')
setkey(dt.utters, convRoot, user)

# create utterance sequence
conv_sig_diff_true = conv_sig_diff[sig == TRUE, convRoot, user]
setkey(conv_sig_diff_true, convRoot, user)
conv_sig_diff_true[, initHedge:='yes']

dt.utters.sig = dt.utters[convRoot %in% conv_sig_diff[sig == TRUE, convRoot]]
dt.utters.sig.match = dt.utters.sig[conv_sig_diff_true,]
dt.utters.sig.nomatch = dt.utters.sig[!conv_sig_diff_true,]
dt.utters.sig.nomatch[, initHedge:='no']
dt.utters.sig.all = rbindlist(list(dt.utters.sig.match, dt.utters.sig.nomatch))


# plot
p1 = ggplot(subset(dt.utters.sig.all, localID <= 6 & isAdmin == 0), 
    aes(x = localID, y = distSelf, color = initHedge, lty = initHedge)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_continuous(breaks = 1:10)

pdf('distSelf_vs_localID_initHedge.pdf')
plot(p1)
dev.off()

p2 = ggplot(subset(dt.utters.sig.all, localID <= 6 & isAdmin == 0), 
    aes(x = localID, y = distOther, color = initHedge, lty = initHedge)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_continuous(breaks = 1:10)

pdf('distOther_vs_localID_initHedge.pdf')
plot(p2)
dev.off()
