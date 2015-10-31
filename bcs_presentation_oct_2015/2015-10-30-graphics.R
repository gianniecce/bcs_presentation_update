---
title: "bcs anaylsis word tables"
author: "Giacomo"
date: "30 Oct 2015"
output: word_document
---
  
source('/Users/giacomovagni/Rprojects/analysis_bcs/bcs_stats 2015-10-30-script_presentation.R')

# dta3 %>% select(pid, SEX_rec, month_rec, maxep) %>% distinct() %>% group_by(pid) %>% mutate(n_max_ep = sum(maxep)) %>% select(SEX_rec, month_rec, n_max_ep) %>% distinct() %>% group_by(SEX_rec, month_rec) %>% mutate(smep = sum(n_max_ep)) %>% select(SEX_rec, month_rec, smep)
# dta3 %>% group_by(SEX_rec, month_rec) %>% distinct(pid) %>% summarise(n = n()) %>% mutate(n = n / sum(n)) %>% df %>% ggplot(aes(month_rec, n, fill = month_rec)) + geom_bar(stat = "identity") + facet_grid(facets=. ~ SEX_rec) + theme_minimal() + ylab('') + scale_fill_manual(values=c('#FB8072', "#56B4E9", "#FDB462")) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank()) + guides(fill=guide_legend(title="month"), guide_legend(reverse = TRUE))
#dta3 %>% group_by(SEX_rec) %>% distinct(pid) %>% summarise(n = n()) %>% df %>% ggplot(aes(1, n, fill = SEX_rec)) + geom_bar(stat = "identity") + facet_grid(facets=. ~ SEX_rec) + guides(fill=guide_legend(title="month")) + theme_minimal() + scale_fill_manual(values=c("lightpink", '#F0E442')) + theme(axis.text.x= element_blank(), axis.title.x=element_blank())

dat = select(dta3, diaryday, act_rec, SEX, duration) %>% left_join(., count(distinct(dta3, pid), SEX)) %>% group_by(diaryday, act_rec, SEX, n) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = sdur / n) %>% group_by(SEX) %>% mutate(sum(meanduration)) %>% select(diaryday, act_rec, SEX, meanduration) # %>% spread(act_rec, meanduration)
cbind(table(dat$act_rec), brewer.pal(12, "Set3"))

perso = c("Eat drink", "Domestic work", "Paid work", "School / Study")
leisure = c("Nonsocial leisure", "TV/radio", "Sport exercise")
human = c("Cultural leisure", "Religion and volunteering", "Social leisure")
sleep = c("Personal care", "Miscellaneous")

quartz()
# perso 
#  boys 
par(mfrow = c(1,2))
dta1 = filter(dat, act_rec %in% perso & SEX == 1) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

text(dta1[,3] + 5, labels = paste( round(dta1[,3]), "min", sep = "") ,family="Garamond" , col = "blue", cex = 1)
text(dta1[,4] + 5, labels = paste( round(dta1[,4]), "min", sep = "") ,family="Garamond" , col = "red", cex = 1)
text(dta1[,5] + 5, labels = paste( round(dta1[,5]), "min", sep = "") ,family="Garamond" , col = "purple", cex = 1)
text(dta1[,6] + 5, labels = paste( round(dta1[,6]), "min", sep = "") ,family="Garamond" , col = "black", cex = 1)

# girls 
dta1 = filter(dat, act_rec %in% perso & SEX == 2) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

text(dta1[,3] + 5, labels = paste( round(dta1[,3]), "min", sep = "") ,family="Garamond" , col = "blue", cex = 1)
text(dta1[,4] + 5, labels = paste( round(dta1[,4]), "min", sep = "") ,family="Garamond" , col = "red", cex = 1)
text(dta1[,5] + 5, labels = paste( round(dta1[,5]), "min", sep = "") ,family="Garamond" , col = "purple", cex = 1)
text(dta1[,6] + 5, labels = paste( round(dta1[,6]), "min", sep = "") ,family="Garamond" , col = "black", cex = 1)

# leisure
quartz()
par(mfrow = c(1,2))
#  boys 
dta1 = filter(dat, act_rec %in% leisure & SEX == 1) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

text(dta1[,3] + 5, labels = paste( round(dta1[,3]), "min", sep = "") ,family="Garamond" , col = "blue", cex = 1)
text(dta1[,4] + 8, labels = paste( round(dta1[,4]), "min", sep = "") ,family="Garamond" , col = "red", cex = 1)
text(dta1[,5] + 5, labels = paste( round(dta1[,5]), "min", sep = "") ,family="Garamond" , col = "purple", cex = 1)

# girls 
dta1 = filter(dat, act_rec %in% leisure & SEX == 2) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

text(dta1[,3] + 5, labels = paste( round(dta1[,3]), "min", sep = "") ,family="Garamond" , col = "blue", cex = 1)
text(dta1[,4] + 5, labels = paste( round(dta1[,4]), "min", sep = "") ,family="Garamond" , col = "red", cex = 1)
text(dta1[,5] + 5, labels = paste( round(dta1[,5]), "min", sep = "") ,family="Garamond" , col = "purple", cex = 1)

# human
quartz()
par(mfrow = c(1,2))
#  boys 
dta1 = filter(dat, act_rec %in% human & SEX == 1) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

text(dta1[,3] + 5, labels = paste( round(dta1[,3]), "min", sep = "") ,family="Garamond" , col = "orange", cex = 1)
text(dta1[,4] + 8, labels = paste( round(dta1[,4]), "min", sep = "") ,family="Garamond" , col = "darkgrey", cex = 1)
text(dta1[,5] + 5, labels = paste( round(dta1[,5]), "min", sep = "") ,family="Garamond" , col = "brown", cex = 1)

# girls 
dta1 = filter(dat, act_rec %in% human & SEX == 2) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

text(dta1[,3] + 5, labels = paste( round(dta1[,3]), "min", sep = "") ,family="Garamond" , col = "orange", cex = 1)
text(dta1[,4] + 8, labels = paste( round(dta1[,4]), "min", sep = "") ,family="Garamond" , col = "darkgrey", cex = 1)
text(dta1[,5] + 5, labels = paste( round(dta1[,5]), "min", sep = "") ,family="Garamond" , col = "brown", cex = 1)

# ggplot(data=filter(dat, act_rec %in% leisure), aes(x=diaryday, y=meanduration, group=act_rec, color = act_rec)) + facet_grid(.~ SEX) + geom_line(size=1.5) + geom_point(size=3, shape=21, fill="grey") + theme_minimal()

# ggplot(data=filter(dat, act_rec %in% human), aes(x=diaryday, y=meanduration, group=act_rec, color = act_rec)) + facet_grid(.~ SEX) + geom_line(size=1.5) + geom_point(size=3, shape=21, fill="grey") + theme_minimal()

# ggplot(data=filter(dat, act_rec %in% sleep), aes(x=diaryday, y=meanduration, group=act_rec, color = act_rec)) + facet_grid(.~ SEX) + geom_line(size=1.5) + geom_point(size=3, shape=21, fill="grey") + theme_minimal()

time_use = dataWide3[, grepl(pattern = "DAY", x = names(dataWide3))] 
kol = brewer.pal(12, 'Set3')

time_seq = seqdef(time_use, cpal = kol)

quartz()
seqiplot(time_seq[,1000:1200], border = NA, withlegend = F, axes = F)
seqiplot(time_seq[,1000:1200], border = NA, withlegend = F, axes = T)
seqlegend(time_seq, bty = "n", family = "Garamond")

TimeClock(1200)
