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

perso = c("Eat drink", "Domestic work") # "Paid work", 
TV = c("TV/radio", "Domestic work") 
leisure = c("Nonsocial leisure", "Sport exercise", "Social leisure")
human = c("Cultural leisure", "School / Study", "Religion and volunteering")
sleep = c("Personal care") # "Miscellaneous"

quartz()
# perso 
#  boys 
par(mfrow = c(1,2))
dta1 = filter(dat, act_rec %in% sleep & SEX == 1) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,600), xlim = c(1,4), axes = F, ylab = "", xlab = "")
axis(1, at = c(1,2,3,4), labels = c('', '', '', ''), col = 'grey')
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2, cex = 2)
text(dta1[,3] + 20, labels = paste( TimeClock( round(dta1[,3]) )) ,family="Garamond" , col = "blue", cex = 1.2)

# girls 
dta1 = filter(dat, act_rec %in% sleep & SEX == 2) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,600), xlim = c(1,4), axes = F, ylab = "", xlab = "")
axis(1, at = c(1,2,3,4), labels = c('', '', '', ''), col = 'grey')
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
text(dta1[,3] + 20, labels = paste( TimeClock( round(dta1[,3]) )) ,family="Garamond" , col = "blue", cex = 1.2)

quartz()
# perso 
#  boys 
par(mfrow = c(1,2))
par(xpd=F)
dta1 = filter(dat, act_rec %in% TV & SEX == 1) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
axis(1, at = c(1,2,3,4), labels = c('', '', '', ''), col = 'grey')
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2, cex = 2)
lines(dta1[,4], pch = 1, type="b", lty = 3, cex = 2)

par(xpd=T)
text(dta1[,3] - 8, labels = paste( TimeClock ( round(dta1[,3] ) ) , "", sep = "") ,family="Garamond" , col = "blue", cex = 1.5)
text(dta1[,4] + 7, labels = paste( TimeClock ( round(dta1[,4] ) ) , "", sep = "") ,family="Garamond" , col = "red", cex = 1.5)

# girls 
dta1 = filter(dat, act_rec %in% TV & SEX == 2) %>% spread(act_rec, meanduration) %>% df 
par(xpd=F)
plot(type = "n", 0, ylim = c(0,200), xlim = c(1,4), axes = F, ylab = "", xlab = "")
axis(1, at = c(1,2,3,4), labels = c('', '', '', ''), col = 'grey')
abline(v = c(1,2,3,4), col = "grey")
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2, cex = 2)
lines(dta1[,4], pch = 1, type="b", lty = 3, cex = 2) 
par(xpd=T)
text(dta1[,3] - 8, labels = paste( TimeClock( round(dta1[,3] ) ), "", sep = "") ,family="Garamond" , col = "blue", cex = 1.5)
text(dta1[,4] + 7, labels = paste( TimeClock( round(dta1[,4] ) ), "", sep = "") ,family="Garamond" , col = "red", cex = 1.5)

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
plot(type = "n", 0, ylim = c(0,50), xlim = c(1,4), axes = F, ylab = "", xlab = "")
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
par(xpd=F)
#  boys 
dta1 = filter(dat, act_rec %in% human & SEX == 1) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,50), xlim = c(1,4), axes = F, ylab = "", xlab = "")
abline(v = c(1,2,3,4), col = "grey")
axis(1, at = c(1,2,3,4), labels = c('', '', '', ''), col = 'grey')
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)
lines(dta1[,6], pch = 9, type="b", lty = 5)

par(xpd=TRUE)
text(dta1[,3] + 2, labels = paste( TimeClock( round(dta1[,3]) ), "", sep = "") ,family="Garamond" , col = "purple", cex = 1.5)
text(dta1[,4] - 2, labels = paste( TimeClock(round(dta1[,4]) ), "", sep = "") ,family="Garamond" , col = "darkgrey", cex = 1.5)
text(dta1[,5] + 4, labels = paste( TimeClock(round(dta1[,5]) ), "", sep = "") ,family="Garamond" , col = "brown", cex = 1.5)

# girls 
dta1 = filter(dat, act_rec %in% human & SEX == 2) %>% spread(act_rec, meanduration) %>% df 
plot(type = "n", 0, ylim = c(0,50), xlim = c(1,4), axes = F, ylab = "", xlab = "")
par(xpd=F)
abline(v = c(1,2,3,4), col = "grey")
axis(1, at = c(1,2,3,4), labels = c('', '', '', ''), col = 'grey')
# text(c(1,2,3,4), y = 200, labels = c("Friday", "Friday", "Friday", "Friday"), family="Garamond")
lines(dta1[,3], pch = 4, type="b", lty = 2)
lines(dta1[,4], pch = 10, type="b", lty = 3)
lines(dta1[,5], pch = 2, type="b", lty = 4)

par(xpd=TRUE)
text(dta1[,3] + 2, labels = paste( TimeClock( round(dta1[,3]) ), "", sep = "") ,family="Garamond" , col = "purple", cex = 1.5)
text(dta1[,4] - 2, labels = paste( TimeClock(round(dta1[,4]) ), "", sep = "") ,family="Garamond" , col = "darkgrey", cex = 1.5)
text(dta1[,5] + 4, labels = paste( TimeClock(round(dta1[,5]) ), "", sep = "") ,family="Garamond" , col = "brown", cex = 1.5)

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

###### 
###### Life Course Plots 
###### 

x = 1:1440

quartz()
# girls 
p1 = ggplot(filter(lifecourse_ppSumClust, gender == 'girls' & life_clusters %in% c("Long_Education") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p2 = ggplot(filter(lifecourse_ppSumClust, gender == 'girls' & life_clusters %in% c("Full-Time") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p3 = ggplot(filter(lifecourse_ppSumClust, gender == 'girls' & life_clusters %in% c("Looking_After_Home") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p4 = ggplot(filter(lifecourse_ppSumClust, gender == 'girls' & life_clusters %in% c("Trans-Self-Emp") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p5 = ggplot(filter(lifecourse_ppSumClust, gender == 'girls' & life_clusters %in% c("Full-Part-Full" ) ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p6 = ggplot(filter(lifecourse_ppSumClust, gender == 'girls' & life_clusters %in% c("Part-Time") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
grid.arrange(p1, p2, p3, p4, p5, p6)

# i plot 
long_girls = ggplot(lcp %>% filter(gender == 'girls' & life_clusters == 'Long_Education'), aes(x=variable, y=pid, fill=value)) + ggtitle("Long Education - Girls") + geom_tile() + theme_minimal() + scale_fill_manual(values=kol) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
1970 - 1990 

quartz()
# boys 
p1 = ggplot(filter(lifecourse_ppSumClust, gender == 'boys' & life_clusters %in% c("Long_Education") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p2 = ggplot(filter(lifecourse_ppSumClust, gender == 'boys' & life_clusters %in% c("Full-Time") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p3 = ggplot(filter(lifecourse_ppSumClust, gender == 'boys' & life_clusters %in% c("Full-Self-Full") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p4 = ggplot(filter(lifecourse_ppSumClust, gender == 'boys' & life_clusters %in% c("Trans-Self-Emp") ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
p5 = ggplot(filter(lifecourse_ppSumClust, gender == 'boys' & life_clusters %in% c("Sick/Unemployed" ) ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
grid.arrange(p1, p2, p3, p4, p5)

# i plot 
long_boys = ggplot(lcp %>% filter(gender == 'boys' & life_clusters == 'Long_Education'), aes(x=variable, y=pid, fill=value)) + ggtitle("Long Education - Boys") + geom_tile() + theme_minimal() + scale_fill_manual(values=kol) + theme(axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank(), legend.position = "none")
1970 - 1990 

quartz()
grid.arrange(long_girls, long_boys)

# girls 
EventGirls = lcp %>% filter(gender == 'girls' & life_clusters == 'Long_Education') %>% arrange(pid)
EventGirls2 = EventGirls %>% group_by(pid) %>% mutate(StopEdu = ifelse(value != 'Education', 1, 0))
EventGirls3 = EventGirls2 %>% group_by(pid) %>% mutate(Event = ifelse(StopEdu, 99, 0)) %>% mutate(Event = lag(Event, default = 0)) %>% df
EventGirls4 = filter(EventGirls3, Event != 99)
  
EventGirls3 %>% group_by(variable, value, Event) %>% summarise(n())
EventGirls4 %>% group_by(variable, value, StopEdu) %>% summarise(n())

# boys 
EventBoys = lcp %>% filter(gender == 'boys' & life_clusters == 'Long_Education') %>% arrange(pid)
EventBoys2 = EventBoys %>% group_by(pid) %>% mutate(StopEdu = ifelse(value != 'Education', 1, 0))
EventBoys3 = EventBoys2 %>% group_by(pid) %>% mutate(Event = ifelse(StopEdu, 99, 0)) %>% mutate(Event = lag(Event, default = 0)) %>% df
EventBoys4 = filter(EventBoys3, Event != 99)

EventBoys3 %>% group_by(variable, value, Event) %>% summarise(n())
EventBoys4 %>% group_by(variable, value, StopEdu) %>% summarise(n())

library(survival)
edusurvBoys  = Surv(time = EventBoys4$variable, event = EventBoys4$StopEdu)
edusurvGirls = Surv(time = EventBoys4$variable, event = EventBoys4$StopEdu)

quartz()
par(mfrow = c(1,2))
par(xpd=F)

plot( survfit(edusurvGirls ~ 1), xlim = c(1986, 2010), xlab="Time", ylab="Survival Probability", main = "Girls", family = "Garamond", axes = F) 
axis(1, c(1986:2010), family = 'Garamond')
axis(2, c(0, 0.5, 1), family = 'Garamond')
abline(h = 0.5, col = 'pink')
plot( survfit(edusurvBoys ~ 1), xlim = c(1986, 2010), xlab="Time", ylab="Survival Probability", main = "Boys", family = "Garamond", axes = F)
axis(1, c(1986:2010), family = 'Garamond')
axis(2, c(0, 0.5, 1), family = 'Garamond')
abline(h = 0.5, col = 'pink')

edusurvfit = survfit(edusurv ~ 1, conf.type = 'log-log')
summary(edusurvfit)
plot(edusurvfit, xlab="Time", ylab="Survival Probability", xlim = c(1986, 2010))
abline(h = 0.5)
abline(v = 1992)

mean(edusurvfit$n.risk)

#######
#######
#######

# Girls  

# cultural 
library(car) # for the Anova function

EDU_girls = ifelse(dtgirlsMeanII$life_clusters == "Long_Education", 1, 0)
dtgirlsMeanII$class_rec = factor(dtgirlsMeanII$class_rec) 

m3 = glm(EDU_girls ~ Culturalleisure + class_rec, family = binomial, data = dtgirlsMeanII)
m4 = glm(EDU_girls ~ Culturalleisure * class_rec, family = binomial, data = dtgirlsMeanII)

summary(m3)
summary(m4)

nd = data.frame( expand.grid(Culturalleisure = c(0:400), class_rec = c('I+II', 'IV+V+missing') ))     
dta = cbind(nd, predicted_proba = predict(m3, type="response", newdata = nd))

ggplot(data=dta, aes(x= Culturalleisure , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() +
  geom_point()

nd = data.frame( expand.grid(Culturalleisure = c(0:400), class_rec = c('I+II', 'IV+V+missing') ))     
dta = cbind(nd, predicted_proba = predict(m4, type="response", newdata = nd))

# quartz()
ggplot(data=dta, aes(x= Culturalleisure , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() + ylab('Predicted Proba') + ylim(0,0.6) + 
  geom_point() + theme_minimal() + ggtitle("Girls - Impact of Cultural leisure consumption - Proba")

Anova(m4)

quartz()
plot(effect(term = 'Culturalleisure:class_rec', mod = m4, xlevels=list(Culturalleisure=0:400)), multiline=TRUE, ylab="Probability(released)", rug=T)

# study 

EDU_girls = ifelse(dtgirlsMeanII$life_clusters == "Long_Education", 1, 0)
dtgirlsMeanII$class_rec = factor(dtgirlsMeanII$class_rec)

m3 = glm(EDU_girls ~ SchoolStudy + class_rec, family = binomial, data = dtgirlsMeanII)
m4 = glm(EDU_girls ~ SchoolStudy * class_rec, family = binomial, data = dtgirlsMeanII)

summary(m3)
summary(m4)

nd = data.frame( expand.grid(SchoolStudy = c(0:500), class_rec = c('I+II', 'IV+V+missing') ))     
dta = cbind(nd, predicted_proba = predict(m3, type="response", newdata = nd))

ggplot(data=dta, aes(x= SchoolStudy , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() +
  geom_point()

TimeClock(500)

nd = data.frame( expand.grid(SchoolStudy = c(0:500), class_rec = c('I+II', 'III', 'IV+V+missing') ))     
dta = cbind(nd, predicted_proba = predict(m4, type="response", newdata = nd))

# quartz()
ggplot(data=dta, aes(x= SchoolStudy , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() +
  geom_point()

Anova(m4)

quartz()
plot(effect(term = 'Culturalleisure:class_rec', mod = m4, xlevels=list(Culturalleisure=0:200)), multiline=TRUE, ylab="Probability(released)", rug=T)


# Boys   
library(car) # for the Anova function
EDU_boys = ifelse(dtboysMeanII$life_clusters == "Long_Education", 1, 0)
dtboysMeanII$class_rec = factor(dtboysMeanII$class_rec)

m3_boys = glm(EDU_boys ~ Culturalleisure + class_rec, family = binomial, data = dtboysMeanII)
m4_boys = glm(EDU_boys ~ Culturalleisure * class_rec, family = binomial, data = dtboysMeanII)

summary(m3_boys)
summary(m4_boys)

Anova(m4_boys)

nd = data.frame( expand.grid(Culturalleisure = c(0:400), class_rec = c('I+II', 'IV+V+missing') ))     
dta_boys = cbind(nd, predicted_proba = predict(m3_boys, type="response", newdata = nd))

ggplot(data=dta_boys, aes(x= Culturalleisure , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() +
  geom_point() + ylab('Predicted Proba') + 
  geom_point() + theme_minimal() + ggtitle("Girls - Impact of Cultural leisure consumption - Proba")

nd = data.frame( expand.grid(Culturalleisure = c(0:400), class_rec = c('I+II', 'IV+V+missing') ))     
dta = cbind(nd, predicted_proba = predict(m4_boys, type="response", newdata = nd))

quartz()
ggplot(data=dta, aes(x= Culturalleisure , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() + ylab('Predicted Proba') + 
  geom_point() + theme_minimal() + ggtitle("Girls - Impact of Cultural leisure consumption - Proba")

Anova(m4)

quartz()
plot(effect(term = 'Culturalleisure:class_rec', mod = m4, xlevels=list(Culturalleisure=0:200)), multiline=TRUE, ylab="Probability(released)", rug=T)

m4_boys = glm(EDU_boys ~ SchoolStudy * class_rec, family = binomial, data = dtboysMeanII)

nd = data.frame( expand.grid(SchoolStudy = c(0:600), class_rec = c('I+II', 'IV+V+missing') ))     
dta = cbind(nd, predicted_proba = predict(m4_boys, type="response", newdata = nd))

ggplot(data=dta, aes(x= SchoolStudy , y=predicted_proba, group=class_rec, colour = class_rec)) + 
  geom_line() + ylab('Predicted Proba') + 
  geom_point() + theme_minimal() + ggtitle("Girls - Impact of Cultural leisure consumption - Proba")

##

table(dtboysMeanII$class_rec, dtboysMeanII$Culturalleisure)
plot( dtboysMeanII$Culturalleisure, dtboysMeanII$life_clusters == 'Long_Education', xlim = c(0,500))

##

# highest quantiles 
table(Quant(dtboysMeanII$Culturalleisure), dtboysMeanII$Culturalleisure)
# 248 minutes - 1710 minutes 
# 789 - 0 

table(Quant(dtboysMeanII$Culturalleisure), dtboysMeanII$Culturalleisure, dtboysMeanII$class_rec)

TimeClock(1710) 

# highest quantiles 
table(Quant(dtgirlsMeanII$Culturalleisure), dtgirlsMeanII$Culturalleisure)
# 250 minutes - 1530 minutes 
# 762 

table(dtboysMeanII$class_rec, CulturalleisureQ_boys) 
table(dtgirlsMeanII$class_rec, CulturalleisureQ_girls) 

aggregate(Culturalleisure ~ class_rec, data = dtboysMeanII, FUN = max)
aggregate(Culturalleisure ~ class_rec, data = dtboysMeanII, FUN = mean)
table(ifelse(dtboysMeanII$Culturalleisure == 0, 0, 1), dtboysMeanII$class_rec)

aggregate(Culturalleisure ~ class_rec, data = dtgirlsMeanII, FUN = max)
aggregate(Culturalleisure ~ class_rec, data = dtgirlsMeanII, FUN = mean)
table(ifelse(dtgirlsMeanII$Culturalleisure == 0, 0, 1), dtgirlsMeanII$class_rec)

table(dtboysMeanII$class_rec, CulturalleisureQ_boys, dtboysMeanII$life_clusters == 'Long_Education') 
table(dtgirlsMeanII$class_rec, CulturalleisureQ_girls, dtgirlsMeanII$life_clusters == 'Long_Education') 

12 / 230 
24 / 88
#

cp(table(dtboysMeanII$class_rec, dtboysMeanII$life_clusters == 'Long_Education'), 1)
cp(table(dtgirlsMeanII$class_rec, dtgirlsMeanII$life_clusters == 'Long_Education'), 1)

meanMen = cbind( aggregate(Culturalleisure ~ class_rec, data = dtboysMeanII, FUN = mean), aggregate(SchoolStudy ~ class_rec, data = dtboysMeanII, FUN = mean) )
meanWomen = cbind( aggregate(Culturalleisure ~ class_rec, data = dtgirlsMeanII, FUN = mean), aggregate(SchoolStudy ~ class_rec, data = dtgirlsMeanII, FUN = mean) )

write.csv(meanMen, file = '/Users/giacomovagni/Desktop/meanMen.csv')
write.csv(meanWomen, file = '/Users/giacomovagni/Desktop/meanWomen.csv')

boxplot(Culturalleisure ~ class_rec, data = dtboysMeanII, ylim = c(0,500))
boxplot(SchoolStudy ~ class_rec, data = dtboysMeanII, ylim = c(0,100))

cp(table(dtboysMeanII$class_rec, dtboysMeanII$life_clusters), 1)
chisq.test(table(dtboysMeanII$class_rec, dtboysMeanII$life_clusters))$stdres
# standardized residuals, (observed - expected) / sqrt(V), where V is the residual cell variance 

chisq.test(table(dtboysMeanII$class_rec, dtboysMeanII$life_clusters == 'Long_Education'))$stdres
cp(table(dtgirlsMeanII$class_rec, dtgirlsMeanII$life_clusters == 'Long_Education'), 1)
cp(table(dtboysMeanII$class_rec, dtboysMeanII$life_clusters == 'Long_Education'), 1)

