
# 2015 10 24 data prep bcs_presentation 

######## 
library(mtusRlocal) # load the package 
# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(knitr)
library(RColorBrewer)
library(TraMineR)
library(foreign)
library(ggplot2)
# library(stargazer)
########
source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')
######## 
tb = function(x) table(x)
df = function(x) as.data.frame(x)
colnumber = function(x) t(data.frame(names(x))) 
recode_modalities <- function(variable, new_variable, var) { LeVelS <- levels(var); cat(paste(new_variable , '[', variable , '== "',LeVelS,'"', '] <- ', '\n',sep="")) }
######## 

# 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dataWide3.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dta3.RData')
# 

n_distinct(dataWide3$pid)
n_distinct(dta3$pid)

########
# boys day sequences 
########
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaBoysDaysClust.RData') 

########
# girls day sequences 
########
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaGirlsDaysClust.RData') 

# 
source('/Users/giacomovagni/Rprojects/analysis_bcs/sourceglmsummary.R')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtaBoysDaysClust2.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtaGirlsDaysClust2.RData')
library(fmsb)
#

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/LifeGirlsClusters_rec.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/LifeBoysClusters_rec.RData') 

# recode_modalities('dataWide3$BD2SOC', new_variable = "dataWide3$BD2SOC_rec", var = dataWide3$BD2SOC) 
# table(dataWide3$BD4PSOC, dataWide3$BD2SOC)

dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "info missing"] <- "missing"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "student/voluntary work"] <- "missing"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "V unskilled"] <- "V unskilled"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "IV partly-skilled"] <- "IV partly-skilled"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "III manual"] <- "III manual"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "III non manual"] <- "III non manual"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "II managerial and Technical"] <- "II managerial and Technical"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "I professional"] <- "I professional"

# missing 
dataWide3$BD2SOC_rec[is.na(dataWide3$BD2SOC_rec)] = "missing"
# sex factor 
dataWide3$SEX_rec = factor(dataWide3$SEX, labels = c('boys', 'girls'))
#  

dta3$withwhom[dta3$WHOWITH_orig== "1"] <- "partner"
dta3$withwhom[dta3$WHOWITH_orig== "10"] <- "partner"
dta3$withwhom[dta3$WHOWITH_orig== "11"] <- ""
dta3$withwhom[dta3$WHOWITH_orig== "12"] <- "siblings"
dta3$withwhom[dta3$WHOWITH_orig== "13"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "14"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "15"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "16"] <- "with children"
dta3$withwhom[dta3$WHOWITH_orig== "17"] <- "lover"
dta3$withwhom[dta3$WHOWITH_orig== "18"] <- "one friend"
dta3$withwhom[dta3$WHOWITH_orig== "19"] <- "more than one friend"
dta3$withwhom[dta3$WHOWITH_orig== "2"] <- "with children"
dta3$withwhom[dta3$WHOWITH_orig== "20"] <- "neighbours"
dta3$withwhom[dta3$WHOWITH_orig== "21"] <- "colleagues"
dta3$withwhom[dta3$WHOWITH_orig== "22"] <- "voluntary group"
dta3$withwhom[dta3$WHOWITH_orig== "24"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "25"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "26"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "27"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "28"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "29"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "3"] <- "siblings"
dta3$withwhom[dta3$WHOWITH_orig== "30"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "31"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "32"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "33"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "34"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "35"] <- "parents"
dta3$withwhom[dta3$WHOWITH_orig== "36"] <- "alone"
dta3$withwhom[dta3$WHOWITH_orig== "4"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "5"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "6"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "7"] <- "misc"
dta3$withwhom[dta3$WHOWITH_orig== "8"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "9"] <- "misc"

##  

dataWide3$y1986_rec[dataWide3$y1986== "Education"] <- "Education"
  dataWide3$y1986_rec[dataWide3$y1986== "Full Time"] <- "Working"
  dataWide3$y1986_rec[dataWide3$y1986== "Looking after home family"] <- "Looking after home family"
  dataWide3$y1986_rec[dataWide3$y1986== "Part time"] <- "Working"
  dataWide3$y1986_rec[dataWide3$y1986== "Retired"] <- "other"
  dataWide3$y1986_rec[dataWide3$y1986== "Self Emp"] <- "Working"
  dataWide3$y1986_rec[dataWide3$y1986== "Sick "] <- "other"
  dataWide3$y1986_rec[dataWide3$y1986== "Unemployed "] <- "Unemployed"
  dataWide3$y1986_rec[dataWide3$y1986== "bad"] <- "other"
  dataWide3$y1986_rec[dataWide3$y1986== "other"] <- "other"

## 

dta3$SEX_rec = ifelse(dta3$SEX == '1', 'boys', 'girls')
dta3$month_rec = ifelse(dta3$month == '6', 'a june', ifelse(dta3$month == '7', 'b july', 'c august'))

### 

count(dta3, SEX)
count(distinct(dta3, pid), SEX)

select(dta3, diaryday, act_rec, SEX, duration) %>% left_join(., count(distinct(dta3, pid), SEX)) %>% group_by(diaryday, act_rec, SEX, n) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = sdur / n) %>% group_by(SEX) %>% mutate(sum(meanduration)) 

########## 
########## 

# day1
# filter(dta3, diaryday == 1)
dta = select(dta3, id = id, av = act_rec, time = duration)
dta = as.data.frame(dta)

seqDay = dta[rep(1:nrow(dta), dta[,'time'] ), -3] %>%
  group_by(id) %>% 
  mutate( Time = 1:n() ) 

seqDay

# unique(nchar(seqDay$id) ) 
seqDay$diaryday = substr(seqDay$id, start = 8, stop = 8) 
seqDay$pid = substr(seqDay$id, start = 1, stop = 7) 

seqDay = seqDay %>% group_by()
head(seqDay)

seqDay2 = select(seqDay, -id, -pid)
seqDay3 = seqDay2 %>% group_by(Time, diaryday, av) %>% summarise(n= n()) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)

kol = brewer.pal(12, 'Set3')
ggplot( filter(seqDay3, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() # + facet_grid(.~y1986_rec, drop = T)

############################
####### by gender ########## 
############################

seqDay$diaryday = substr(seqDay$id, start = 8, stop = 8) 
seqDay$pid = substr(seqDay$id, start = 1, stop = 7) 

seqDay = seqDay %>% group_by()
head(seqDay)

dtasex = dataWide3[, c('pid', 'SEX_rec')] 

seqDaysex = full_join(dtasex, seqDay, by = 'pid') 
count(distinct(seqDaysex, pid), SEX_rec) 

seqDay2sex = select(seqDaysex, -id, -pid)
# boys 
seqDay2boys = filter(seqDay2sex, SEX_rec == 'boys') 
seqDay2boys = seqDay2boys %>% group_by(Time, diaryday, av) %>% summarise(n= n()) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)
# girls 
seqDay2girls = filter(seqDay2sex, SEX_rec == 'girls') 
seqDay2girls = seqDay2girls %>% group_by(Time, diaryday, av) %>% summarise(n= n()) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)

### 
kol = brewer.pal(12, 'Set3')

ggplot( filter(seqDay2boys, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() # + facet_grid(.~y1986_rec, drop = T)
ggplot( filter(seqDay2girls, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() # + facet_grid(.~y1986_rec, drop = T)

# bind and separate 
seqDay_gender = rbind(cbind(seqDay2boys, gender = 'boys'), cbind(seqDay2girls, gender = 'girls')) 
ggplot( filter(seqDay_gender, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~gender, drop = T)

############################
####### by employment and gender 
############################

dtasocio = dataWide3[, c('pid', 'y1986_rec', 'SEX_rec')] 

seqDay_socio = full_join(dtasocio, seqDay, by = 'pid') 
table(seqDay_socio$SEX_rec) 

names(seqDay_socio)
table(seqDay_socio$SEX_rec)
count(distinct(seqDay_socio, pid), SEX_rec, y1986_rec) %>% group_by(SEX_rec) %>% mutate(sum(n))

dtaboys = filter(seqDay_socio, SEX_rec == 'boys')
dtagirls = filter(seqDay_socio, SEX_rec == 'girls')

count(distinct(dtaboys, pid), SEX_rec, y1986_rec)
count(distinct(dtagirls, pid), SEX_rec, y1986_rec)

dtaboys = dtaboys %>% group_by()
dtagirls = dtagirls %>% group_by()

#seqDay2_boys = dtaboys %>% left_join(., count(distinct(dtaboys, pid), SEX_rec, y1986_rec)) %>% group_by(Time, av, diaryday, SEX_rec, y1986_rec, n) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100)
#seqDay2_girls = dtagirls %>% left_join(., count(distinct(dtagirls, pid), SEX_rec, y1986_rec)) %>% group_by(Time, av, diaryday, SEX_rec, y1986_rec, n) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100)

#########
# Boys 
#########

table(factor(dtaboys$y1986_rec))
# select 
dtaboys = select(dtaboys, -pid, -id)
head(dtaboys)

# Education 
dtaboysEdu = filter(dtaboys, y1986_rec == "Education") %>% select(-y1986_rec)
dtaboysEdu = dtaboysEdu %>% group_by(Time, diaryday, av) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100) 

# Full Time 
dtaboysWorking = filter(dtaboys, y1986_rec == "Working") %>% select(-y1986_rec)
dtaboysWorking = dtaboysWorking %>% group_by(Time, diaryday, av) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100) 

# Unemployed 
dtaboysUnemployed = filter(dtaboys, y1986_rec == "Unemployed") %>% select(-y1986_rec)
dtaboysUnemployed = dtaboysUnemployed %>% group_by(Time, diaryday, av) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100) 

# 
ggplot( filter(dtaboysUnemployed, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + ggtitle(paste("n:", unique(dtaboysUnemployed$tot) )) # + facet_grid(.~y1986_rec, drop = T)
ggplot( filter(dtaboysWorking, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + ggtitle(paste("n:", unique(dtaboysWorking$tot) )) # + facet_grid(.~y1986_rec, drop = T)
ggplot( filter(dtaboysEdu, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + ggtitle(paste("n:", unique(dtaboysEdu$tot) )) # + facet_grid(.~y1986_rec, drop = T) 
# 

#########
# Girls  
#########

table(factor(dtagirls$y1986_rec))
# select 
dtagirls = select(dtagirls, -pid, -id)
head(dtagirls)

# Education 
dtagirlsEdu = filter(dtagirls, y1986_rec == "Education") %>% select(-y1986_rec)
dtagirlsEdu = dtagirlsEdu %>% group_by(Time, diaryday, av) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100) 

# Full Time 
dtagirlsWorking = filter(dtagirls, y1986_rec == "Working") %>% select(-y1986_rec)
dtagirlsWorking = dtagirlsWorking %>% group_by(Time, diaryday, av) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100) 

# Unemployed 
dtagirlsUnemployed = filter(dtagirls, y1986_rec == "Unemployed") %>% select(-y1986_rec)
dtagirlsUnemployed = dtagirlsUnemployed %>% group_by(Time, diaryday, av) %>% summarise(nsum = n()) %>% mutate(tot = sum(nsum)) %>% mutate(SharePerHour= round( nsum / tot, 4) * 100) 

# 
ggplot( filter(dtagirlsUnemployed, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + ggtitle(paste("n:", unique(dtagirlsUnemployed$tot) )) # + facet_grid(.~y1986_rec, drop = T)
ggplot( filter(dtagirlsWorking, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + ggtitle(paste("n:", unique(dtagirlsWorking$tot) )) # + facet_grid(.~y1986_rec, drop = T)
ggplot( filter(dtagirlsEdu, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + ggtitle(paste("n:", unique(dtagirlsEdu$tot) )) # + facet_grid(.~y1986_rec, drop = T) 
# 

######
# bind and separate 
######

# Edu 
seqDay_genderEDU = rbind(cbind(dtaboysEdu, gender = 'boys'), cbind(dtagirlsEdu, gender = 'girls')) 
ggplot( filter(seqDay_genderEDU, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~gender, drop = T)

# Working  
seqDay_genderWorking = rbind(cbind(dtaboysWorking, gender = 'boys'), cbind(dtagirlsWorking, gender = 'girls')) 
ggplot( filter(seqDay_genderWorking, diaryday ==  1), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~gender, drop = T)

########################################
########################################
########################################

save(seqDay3, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay3.RData') 
save(seqDay_gender, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay_gender.RData') 
save(seqDay_genderEDU, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay_genderEDU.RData') 
save(seqDay_genderWorking, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay_genderWorking.RData') 

load(file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay3.RData') 
load(file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay_gender.RData') 
load(file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay_genderEDU.RData') 
load(file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/seqDay_genderWorking.RData') 

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

library(TraMineR)

lifecourse = dataWide3[, grepl('y', names(dataWide3))] 
lifecourse$gender = dataWide3$SEX_rec 
lifecourse$class = dataWide3$BD2SOC_rec 
lifecourse$pid = dataWide3$pid 

lifecourse = lifecourse[, -26]

kollife = brewer.pal(10, 'Set3')
lifecourse_seq  = seqdef(lifecourse [,-c(26:28)], cpal = kollife)

lab = attributes(lifecourse_seq)$labels

# theocosts  <- matrix(1, nrow = 10, ncol = 10, byrow = TRUE, dimnames = list(lab, lab) )
dist_boys = seqdist(lifecourse_seq[which(lifecourse$gender == 'boys'), ], method = "OM", sm = "TRATE", with.missing=TRUE) 
dist_girls = seqdist(lifecourse_seq[which(lifecourse$gender == 'girls'), ], method = "OM", sm = "TRATE", with.missing=TRUE)

plot ( hclust(as.dist(dist_boys), method = "ward")  )
plot ( hclust(as.dist(dist_girls), method = "ward")  )

clust_boys = cutree( hclust(as.dist(dist_boys), method = "ward"), 5) 
clust_girls = cutree( hclust(as.dist(dist_girls), method = "ward"), 6) 

##
##

clust_boys = factor(clust_boys, labels = c("Long_Education", "Full-Time", "Full-Self-Full", "Sick/Unemployed", "Trans-Self-Emp"))
clust_girls = factor(clust_girls, labels = c("Looking_After_Home", "Part-Time", "Full-Time", "Full-Part-Full", "Long_Education", "Trans-Self-Emp"))

##
##

quartz()
seqdplot(lifecourse_seq[which(lifecourse$gender == 'boys'), ], group = clust_boys, border = NA)
seqdplot(lifecourse_seq[which(lifecourse$gender == 'girls'), ], group = clust_girls, border = NA)

seqIplot(lifecourse_seq[which(lifecourse$gender == 'boys'), ], group = clust_boys, border = NA)
seqIplot(lifecourse_seq[which(lifecourse$gender == 'girls'), ], group = clust_girls, border = NA)

##  
## 

lifecourse[which(lifecourse$gender == 'boys'), 'life_clusters'] = as.character(clust_boys)
lifecourse[which(lifecourse$gender == 'girls'), 'life_clusters'] = as.character(clust_girls)

# save(lifecourse, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/data/lifecourse.RData')

#######
# Gender 
#######
lifecourse_pp = lifecourse %>% melt(id.vars = c("gender", "class"))
head(lifecourse_pp)

lifecourse_ppSum = lifecourse_pp %>% group_by(gender, value, variable) %>% summarise(nact = n()) %>% group_by(gender, variable) %>% mutate(EmpStatus = nact / sum(nact)) 
lifecourse_ppSum$variable = as.numeric( substr(lifecourse_ppSum$variable, start = 2, stop = nchar(as.character(lifecourse_ppSum$variable[1])) ) )

ggplot(lifecourse_ppSum, aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~gender, drop = T) 

# save(lifecourse_ppSum, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/lifecourse_ppSum.RData') 


#######
# Gender X clusters 
#######
lifecourse_ppClust = lifecourse %>% melt(id.vars = c("gender", "class", "life_clusters"))
head(lifecourse_ppClust)

lifecourse_ppSumClust = lifecourse_ppClust %>% group_by(gender, value, variable, life_clusters) %>% summarise(nact = n()) %>% group_by(gender, variable, life_clusters) %>% mutate(EmpStatus = nact / sum(nact)) 
lifecourse_ppSumClust$variable = as.numeric( substr(lifecourse_ppSumClust$variable, start = 2, stop = nchar(as.character(lifecourse_ppSumClust$variable[1])) ) )

ggplot(filter(lifecourse_ppSumClust, gender == 'boys'), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(life_clusters~., drop = T) 
ggplot(filter(lifecourse_ppSumClust, gender == 'girls'), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters~., drop = T) 

# save(lifecourse_ppSumClust, file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/2015-10-30-App-BCS-time-use/data/lifecourse_ppSumClust.RData') 

ggplot(lifecourse_ppSum, aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~gender, drop = T) 

library(reshape2)
library(ggplot2)

m_dta <- melt(dta, id.var="id")
m_dta

load(file = '/Users/giacomovagni/Rprojects/bcs_presentation_update/bcs_presentation_oct_2015/data/lifecourse.RData')
head(lifecourse)

lcp = select(lifecourse, -class) %>% melt(id.vars = c("pid", "gender", "life_clusters")) %>% group_by()
lcp

kol = brewer.pal(12, 'Set3')
ggplot(lcp %>% filter(gender == 'girls' & life_clusters == "Looking_After_Home"), aes(x=variable, y=pid, fill=value)) + geom_tile() + theme_minimal() + scale_fill_manual(values=kol) + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank())

ggplot(lcp %>% filter(gender == 'girls' & life_clusters == value), aes(x=variable, y=pid, fill=value)) + geom_tile() + theme_minimal() + scale_fill_manual(values=kol) + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), panel.grid.major=element_blank(), panel.border=element_blank())

value = "Education"


value 
p1 = ggplot(filter(lifecourse_ppSumClust, gender == 'boys' & variable %in% c(x) & life_clusters %in% c() ), aes(x = variable, fill = factor(value), y = EmpStatus)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~life_clusters, drop = T) 


  
