##data cleaning 

library(dplyr)
##Randomization
datrdm<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/rdm01.txt", fill=TRUE,header = TRUE)
arm <- datrdm[-c(1),]
arm<-select(arm,4,42)%>% 
  rename(SUBID=src_subject_id)
arm[,1:2] <- lapply(arm[,1:2], function(x) as.numeric(as.character(x)))
arm<-arrange(arm,SUBID) ##1=Escitalopram Plus Placebo; 2=Sustained-Release Bupropion Plus Escitalopram; 3=Extended-Release Venlafaxine Plus Mirtazapine

datsite<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/et01.txt", fill=TRUE,header = TRUE)
head(datsite)

arm <- datrdm[-c(1),]
arm<-select(arm,4,42)%>% 
  rename(SUBID=src_subject_id)
arm[,1:2] <- lapply(arm[,1:2], function(x) as.numeric(as.character(x)))
arm<-arrange(arm,SUBID)

##DEMOG
##select variable GUID, subid,age,gender,ethnicity,race variables
datdm<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/demogr01.txt", fill=TRUE, na.strings=c("","NA"))
dat1_1<-datdm[-c(1,2,1311,1312), ]##remove head and tail description
dat1_1<-select(dat1_1,3:4,6:7,43:44) %>% 
  rename(GUID=V3,SUBID=V4,AGE=V6,GENDER=V7,ETHNICITY=V43,RACE=V44)
dat1_1<-dat1_1[!(is.na(dat1_1$ETHNICITY)) | !(is.na(dat1_1$RACE)),]
dat1_1[,2:3]<- lapply(dat1_1[,2:3], function(x) as.numeric(as.character(x)))
dat1_1<-arrange(dat1_1,SUBID)

##select variable GUID, subid,educat,income variables
datdm<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/demogr01.txt", fill=TRUE,na.strings=c("","NA"))
dat1_2<-datdm[-c(1,2,1311,1312), ] ##remove head and tail description
dat1_2<-select(dat1_2,4,97,99) %>% 
  rename(SUBID=V4,EDU=V97,income=V99)
dat1_2<-dat1_2[!(is.na(dat1_2$EDU)) | !(is.na(dat1_2$income)),]
dat1_2[,1:3]<- lapply(dat1_2[,1:3], function(x) as.numeric(as.character(x)))
dat1_2<-arrange(dat1_2,SUBID)

##HAM-D
##select variable GUID, subid, anxiety 6 items and total score
datham<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/hrsd01.txt", fill=TRUE,header = TRUE)
dat2 <- datham[-c(1), ] ##remove head and tail description
dat2<-select(dat2,3:4,16:17,20:21,34,39,46) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id)
dat2[,2:7] <- lapply(dat2[,2:7], function(x) as.numeric(as.character(x)))
dat2<-mutate(dat2,anx_sum=hpanx+hinsg+hsanx+hhypc) ##calculate sum of 4 items (except hamd_10 and hamd_18)
dat2 <- dat2[!duplicated(dat2$SUBID), ]## remove the duplicated subid

##IDSC
##select variable GUID, subid, anxiety 6 items and total score; reactivity of mood, hypersomnia, appetite, weight,interpersonal sensitivity, leaden paralysis)
datidsc<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/idsc01.txt", fill=TRUE,header = TRUE)
dat3 <- datidsc[-c(1), ] ##remove head and tail description
dat3<-select(dat3,3:4,11,47:48,23,18,29,31,45:46) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  filter(days_baseline=="0"|days_baseline=="-1"|days_baseline=="-2"|days_baseline=="-3") ##define baseline visit as day -3,-2,-1,0
dat3[,2:11] <- lapply(dat3[,2:11], function(x) as.numeric(as.character(x)))
dat3<-mutate(dat3,indirct= factor(irct>=0 & irct<=2,labels = c("0", "1")),indwg= factor(iwtin>=2 & iwtin<=3,labels = c("0", "1")),indap= factor(iapin>=2 & iapin<=3,labels = c("0", "1")), indlp= factor(ildn>=2 & ildn<=3,labels = c("0", "1")), indhsm= factor(ihysm>=2 & ihysm<=3,labels = c("0", "1")),indintp= factor(iintp==3,labels = c("0", "1"))) # the criteria required a score of 0–2 to indicate mood reactivity, 2–3 to indicate leaden paralysis, 2–3 to indicate weight gain or increased appetite, 2–3 to indicate hypersomnia, and 3 to indicate interpersonal sensitivity.
dat3[,12:17] <- lapply(dat3[,12:17], function(x) as.numeric(as.character(x)))
dat3<-mutate(dat3,subsum=factor(indwg>0|indap>0,labels=c("0","1"))) # increased appetite or increased weight
dat3[,17:18] <-lapply(dat3[,17:18], function(x) as.numeric(as.character(x))) 
dat3<-mutate(dat3,sum=rowSums(dat3[15:18])) # the sum of the following symptoms: hypersomnia, increased appetite or increased weight, interpersonal rejection sensitivity, and leaden paralysis.
dat3$atyp<-ifelse((dat3$indirct==1)&(dat3$sum>1),1,0) ## Atypical features (IDS-C): the symptom of mood reactivity together with two or more of the following symptoms: hypersomnia, increased appetite or increased weight, interpersonal rejection sensitivity, and leaden paralysis.

### Melancholic features (IDS-C)
dat11 <- datidsc[-c(1), ] ##remove head and tail description
dat11<-select(dat11,3:4,11,23,37,27,24,40,41,28,30,33) %>% ##select variable GUID, subid, days since baseline; reactivity of mood, pleasure,quality of mood, diurnal mood variation, psychomotor retardation, psychomotor agitation,appetite decrease, weight decrease, self-outlook)
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  filter(days_baseline=="0"|days_baseline=="-1"|days_baseline=="-2"|days_baseline=="-3") ##define baseline visit as day -3,-2,-1,0
dat11[,4:12] <- lapply(dat11[,4:12], function(x) as.numeric(as.character(x)))
dat11<-mutate(dat11,indirct=factor(irct>=2 & irct<=3,labels = c("0", "1")),indplsr= factor(iplsr>=2 & iplsr<=3,labels = c("0", "1")),inqty= factor(iqty>=2 & iqty<=3,labels = c("0", "1")), indvrtn= factor(ivrtn>=2 & ivrtn<=3,labels = c("0", "1")), indslow= factor(islow>=2 & islow<=3,labels = c("0", "1")),indagit= factor(iagit>=2 & iagit<=3,labels = c("0", "1")), subsum=factor((iapdc>=2 & iapdc<=3)|(iwtdc>=2 &iwtdc<=3),labels = c("0", "1")), indvwsf=factor(ivwsf>=2 &ivwsf<=3,labels = c("0", "1"))) 
dat11[,13:20] <- lapply(dat11[,13:20], function(x) as.numeric(as.character(x)))
dat11<-mutate(dat11,sum=rowSums(dat11[15:20])) #  sum of the following criteria: quality of mood, diurnal mood variation, psychomotor retardation, psychomotor agitation, appetite decrease or weight decrease, or self-outlook.

## QIDS
##select visit_day and 16 items scores
##Sixteen items are used to rate the nine criterion symptom domains of a major depressive episode: 4 items are used to rate sleep disturbance (early, middle, and late insomnia plus hypersomnia); 2 items are used to rate psychomotor disturbance (agitation and retardation); 4 items are used to rate appetite/weight disturbance (appetite increase or decrease and weight increase or decrease). Only one item is used to rate the remaining 6 domains (depressed mood, decreased interest, decreased energy, worthlessness/guilt, concentration/decision making, and suicidal ideation). Each item is rated 0-3. For symptom domains that require more than one item, the highest score of the item relevant for each domain is taken. For example, if early insomnia is 0, middle insomnia is 1, late insomnia is 3, and hypersomnia is 0, the sleep disturbance domain is rated 3. The total score ranges from 0-27.

datqids<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/qids01.txt", fill=TRUE,header = TRUE)
dat4 <- datqids[-c(1), ] ##remove head and tail description
dat4<-select(dat4,3:4,8,43,10:13,15:18,24:25,14,19:23) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id)
dat4[,2:20] <- lapply(dat4[,2:20], function(x) as.numeric(as.character(x)))
dat4<-arrange(dat4,SUBID,week)
dat4[is.na(dat4)]<-0
dat4$sleep=apply(dat4[,5:8],1,max)##4 items are used to rate sleep disturbance (early, middle, and late insomnia plus hypersomnia)
dat4$aw=apply(dat4[,9:12],1,max)##4 items are used to rate appetite/weight disturbance (appetite increase or decrease and weight increase or decrease)
dat4$psych=apply(dat4[,13:14],1,max)##2 items are used to rate psychomotor disturbance (agitation and retardation)
dat4$qids_sum=apply(dat4[,15:23],1,sum) ##calculate sum of 9 domains

##Outcome-12week remission rate
##12week-remission rate: symptom remission, was based on the score on the 16-item Quick Inventory of Depressive Symptomatology—Self-Report (QIDS-SR) at 12 weeks. The designation of remission was based on the last two consecutive measurements during the 12-week acute-phase trial to ensure that a single “good week” was not falsely signaling remission. remission (At least one of these ratings had to be less than 6, while the other had to be less than 8. If participants exited before 12 weeks, their last two consecutive scores were used to determine remission. Those without two postbaseline measures were considered not to have remission.)

datqids<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/qids01.txt", fill=TRUE,header = TRUE)
dat4 <- datqids[-c(1), ] ##remove head and tail description
dat4<-select(dat4,3:4,8,43,10:13,15:18,24:25,14,19:23) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id)
dat4[,2:20] <- lapply(dat4[,2:20], function(x) as.numeric(as.character(x)))
dat4<-arrange(dat4,SUBID,week)
dat4[is.na(dat4)]<-0
dat4$sleep=apply(dat4[,5:8],1,max) ##4 items are used to rate sleep disturbance (early, middle, and late insomnJia plus hypersomnia)
dat4$aw=apply(dat4[,9:12],1,max) ##4 items are used to rate appetite/weight disturbance (appetite increase or decrease and weight increase or decrease)
dat4$psych=apply(dat4[,13:14],1,max) ##2 items are used to rate psychomotor disturbance (agitation and retardation)
dat4$qids_sum=apply(dat4[,15:23],1,sum)

dat4_2<-filter(dat4,week<=12)%>% ##only remain week<=12 for each subject
  select(2,4,24)
id<-unique(dat4_2$SUBID)
rem<-rep("0",length(id))
score<-rep("0",length(id))
week<-rep("0",length(id))
for (i in 1:length(id)){
  d_subject<-filter(dat4_2,SUBID==id[i])
  score[i]=d_subject[nrow(d_subject),3]##the last score before 12week
  week[i]=d_subject[nrow(d_subject),2]
  if (nrow(d_subject)<3){
    rem[i]=0
  }##Those without two postbaseline measures were considered not to have remission.
  else {
    temp<-d_subject[(nrow(d_subject)-1):nrow(d_subject),]##the last two consecutive measurements
    if(min(temp$qids_sum)<=5 & max(temp$qids_sum)<=7){
      rem[i]=1} ##At least one of these ratings had to be less than 6, while the other had to be less than 8.
    else{rem[i]=0}
  }
}
rem<-as.numeric(rem)
score<-as.numeric(score)
week<-as.numeric(week)
outcome<-data.frame(SUBID = id, rem =rem, score=score, week=week)
outcome1<-left_join(outcome, arm, by = "SUBID") ##merge arm and remmission rate by SUBID

##Altman
##select 5 items score and baseline_day
datasrm<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/asrm01.txt", fill=TRUE,header = TRUE)
dat5 <- datasrm[-c(1), ] ##remove head and tail description
dat5<-select(dat5,3:4,8:12,18) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  filter(days_baseline=="0"|days_baseline=="-1"|days_baseline=="-2"|days_baseline=="-3")%>% ##define baseline visit as day -3,-2,-1,0
  arrange(SUBID)
dat5[,2:7] <- lapply(dat5[,2:7], function(x) as.numeric(as.character(x)))
dat5<-mutate(dat5,atm_sum=rowSums(dat5[3:7])) ##calculate sum of 5 items

## cpfq: cognitive and physical functioning question
##select 7 items score and baseline_day
datcpfq<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/cpfq01.txt", fill=TRUE,header = TRUE)
dat6 <- datcpfq[-c(1), ] ##remove head and tail description
dat6<-select(dat6,3:4,11:17,9) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  filter(days_baseline=="0"|days_baseline=="-1"|days_baseline=="-2"|days_baseline=="-3")%>% ##define baseline visit as day -3,-2,-1,0
  arrange(SUBID)
dat6[,2:9] <- lapply(dat6[,2:9], function(x) as.numeric(as.character(x)))
dat6<-mutate(dat6,cpfq_sum=rowSums(dat6[3:9])) ##calculate sum of 7 items

##wsas: Work and Social Adjustment scale 
##select 5 items score and baseline_day
datwsas<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/wsas01.txt", fill=TRUE,header = TRUE)
dat8 <- datwsas[-c(1), ] ##remove head and tail description
dat8<-select(dat8,3:4,11:15,9) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  filter(days_baseline=="0"|days_baseline=="-1"|days_baseline=="-2"|days_baseline=="-3")%>% ##define baseline visit as day -3,-2,-1,0
  arrange(SUBID)
dat8[,2:7] <- lapply(dat8[,2:7], function(x) as.numeric(as.character(x)))
dat8<-mutate(dat8,wsas_sum=rowSums(dat8[3:7])) ##calculate sum of 5 items

##pdsq: Psychiatric Diagnostic Screening Questionnaire
##select all variables related to axis I disorders
datpdsq<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/pdsq01.txt", fill=TRUE,header = TRUE)
dat9 <- datpdsq[-c(1), ] ##remove head and tail description
dat9<-select(dat9,3:4,119:130,104:118,40:54,55:64,65:72,73:80,81:86,87:92,93:103,131:140,141:145,146:150) %>%  
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  arrange(SUBID)
dat9[,2:113] <- lapply(dat9[,2:113], function(x) as.numeric(as.character(x)))
dat9<-mutate(dat9,alch_sum=rowSums(dat9[3:8]),drug_sum=rowSums(dat9[9:14]),social_sum=rowSums(dat9[15:29]), trauma_sum=rowSums(dat9[30:44]), bulimia_sum=rowSums(dat9[45:54]), obsessive_sum=rowSums(dat9[55:62]),an_sum=rowSums(dat9[63:70]),fr_sum=rowSums(dat9[83:93]),wy_sum=rowSums(dat9[94:103]),ph_sum=rowSums(dat9[104:108]),wi_sum=rowSums(dat9[109:113]),alch=as.numeric((alch_sum>2)),fr=as.numeric((fr_sum>6)),bulimia=as.numeric(bulimia_sum>7),drug=as.numeric(drug_sum>1),wy=as.numeric(wy_sum>9),wi=as.numeric(wi_sum>4),obsessive=as.numeric(obsessive_sum>3),an=as.numeric(an_sum>6),trauma=as.numeric(trauma_sum>11),social=as.numeric(social_sum>9),ph=as.numeric(ph_sum>4)) 
dat9<-mutate(dat9,count=rowSums(dat9[125:135]))

##sacq: Self-Administered Comorbidity Questionnaire
datsacq<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/sacq01.txt", fill=TRUE,header = TRUE)
dat10 <- datsacq[-c(1), ] ##remove head and tail description
dat10<-select(dat10,3:4,8,11,14,17,23,29,35,38,41,44,47,50,20,26,32) %>% ##select 5 items score and baseline_day 
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  arrange(SUBID)
dat10[,2:17] <- lapply(dat10[,2:17], function(x) as.numeric(x)-2)
dat10[,11:16] <- lapply(dat10[,11:16], function(x) x+1)
dat10$sacq_count<-rowSums(dat10[,3:17]==1)

##sq: suicide questionnair
datsq<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/sq01.txt", fill=TRUE,header = TRUE)
dat12 <- datsq[-c(1), ] ##remove head and tail description
dat12<-select(dat12,3:4,38) %>% 
  rename(GUID=subjectkey,SUBID=src_subject_id)
dat12[,2] <- as.numeric(as.character(dat12[,2]))

##mini01: episode
datmini<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/mini01.txt", fill=TRUE)
dat13 <- datmini[-c(2), ] ##remove head and tail description
dat13<-select(dat13,3:4,6,115,126,118,116) %>% 
  rename(GUID=V3,SUBID=V4,age=V6,dage=V126,recdep=V118,chrdep=V116,MDE=V115) %>%
  arrange(SUBID)
dat13[,2:5] <- lapply(dat13[,2:5], function(x) as.numeric(as.character(x)))
dat13<-mutate(dat13,agediff=age/12-dage,age18=factor(dage<18,labels = c("0", "1")),dur2=factor(MDE>=24, label=c("0","1"))) #agediff=Years since first episode; age18=first episode before age 18; dur2=Chronic depression (indexepisode duration ≥2 years)

## negab: History of Neglect/Abuse
datnegab<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/negab01.txt", fill=TRUE,header = TRUE)
dat14 <- datnegab[-c(1), ] ##remove head and tail description
dat14<-select(dat14,3:4,13,15,17) %>% 
  rename(GUID=subjectkey,SUBID=src_subject_id)
dat14[,2]<-as.numeric(as.character(dat14[,2]))

##wpai: Work Productivity and Activity Impairment
datwpai<-read.table("/users/mrosen/Projects/NDCTDataSets/NIMH_NDCT_DATA_SETS/NCT00590863/wpai01.txt", fill=TRUE,header = TRUE)
dat15 <- datwpai[-c(1), ]
dat15<-select(dat15,3:4,9,11) %>% ##employment
  rename(GUID=subjectkey,SUBID=src_subject_id) %>%
  filter(days_baseline=="0"|days_baseline=="-1"|days_baseline=="-2"|days_baseline=="-3")
dat15[,2]<-as.numeric(as.character(dat15[,2]))

##Merge all the datasets
fdat<-left_join(dat1_1, dat1_2, by = "SUBID")
fdat<-left_join(fdat,arm,by="SUBID")
fdat<-left_join(fdat,outcome1[,1:2],by="SUBID")
fdat<-left_join(fdat,dat3[,c(2,4,5,20)],by="SUBID")
fdat<-left_join(fdat,dat4_1[,c(2,24)],by="SUBID")
fdat<-left_join(fdat,dat5[,c(2,9)],by="SUBID")
fdat<-left_join(fdat,dat6[,c(2,11)],by="SUBID")
fdat<-left_join(fdat,dat8[,c(2,9)],by="SUBID")
fdat<-left_join(fdat,dat9[,c(2,114:124,136)],by="SUBID")
fdat<-left_join(fdat,dat10[,c(2,18)],by="SUBID")
fdat<-left_join(fdat,dat12[,2:3],by="SUBID")
fdat<-left_join(fdat,dat13[,c(2,4:10)],by="SUBID")
fdat<-left_join(fdat,dat14[,2:5],by="SUBID")
fdat<-left_join(fdat,dat15[,c(2,4)],by="SUBID")
fdat<-left_join(fdat,dat2[,c(2,10)],by="SUBID")
fdat<-fdat[-1]

# transform some variables into binary, and put them into fdat
# 3 categorical variables
levels(fdat$RACE) <- c("3", "3", "2","3","3","3","3","3","1") #3-others; 2-black; 1-white
#levels(fdat$ETHNICITY) <- c("0","0","0","1")
fdat$count<-factor(fdat$count) # Number of comorbid axis I disorders (0,1,2,3,>3)
levels(fdat$count) <- c("0", "1", "2","3",">3",">3",">3",">3",">3",">3") 
fdat$sacq_count<-factor(fdat$sacq_count) # Number of comorbid axis III disordersd (0,1,2,3,>3)
levels(fdat$sacq_count) <- c("0", "1", "2","3",">3",">3",">3",">3",">3") 

fdat$anx_sum<-ifelse(fdat$anx_sum>=7,1,0)
fdat$sads<-ifelse(fdat$sads==7,1,0)
fdat$fr_sum<-ifelse(fdat$fr_sum>6,1,0)
fdat$alch_sum<-ifelse(fdat$alch_sum>2,1,0)
fdat$bulimia_sum<-ifelse(fdat$bulimia_sum>7,1,0)
fdat$drug_sum<-ifelse(fdat$drug_sum>1,1,0)
fdat$wy_sum<-ifelse(fdat$wy_sum>9,1,0)
fdat$wi_sum<-ifelse(fdat$wi_sum>4,1,0)
fdat$obsessive_sum<-ifelse(fdat$obsessive_sum>3,1,0)
fdat$an_sum<-ifelse(fdat$an_sum>6,1,0)
fdat$trauma_sum<-ifelse(fdat$trauma_sum>11,1,0)
fdat$social_sum<-ifelse(fdat$social_sum>9,1,0)
fdat$ph_sum<-ifelse(fdat$ph_sum>4,1,0)

# Adjusted all variables (Delete variables with more than 5 missing data and input other missing data with mode/median)

fdatm<-select(fdat,-c(1),-("EDU"),-("income"),-("sads"),-("sacq_count"),-("dur2"))
levels <- levels(fdatm$age18)
levels[length(levels) + 1] <- "0"
fdatm$age18 <- factor(fdatm$age18, levels = levels)
fdatm$age18[is.na(fdatm$age18)] <- "0"
fdatm$anx_sum[is.na(fdatm$anx_sum)] <- 0
fdatm$alch_sum[is.na(fdatm$alch_sum)] <- 0
fdatm$dage[is.na(fdatm$dage)] <- 20
fdatm$agediff[is.na(fdatm$agediff)] <- 15.5
fdatm$wsas_sum[is.na(fdatm$wsas_sum)] <- 28
fdatm$fr_sum[is.na(fdatm$fr_sum)] <- 0
fdatm$wy_sum[is.na(fdatm$wy_sum)] <- 0
fdatm$an_sum[is.na(fdatm$an_sum)] <- 0
fdatm$trauma_sum[is.na(fdatm$trauma_sum)] <- 0
fdatm$social_sum[is.na(fdatm$social_sum)] <- 0
fdatm$MDE[is.na(fdatm$MDE)] <- 24
levels1 <- levels(fdatm$count)
levels1[length(levels1) + 1] <- "0"
fdatm$count <- factor(fdatm$count, levels = levels1)
fdatm$count[is.na(fdatm$count)] <- "0"

#------------------------------------------------------------------------------------------------------------------------------------

# Regression Analysis
# Unadjusted model

# Group 1 vs 2
# unadjusted logistic regression
fdat12<-filter(fdatm,arm!=3)
unadjusted12 <- glm(rem ~ arm, data = fdat12, family = "binomial")
summary(unadjusted12)

# bootstrap to get standard error
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(unadjusted12))))
colnames(par_bootstrap) = names(coefficients(unadjusted12))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat12), size = nrow(fdat12), replace = TRUE)
  tempdata = fdat12[idx_rd, ]
  fit_temp = glm(rem ~ arm, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se12 = sd(par_bootstrap[, 2])
bt_ci12 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))

# Group 1 vs 2
# difference of risk proportion
fdat12<-filter(fdatm,arm!=3)
# bootstrap to get standard error
set.seed(123)
par_bootstrap = data.frame(A1=rep(NA,10000),A2=rep(NA,10000),Diff=rep(NA,10000))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat12), size = nrow(fdat12), replace = TRUE)
  tempdata = fdat12[idx_rd, ]
  par_bootstrap[k,1] = mean(tempdata$rem[which(tempdata$arm==1)])
  par_bootstrap[k,2] = mean(tempdata$rem[which(tempdata$arm==2)])
}
par_bootstrap[,3]=par_bootstrap[,2]-par_bootstrap[,1]
mean(par_bootstrap[,3])
sd(par_bootstrap[, 3])
var(par_bootstrap[,3])
quantile(par_bootstrap[, 3], probs = c(0.025, 0.975))


# Group 1 vs 3
# unadjusted logistic regression
fdat13<-filter(fdatm,arm!=2)
fdat13$arm<-ifelse(fdat13$arm==1,1,2)
unadjusted13 <- glm(rem ~ arm, data = fdat13, family = "binomial")
summary(unadjusted13)

# bootstrap to get standard error
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(unadjusted13))))
colnames(par_bootstrap) = names(coefficients(unadjusted13))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat13), size = nrow(fdat13), replace = TRUE)
  tempdata = fdat13[idx_rd, ]
  fit_temp = glm(rem ~ arm, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se13 = sd(par_bootstrap[, 2])
bt_ci13 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))

# difference of risk proportion
# bootstrap to get standard error
set.seed(123)
par_bootstrap = data.frame(A1=rep(NA,10000),A2=rep(NA,10000),Diff=rep(NA,10000))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat13), size = nrow(fdat13), replace = TRUE)
  tempdata = fdat13[idx_rd, ]
  par_bootstrap[k,1] = mean(tempdata$rem[which(tempdata$arm==1)])
  par_bootstrap[k,2] = mean(tempdata$rem[which(tempdata$arm==2)])
}
par_bootstrap[,3]=par_bootstrap[,2]-par_bootstrap[,1]
mean(par_bootstrap[,3])
sd(par_bootstrap[, 3])
var(par_bootstrap[,3])
quantile(par_bootstrap[, 3], probs = c(0.025, 0.975))


# Group 2 vs 3
# unadjusted logistic regression
fdat23<-filter(fdatm,arm!=1)
unadjusted23 <- glm(rem ~ arm, data = fdat23, family = "binomial")
summary(unadjusted23)

# bootstrap to get standard error
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(unadjusted23))))
colnames(par_bootstrap) = names(coefficients(unadjusted23))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat23), size = nrow(fdat23), replace = TRUE)
  tempdata = fdat23[idx_rd, ]
  fit_temp = glm(rem ~ arm, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se23 = sd(par_bootstrap[, 2])
bt_ci23 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))

# Group 2 vs 3
# difference of risk proportion
# bootstrap to get standard error
set.seed(123)
par_bootstrap = data.frame(A1=rep(NA,10000),A2=rep(NA,10000),Diff=rep(NA,10000))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat23), size = nrow(fdat23), replace = TRUE)
  tempdata = fdat23[idx_rd, ]
  par_bootstrap[k,1] = mean(tempdata$rem[which(tempdata$arm==2)])
  par_bootstrap[k,2] = mean(tempdata$rem[which(tempdata$arm==3)])
}
par_bootstrap[,3]=par_bootstrap[,2]-par_bootstrap[,1]
mean(par_bootstrap[,3])
sd(par_bootstrap[, 3])
var(par_bootstrap[,3])
quantile(par_bootstrap[, 3], probs = c(0.025, 0.975))

#------------------------------------------------------------------------------------------------------------------------------------

# Adjusted Imbalanced variables

# Group 1 vs 2
# Logistic regression
# Only adjusted for employment
fdat12<-filter(fdat,arm!=3)
adjusted12_1 <- glm(rem ~ arm+wpai01, data = fdat12, family = "binomial")
summary(adjusted12_1)
ci12<-confint(adjusted12_1)
round(ci12,3)
# Bootstrap to get standard error
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(adjusted12_1))))
colnames(par_bootstrap) = names(coefficients(adjusted12_1))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat12), size = nrow(fdat12), replace = TRUE)
  tempdata = fdat12[idx_rd, ]
  fit_temp = glm(rem ~ arm+wpai01, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se12 = sd(par_bootstrap[, 2])
bt_ci12 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))

# Group 1 vs 2
# Standardized estimator
# only adjust employment
fdat12<-filter(fdat,arm!=3)
fdat121<-select(fdat12,arm,rem,wpai01)
# Function to get standard estimator
stand.est = function(Y, A, W){
  # Creating the data frame
  data.used = data.frame(W, A, Y)
  # Fitting the logistic regression model
  log.reg = glm(Y ~., data = data.used, family = "binomial")
  # Creating dataset for calculating the predictions corresponding to
  # A = 1 and A = 0
  data.a.1 = data.used
  data.a.1$A = 1
  data.a.0 = data.used
  data.a.0$A = 0
  # Calculating the predictions
  pred.1 = predict.glm(log.reg, newdata =
                         data.a.1[, colnames(data.used) != "Y"]
                       , type = "response")
  pred.0 = predict.glm(log.reg, newdata =
                         data.a.0[, colnames(data.used) != "Y"]
                       , type = "response")
  res.gcomp = mean(pred.1) - mean(pred.0)
  return(res.gcomp)
}
stand.est(fdat121$rem,fdat121$arm,fdat121$wpai01)

# Function to calculate the variance estimator
fdat121<-select(fdat12,arm,rem,wpai01)%>%na.omit(fdat121)
set.seed(123)
var.stand.est = function(Y, A, W, n.boot){
  # Creating the data frame
  data.used = data.frame(W, A, Y)
  # Calculating the variance estimator
  boot.gcomp = rep(NA, n.boot)
  for(i in 1:n.boot){
    # Finding the bootstrap sample
    bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
    # Fitting the bootstrapped logistic regression model
    log.reg.bs = glm(Y ~., data = data.used[bs, ], family = "binomial")
    data.a.1 = data.used[bs, ]
    data.a.1$A = 1
    data.a.0 = data.used[bs, ]
    data.a.0$A = 0
    # Calculating the predictions
    p.1.bs = predict.glm(log.reg.bs, newdata =
                           data.a.1[, colnames(data.used) != "Y"]
                         , type = "response")
    p.0.bs = predict.glm(log.reg.bs, newdata =
                           data.a.0[, colnames(data.used) != "Y"]
                         , type = "response")
    # Calculating the bootstrap estimator
    boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
  }
  # Returning the estimator and variance estimator
  return(list(bootvar=var(boot.gcomp),bootse=sd(boot.gcomp),ci=quantile(boot.gcomp, probs = c(0.025, 0.975))))
}
var.stand.est(fdat121$rem,fdat121$arm,fdat121$wpai01,10000)


# Group 1 vs 3
# Logistic regression
# Adjusted for sex, baseline score on IDS-C, and baseline score on Work and Social Adjustment Scale
fdat13m<-filter(fdatm,arm!=2)
fdat13m$arm<-ifelse(fdat13m$arm==1,1,2)
adjusted13 <- glm(rem ~ arm+GENDER+ictot+wsas_sum, data = fdat13m, family = "binomial")
summary(adjusted13)
ci13<-confint(adjusted13)
round(ci13,3)
# bootstrap
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(adjusted13))))
colnames(par_bootstrap) = names(coefficients(adjusted13))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat13m), size = nrow(fdat13m), replace = TRUE)
  tempdata = fdat13m[idx_rd, ]
  fit_temp = glm(rem ~ arm+GENDER+ictot+wsas_sum, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se13 = sd(par_bootstrap[, 2])
bt_ci13 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))
round(bt_se13,3)
round(bt_ci13,3)

# Standardized estimator
# Adjusted for sex, baseline score on IDS-C, and baseline score on Work and Social Adjustment Scale.
stand.est(fdat13m$rem,fdat13m$arm,fdat13m$GENDER,fdat13m$ictot,fdat13m$wsas_sum)
var.stand.est(fdat13m$rem,fdat13m$arm,fdat13m$GENDER,fdat13m$ictot,fdat13m$wsas_sum,10000)

#------------------------------------------------------------------------------------------------------------------------------------

# Adjust ALL variables
# Group 1 vs 2
# Logistic regression
fdat12m<-filter(fdatm,arm==1|arm==2)
adjusted12m <- glm(rem ~ ., data = fdat12m, family = "binomial")
summary(adjusted12m)
ci12<-confint(adjusted12m)
round(ci12,3)
# Bootstrap
set.seed(123)
#par_bootstrap = array(0, c(10000, length(coefficients(adjusted12m))))
coef<-rep(NA,10000)
#colnames(par_bootstrap) = names(coefficients(adjusted12m))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat12m), size = nrow(fdat12m), replace = TRUE)
  tempdata = fdat12m[idx_rd, ]
  fit_temp = glm(rem ~., data = tempdata, family = "binomial")
  coef[k] = coefficients(fit_temp)["arm"]
}
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))

# Group 1 vs 2; 
# Standardized estimator
# Creating the data frame
fdat12m<-filter(fdatm,arm==1|arm==2)
data.used = fdat12m
# Fitting the logistic regression model
log.reg = glm(rem ~., data = data.used, family = "binomial")
# Creating dataset for calculating the predictions corresponding to
# Arm = 1 and Arm = 0
data.a.1 = data.used
data.a.1$arm = 1
data.a.0 = data.used
data.a.0$arm = 0
# Calculating the predictions
pred.1 = predict.glm(log.reg, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
pred.0 = predict.glm(log.reg, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
res.gcomp = mean(pred.1) - mean(pred.0)
res.gcomp

# Function to calculate the variance estimator
set.seed(123)
# Creating the data frame
data.used = fdat12m
boot.gcomp = rep(NA, 10000)
for(i in 1:10000){
  # Finding the bootstrap sample
  bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  # Fitting the bootstrapped logistic regression model
  log.reg.bs = glm(rem ~., data = data.used[bs, ], family = "binomial")
  data.a.1 = data.used[bs, ]
  data.a.1$arm = 1
  data.a.0 = data.used[bs, ]
  data.a.0$arm = 0
  # Calculating the predictions
  p.1.bs = predict.glm(log.reg.bs, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
  p.0.bs = predict.glm(log.reg.bs, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
  # Calculating the bootstrap estimator
  boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
}
# Returning the estimator and variance estimator
bootvar=var(boot.gcomp)
bootse=sd(boot.gcomp)
ci=quantile(boot.gcomp, probs = c(0.025, 0.975))


# Group 1 vs 3
# Logistic regression
fdat13m<-filter(fdatm,arm==1|arm==3)
fdat13m$arm<-ifelse(fdat13m$arm==1,1,2)
adjusted13m <- glm(rem ~ ., data = fdat13m, family = "binomial")
summary(adjusted13m)
ci13<-confint(adjusted13m)
round(ci13,3)
# Bootstrap
set.seed(123)
#par_bootstrap = array(0, c(10000, length(coefficients(adjusted12m))))
coef<-rep(NA,10000)
#colnames(par_bootstrap) = names(coefficients(adjusted12m))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat13m), size = nrow(fdat13m), replace = TRUE)
  tempdata = fdat13m[idx_rd, ]
  fit_temp = glm(rem ~., data = tempdata, family = "binomial")
  coef[k] = coefficients(fit_temp)["arm"]
}
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))


# Adjust ALL variables
# Group 1 vs 3; 
# Standardized estimator
# Creating the data frame
fdat13m<-filter(fdatm,arm==1|arm==3)
fdat13m$arm<-ifelse(fdat13m$arm==1,0,1)
data.used = fdat13m
# Fitting the logistic regression model
log.reg = glm(rem ~., data = data.used, family = "binomial")
# Creating dataset for calculating the predictions corresponding to
# A = 1 and A = 0
data.a.1 = data.used
data.a.1$arm = 1
data.a.0 = data.used
data.a.0$arm = 0
# Calculating the predictions
pred.1 = predict.glm(log.reg, newdata =
                       data.a.1[, colnames(data.used) != "rem"]
                     , type = "response")
pred.0 = predict.glm(log.reg, newdata =
                       data.a.0[, colnames(data.used) != "rem"]
                     , type = "response")
res.gcomp = mean(pred.1) - mean(pred.0)
res.gcomp

# Calculate the variance estimator
set.seed(123)
# Creating the data frame
data.used = fdat13m
boot.gcomp = rep(NA, 10000)
for(i in 1:10000){
  # Finding the bootstrap sample
  bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  # Fitting the bootstrapped logistic regression model
  log.reg.bs = glm(rem ~., data = data.used[bs, ], family = "binomial")
  data.a.1 = data.used[bs, ]
  data.a.1$arm = 1
  data.a.0 = data.used[bs, ]
  data.a.0$arm = 0
  # Calculating the predictions
  p.1.bs = predict.glm(log.reg.bs, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
  p.0.bs = predict.glm(log.reg.bs, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
  # Calculating the bootstrap estimator
  boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
}
# Returning the estimator and variance estimator
bootvar=var(boot.gcomp)
bootse=sd(boot.gcomp)
ci=quantile(boot.gcomp, probs = c(0.025, 0.975))

# Adjust ALL variables
# Group 2 vs 3
# Logistic regression
fdat23m<-filter(fdatm,arm==2|arm==3)
fdat23m$arm<-ifelse(fdat23m$arm==2,0,1)
adjusted23m <- glm(rem ~ ., data = fdat23m, family = "binomial")
summary(adjusted23m)
ci23<-confint(adjusted23m)
round(ci23,3)
# Bootstrap
set.seed(123)
coef<-rep(NA,10000)
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat23m), size = nrow(fdat23m), replace = TRUE)
  tempdata = fdat23m[idx_rd, ]
  fit_temp = glm(rem ~., data = tempdata, family = "binomial")
  coef[k] = coefficients(fit_temp)["arm"]
}
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))

# Group 2 vs 3; 
# Standardized estimator
# Creating the data frame
fdat23m<-filter(fdatm,arm==2|arm==3)
fdat23m$arm<-ifelse(fdat23m$arm==2,0,1)
data.used = fdat23m
# Fitting the logistic regression model
log.reg = glm(rem ~., data = data.used, family = "binomial")
# Creating dataset for calculating the predictions corresponding to
# Arm = 1 and Arm = 0
data.a.1 = data.used
data.a.1$arm = 1
data.a.0 = data.used
data.a.0$arm = 0
# Calculating the predictions
pred.1 = predict.glm(log.reg, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
pred.0 = predict.glm(log.reg, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
res.gcomp = mean(pred.1) - mean(pred.0)
res.gcomp

# Calculate the variance estimator
set.seed(123)
# Creating the data frame
data.used = fdat23m
boot.gcomp = rep(NA, 10000)
for(i in 1:10000){
  # Finding the bootstrap sample
  bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  # Fitting the bootstrapped logistic regression model
  log.reg.bs = glm(rem ~., data = data.used[bs, ], family = "binomial")
  data.a.1 = data.used[bs, ]
  data.a.1$arm = 1
  data.a.0 = data.used[bs, ]
  data.a.0$arm = 0
  # Calculating the predictions
  p.1.bs = predict.glm(log.reg.bs, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
  p.0.bs = predict.glm(log.reg.bs, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
  # Calculating the bootstrap estimator
  boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
}
# Returning the estimator and variance estimator
bootvar=var(boot.gcomp)
bootse=sd(boot.gcomp)
ci=quantile(boot.gcomp, probs = c(0.025, 0.975))
#------------------------------------------------------------------------------------------------------------------------------------

# adjust prognostic covariate- qids_sum
# Group 1 vs 2
# adjusted logistic regression 
fdat12<-filter(fdatm,arm!=3)%>%select(c(rem,arm,qids_sum))
unadjusted12 <- glm(rem ~ arm+qids_sum, data = fdat12, family = "binomial")
summary(unadjusted12)
ci12<-confint(unadjusted12)
round(ci12,3)
# Bootstrap
set.seed(123)
#par_bootstrap = array(0, c(10000, length(coefficients(adjusted12m))))
coef<-rep(NA,10000)
#colnames(par_bootstrap) = names(coefficients(adjusted12m))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat12), size = nrow(fdat12), replace = TRUE)
  tempdata = fdat12[idx_rd, ]
  fit_temp = glm(rem ~., data = tempdata, family = "binomial")
  coef[k] = coefficients(fit_temp)["arm"]
}
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))

# Group 1 vs 2
# Standardized estimator
# only adjust qids_sum
fdat12m<-filter(fdatm,arm!=3)%>%select(c(rem,arm,qids_sum))

data.used = fdat12m
# Fitting the logistic regression model
log.reg = glm(rem ~., data = data.used, family = "binomial")
# Creating dataset for calculating the predictions corresponding to
# Arm = 1 and Arm = 0
data.a.1 = data.used
data.a.1$arm = 1
data.a.0 = data.used
data.a.0$arm = 0
# Calculating the predictions
pred.1 = predict.glm(log.reg, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
pred.0 = predict.glm(log.reg, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
res.gcomp = mean(pred.1) - mean(pred.0)
res.gcomp

# Function to calculate the variance estimator
set.seed(123)
# Creating the data frame
data.used = fdat12m
boot.gcomp = rep(NA, 10000)
for(i in 1:10000){
  # Finding the bootstrap sample
  bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  # Fitting the bootstrapped logistic regression model
  log.reg.bs = glm(rem ~., data = data.used[bs, ], family = "binomial")
  data.a.1 = data.used[bs, ]
  data.a.1$arm = 1
  data.a.0 = data.used[bs, ]
  data.a.0$arm = 0
  # Calculating the predictions
  p.1.bs = predict.glm(log.reg.bs, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
  p.0.bs = predict.glm(log.reg.bs, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
  # Calculating the bootstrap estimator
  boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
}
# Returning the estimator and variance estimator
bootvar=var(boot.gcomp)
bootse=sd(boot.gcomp)
ci=quantile(boot.gcomp, probs = c(0.025, 0.975))


# Group 1 vs 3
# adjusted logistic regression (only adjusted qids_sum)
fdat13<-filter(fdatm,arm!=2)
fdat13$arm<-ifelse(fdat13$arm==1,1,2)
unadjusted13 <- glm(rem ~ arm+qids_sum, data = fdat13, family = "binomial")
summary(unadjusted13)
ci13<-confint(unadjusted13)
round(ci13,3)
# bootstrap
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(unadjusted13))))
colnames(par_bootstrap) = names(coefficients(unadjusted13))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat13), size = nrow(fdat13), replace = TRUE)
  tempdata = fdat13[idx_rd, ]
  fit_temp = glm(rem ~ arm+qids_sum, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se13 = sd(par_bootstrap[, 2])
bt_ci13 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))
round(bt_se13,3)
round(bt_ci13,3)

# Group 1 vs 3
# Standardized estimator
# only adjust qids_sum
fdat13<-filter(fdatm,arm!=2)%>%select(arm,rem,qids_sum)
fdat13$arm<-ifelse(fdat13$arm==1,0,1)

data.used = fdat13m
# Fitting the logistic regression model
log.reg = glm(rem ~., data = data.used, family = "binomial")
# Creating dataset for calculating the predictions corresponding to
# Arm = 1 and Arm = 0
data.a.1 = data.used
data.a.1$arm = 1
data.a.0 = data.used
data.a.0$arm = 0
# Calculating the predictions
pred.1 = predict.glm(log.reg, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
pred.0 = predict.glm(log.reg, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
res.gcomp = mean(pred.1) - mean(pred.0)
res.gcomp

# Function to calculate the variance estimator
set.seed(123)
# Creating the data frame
data.used = fdat13m
boot.gcomp = rep(NA, 10000)
for(i in 1:10000){
  # Finding the bootstrap sample
  bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  # Fitting the bootstrapped logistic regression model
  log.reg.bs = glm(rem ~., data = data.used[bs, ], family = "binomial")
  data.a.1 = data.used[bs, ]
  data.a.1$arm = 1
  data.a.0 = data.used[bs, ]
  data.a.0$arm = 0
  # Calculating the predictions
  p.1.bs = predict.glm(log.reg.bs, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
  p.0.bs = predict.glm(log.reg.bs, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
  # Calculating the bootstrap estimator
  boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
}
# Returning the estimator and variance estimator
bootvar=var(boot.gcomp)
bootse=sd(boot.gcomp)
ci=quantile(boot.gcomp, probs = c(0.025, 0.975))

# Group 2 vs 3
# adjusted logistic regression (only adjusted qids_sum)
unadjusted23 <- glm(rem ~ arm+qids_sum, data = fdat23, family = "binomial")
summary(unadjusted23)
ci23<-confint(unadjusted23)
round(ci23,3)
# bootstrap
set.seed(123)
par_bootstrap = array(0, c(10000, length(coefficients(unadjusted23))))
colnames(par_bootstrap) = names(coefficients(unadjusted23))
for (k in 1:10000) {
  idx_rd = sample(1:nrow(fdat23), size = nrow(fdat23), replace = TRUE)
  tempdata = fdat23[idx_rd, ]
  fit_temp = glm(rem ~ arm+qids_sum, data = tempdata, family = "binomial")
  par_bootstrap[k, ] = coefficients(fit_temp)
}
bt_se23 = sd(par_bootstrap[, 2])
bt_ci23 = quantile(par_bootstrap[, 2], probs = c(0.025, 0.975))
round(bt_se23,3)
round(bt_ci23,3)

# Group 2 vs 3
# Standardized estimator
# only adjust qids_sum

fdat23m<-filter(fdatm,arm!=1)%>%select(arm,rem,qids_sum)
fdat23m$arm<-ifelse(fdat23m$arm==2,0,1)

data.used = fdat23m
# Fitting the logistic regression model
log.reg = glm(rem ~., data = data.used, family = "binomial")
# Creating dataset for calculating the predictions corresponding to
# Arm = 1 and Arm = 0
data.a.1 = data.used
data.a.1$arm = 1
data.a.0 = data.used
data.a.0$arm = 0
# Calculating the predictions
pred.1 = predict.glm(log.reg, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
pred.0 = predict.glm(log.reg, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
res.gcomp = mean(pred.1) - mean(pred.0)
res.gcomp

# Function to calculate the variance estimator
set.seed(123)
# Creating the data frame
data.used = fdat23m
boot.gcomp = rep(NA, 10000)
for(i in 1:10000){
  # Finding the bootstrap sample
  bs = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  # Fitting the bootstrapped logistic regression model
  log.reg.bs = glm(rem ~., data = data.used[bs, ], family = "binomial")
  data.a.1 = data.used[bs, ]
  data.a.1$arm = 1
  data.a.0 = data.used[bs, ]
  data.a.0$arm = 0
  # Calculating the predictions
  p.1.bs = predict.glm(log.reg.bs, newdata =data.a.1[, colnames(data.used) != "rem"], type = "response")
  p.0.bs = predict.glm(log.reg.bs, newdata =data.a.0[, colnames(data.used) != "rem"], type = "response")
  # Calculating the bootstrap estimator
  boot.gcomp[i] = mean(p.1.bs) - mean(p.0.bs)
}
# Returning the estimator and variance estimator
bootvar=var(boot.gcomp)
bootse=sd(boot.gcomp)
ci=quantile(boot.gcomp, probs = c(0.025, 0.975))

#------------------------------------------------------------------------------------------------------------------------------------

# Simulate W, A, Y independently with replacement
# Group 1 vs 2; 
set.seed(123)
fdat12m<-filter(fdatm,arm!=3)
data.used = fdat12m
coef=res.gcomp<-rep(NA,10000)
par_bootstrap = data.frame(A1=rep(NA,10000),A2=rep(NA,10000),Diff=rep(NA,10000))
for (k in 1:10000) {
  bsw = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  ndat=data.used[bsw,]
  ndat$arm<-rbinom(nrow(ndat),1,0.5)
  ndat$rem<-rbinom(nrow(ndat),1,0.38)
  fit_temp = glm(rem ~., data = ndat, family = "binomial")
  #logistic coefficient
  coef[k] = coefficients(fit_temp)["arm"] 
  # unadjusted estimator
  par_bootstrap[k,1] = mean(ndat$rem[which(ndat$arm==0)])
  par_bootstrap[k,2] = mean(ndat$rem[which(ndat$arm==1)])
  # Standard estimator
  data.a.1 = ndat
  data.a.1$arm = 1
  data.a.0 = ndat
  data.a.0$arm = 0
  pred.1 = predict.glm(fit_temp, newdata =data.a.1[, colnames(ndat) != "rem"], type = "response")
  pred.0 = predict.glm(fit_temp, newdata =data.a.0[, colnames(ndat) != "rem"], type = "response")
  res.gcomp[k] = mean(pred.1) - mean(pred.0)
}
#logistic coefficient
mean(coef)
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))
#unadjusted risk difference
par_bootstrap[,3]=par_bootstrap[,2]-par_bootstrap[,1] 
mean(par_bootstrap[,3])
var(par_bootstrap[,3])
sd(par_bootstrap[,3])
quantile(par_bootstrap[,3],probs = c(0.025,0.975))
# Standard estimator
mean(res.gcomp)
var(res.gcomp)
sd(res.gcomp)
quantile(res.gcomp, probs = c(0.025, 0.975))

# Simulate W, A, Y independently with replacement
# Group 1 vs 3; 
set.seed(123)
fdat13m<-filter(fdatm,arm!=2)
data.used = fdat13m
coef=res.gcomp<-rep(NA,10000)
par_bootstrap = data.frame(A1=rep(NA,10000),A2=rep(NA,10000),Diff=rep(NA,10000))
for (k in 1:10000) {
  bsw = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  ndat=data.used[bsw,]
  ndat$arm<-rbinom(nrow(ndat),1,0.5)
  ndat$rem<-rbinom(nrow(ndat),1,0.38)
  fit_temp = glm(rem ~., data = ndat, family = "binomial")
  #logistic coefficient
  coef[k] = coefficients(fit_temp)["arm"] 
  # unadjusted estimator
  par_bootstrap[k,1] = mean(ndat$rem[which(ndat$arm==0)])
  par_bootstrap[k,2] = mean(ndat$rem[which(ndat$arm==1)])
  # Standard estimator
  data.a.1 = ndat
  data.a.1$arm = 1
  data.a.0 = ndat
  data.a.0$arm = 0
  pred.1 = predict.glm(fit_temp, newdata =data.a.1[, colnames(ndat) != "rem"], type = "response")
  pred.0 = predict.glm(fit_temp, newdata =data.a.0[, colnames(ndat) != "rem"], type = "response")
  res.gcomp[k] = mean(pred.1) - mean(pred.0)
}
#logistic coefficient
mean(coef)
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))
#unadjusted risk difference
par_bootstrap[,3]=par_bootstrap[,2]-par_bootstrap[,1] 
mean(par_bootstrap[,3])
var(par_bootstrap[,3])
sd(par_bootstrap[,3])
quantile(par_bootstrap[,3],probs = c(0.025,0.975))
# Standard estimator
mean(res.gcomp)
var(res.gcomp)
sd(res.gcomp)
quantile(res.gcomp, probs = c(0.025, 0.975))


# Simulate W, A, Y independently with replacement
# Group 2 vs 3; 
set.seed(123)
fdat23m<-filter(fdatm,arm!=1)
data.used = fdat23m
coef=res.gcomp<-rep(NA,10000)
par_bootstrap = data.frame(A1=rep(NA,10000),A2=rep(NA,10000),Diff=rep(NA,10000))
for (k in 1:10000) {
  bsw = sample(1:nrow(data.used), size = nrow(data.used), replace = TRUE)
  ndat=data.used[bsw,]
  ndat$arm<-rbinom(nrow(ndat),1,0.5)
  ndat$rem<-rbinom(nrow(ndat),1,0.38)
  fit_temp = glm(rem ~., data = ndat, family = "binomial")
  #logistic coefficient
  coef[k] = coefficients(fit_temp)["arm"] 
  # unadjusted estimator
  par_bootstrap[k,1] = mean(ndat$rem[which(ndat$arm==0)])
  par_bootstrap[k,2] = mean(ndat$rem[which(ndat$arm==1)])
  # Standard estimator
  data.a.1 = ndat
  data.a.1$arm = 1
  data.a.0 = ndat
  data.a.0$arm = 0
  pred.1 = predict.glm(fit_temp, newdata =data.a.1[, colnames(ndat) != "rem"], type = "response")
  pred.0 = predict.glm(fit_temp, newdata =data.a.0[, colnames(ndat) != "rem"], type = "response")
  res.gcomp[k] = mean(pred.1) - mean(pred.0)
}
#logistic coefficient
mean(coef)
sd(coef)
var(coef)
quantile(coef, probs = c(0.025, 0.975))
#unadjusted risk difference
par_bootstrap[,3]=par_bootstrap[,2]-par_bootstrap[,1] 
mean(par_bootstrap[,3])
var(par_bootstrap[,3])
sd(par_bootstrap[,3])
quantile(par_bootstrap[,3],probs = c(0.025,0.975))
# Standard estimator
mean(res.gcomp)
var(res.gcomp)
sd(res.gcomp)
quantile(res.gcomp, probs = c(0.025, 0.975))
