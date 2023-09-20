#===============================================================================
#Propensity Score Matching using optimal (face to face and push to web)
#Face to face and push to web (Community Life Survey)
#Eliud Kibuchi 
#===============================================================================
#Packages 
rm(list=ls())
list.files()
library(plyr)
library(fmsb)
library(lattice)
require(Rcpp)
library(MatchIt)
require(cobalt)
library(optmatch)
library(Matching)
library(rbounds)
require(Zelig)
library(knitr)
library(nnet)
library(semPlot)
library(corrplot)
library(ggplot2)
require(psychometric) #calculation of Cronbach's alpha 
library(semTools)
library(survey)
library(tableone)
library(fmsb)
library(crosstable)
library(sjPlot)
library(plyr)
library(broom)
#===============================================================================
set.seed(1)


#Set working directory 
setwd("C:/____Kibuchi_local/MyOneDrive/OneDrive - University of Glasgow/kibuchi/Survey papers/CLS")
list.files()

#Import data 
cls_data<-read.csv("data/cls_fulldata__140118.csv")
names(cls_data);length(cls_data[,1])
#===============================================================================
#data management 

cls_revised<-read.csv("data/Revised GOR.csv");names(cls_revised)
colnames(cls_revised)<-c("Samplegroup","Cserial","GOR_rev","Numcases");length(cls_revised[,1])

summary(cls_data$Cserial);summary(cls_data$Samplegroup)
summary(cls_revised$Cserial);summary(cls_revised$Samplegroup);

cls_data<-merge(cls_data,cls_revised, by=c("Cserial","Samplegroup"))
summary(cls_data$GOR_rev)

#======================================================
#select the following variables in data 
#======================================================
#into factors
cls_data$mdl_numberofadultsinhousehold<-as.factor(cls_data$mdl_numberofadultsinhousehold)
cls_data$mdl_agebanded<-as.factor(cls_data$mdl_agebanded)
cls_data$mdl_single<-as.factor(cls_data$mdl_single)
cls_data$mdl_male<-as.factor(cls_data$mdl_male)
cls_data$mdl_numberofchilden<-as.factor(cls_data$mdl_numberofchilden)
cls_data$mdl_paidwork<-as.factor(cls_data$mdl_paidwork)
cls_data$mdl_usetheinternet<-as.factor(cls_data$mdl_usetheinternet)
cls_data$mdl_meetfamilyoncefortnightorlessoften<-as.factor(cls_data$mdl_meetfamilyoncefortnightorlessoften)
cls_data$mdl_stronglybelongtoneighbourhood<-as.factor(cls_data$mdl_stronglybelongtoneighbourhood)
cls_data$mdl_sometimesoralwaysfeellonely<-as.factor(cls_data$mdl_sometimesoralwaysfeellonely)
cls_data$mdl_votedlocalelection<-as.factor(cls_data$mdl_votedlocalelection)
cls_data$mdl_volunteer<-as.factor(cls_data$mdl_volunteer)
#cls_data$mdl_wellbeing1<-as.factor(cls_data$mdl_wellbeing1)
cls_data$mdl_wellbeing2<-as.factor(cls_data$mdl_wellbeing2)
cls_data$mdl_wellbeing3<-as.factor(cls_data$mdl_wellbeing3)
cls_data$mdl_wellbeing4<-as.factor(cls_data$mdl_wellbeing4)
cls_data$mdl_ownmortgagesharedownership<-as.factor(cls_data$mdl_ownmortgagesharedownership)
cls_data$mdl_sometimesoralwaysfeellonely<-as.factor(cls_data$mdl_sometimesoralwaysfeellonely)
cls_data$mdl_white<-as.factor(cls_data$mdl_white)
cls_data$mdl_englishmainlanguage<-as.factor(cls_data$mdl_englishmainlanguage)
cls_data$mdl_christian<-as.factor(cls_data$mdl_christian)
cls_data$mdl_badorverybadhealth<-as.factor(cls_data$mdl_badorverybadhealth)
cls_data$mdl_civicparticipation<-as.factor(cls_data$mdl_civicparticipation)
cls_data$mdl_noqualifications<-as.factor(cls_data$mdl_noqualifications)
cls_data$mdl_careresponsibility<-as.factor(cls_data$mdl_careresponsibility)
cls_data$mdl_howoftenusetheinternet<-as.factor(cls_data$mdl_howoftenusetheinternet)
summary(cls_data$mdl_paidwork)
summary(as.factor(cls_data$mdl_male))
summary(as.factor(cls_data$mdl_numberofchilden))
summary(as.factor(cls_data$CivConsa))
names(cls_data)

#===================================================]
#subset data by variable names 
covariates_ALL<-c("Samplegroup","Mode","mdl_agebanded","mdl_male","mdl_single","mdl_numberofchilden","mdl_paidwork","Zincome",
                  "mdl_white","mdl_englishmainlanguage","mdl_numberofadultsinhousehold","mdl_ownmortgagesharedownership","ZTenure","GOR_rev","WellB1","WellB2","WellB3","WellB4","PAffLoc","PInfl",
                  "Conoft","FGroup1A","FGroup1B","FGroup1C","FGroup1D","FGroup1E","FGroup1F","FGroup1G","FGroup1H","FGroup1I",
                  "FGroup1J","FGroup1K","FGroup1L","FGroup1M","FGroup1N","FGroup1O","FGroup1P","FGroup1Q","FUnPd1A","FUnPd1B",
                  "FUnPd1C","FUnPd1D","FUnPd1E","FUnPd1F","FUnPd1G","FUnPd1H","FUnPd1I","FUnPd1J","FUnPd1K","FUnPd1L","FUnPd1M","FUnPd1N","FIndGp1a",
                  "FIndGp1b","FIndGp1c","FIndGp1d","FIndGp1e","FIndGp1f","FIndGp1g","FIndGp1h","FIndGp1i","FIndGp1j",
                  "FIndGp1k","FIndGp1l","FIndGp1m","FIndGp1n","FIndGp1o","FIndGp1p","FIndGp1q","FIndGp1r","FIndGp1s","FIndGp1t",
                  "FIndGp1u","FIndGp1v","FIndGp1w","FIndGp1x","MxFVol2a","MxFVol2b","MxFVol2c","MxFVol2d","MxFVol2e","VolBen1a",
                  "VolBen1b","VolBen1c","VolBen1d","VolBen1e","VolBen1f","VolBen1g","VolBen1h","VolBen1i","VolBen1j",
                  "VolBen1k","VolBen1l","VolBen1m","VolBen1n","VolBen1o","VYStopa","VYStopb","VYStopc","VYStopd","VYStope",
                  "VYStopf","VYStopg","VYStoph","VYStopi","VYStopj","VYStopk","VYStopl","VYStopm","VYStopn","VYStopo","VYStopp",
                  "VBarr1a","VBarr1b","VBarr1c","VBarr1d","VBarr1e","VBarr1f","VBarr1g","VBarr1h","VBarr1h","VBarr1i","VBarr1j",
                  "VBarr1k","VBarr1l","VBarr1m","VBarr1n","VBarr1o","VBarr1p","VBarr1q","VBarr1r","IHlp1","IHlp2","IHlp3",
                  "IHlp4","IHlp5","IHlp6","IHlp7","IHlp8","IHlp9","IHlp10","IHlp11","IHlp12","IHlp13","GGroup2a","GGroup2b",
                  "GGroup2c","GGroup2d","GGroup2e","GGroup2f","GGroup2g","GGroup2h","GGroup2i","GGroup2j","GGroup2k","GGroup2l",
                  "GGroup2m","GGroup2n","GGroup2o","GGroup2p","GGroup2u","GGroup2t","GGroup2q","GGroup2r","GGroup2s","Caus4wa", 
                  "Caus4wb","Caus4wc","Caus4wd","Caus4wf","Caus4wg","Caus4wh","Caus4wh","Caus4wi","Caus4wj","Caus4wk","Caus4wl",
                  "Caus4wm","Caus4wn","Caus4wq","Caus4wp","Caus4wo","Teuse3_1","Teuse3_2","Teuse3_3","Teuse3_4","Teuse3_5",
                  "Teuse3_6","Teuse3_7","Teuse3_8","Teuse3_9","Teuse3_10","Teuse3_11","Teuse3_12","Teuse3_13","LocInvNa",
                  "LocInvNb","LocInvNc","LocInvNd","LocInvNe","LocInvNf","LocInvNg","LocPeopNewa","LocPeopNewb","LocPeopNewc",
                  "LocPeopNewd","LocPeopNewe","LocPeopNewf","LocPeopNewg","LocActa","LocActb","LocActc","LocActd","LocActe",
                  "LocActf","LocActg","LocActk","LocActk","LocActm","LocHowa","LocHowb","LocHowc","LocHowd","LocHowe","LocHowf",
                  "LocHowg","LocHowh", "LocBarr1a","LocBarr1b","LocBarr1c","LocBarr1d","LocBarr1e","LocBarr1f","LocBarr1g","LocBarr1h",
                  "LocBarr1l","LocBarr1m","LocBarr1n","LocBarr1o","LocBarr1p","LocBarr1q","SBeNeigh","SBeGB","SFavN","NComfort1",
                  "NComfort2","NComfort3","FrndRel1","FrndRel2","FrndRel3","FrndRel4","FrndSat1","FrndSat2","Counton1","Comoft2", "CivParta","CivPartb",
                  "CivPartc","CivPartd","CivConsa","CivConsb","CivConsc","CivConsd","CivAct11", "CivAct12","CivAct13","CivAct14",
                  "CivAct15","CivAct21","CivAct22","CivAct23","CivAct24","CivAct25","CivAct26","CivAct27","CivAct28","SchatN","SPull",
                  "STrust","Slocsat","STogeth","BetWors","SatAsset","FifGp2","Givech3","LocAtt","LocInvNg",
                  "LonOft","Relig2","Nslivarr","SLive7","DeviceDetails_KantarDeviceType","DiDWeight","Zquals1","FinalStratum","FinalStratum_collapsed")


cls_data_sub<-cls_data[covariates_ALL]

str(cls_data_sub$mdl_male); summary(cls_data_sub$mdl_male)


str(cls_data_sub$mdl_white); summary(cls_data_sub$mdl_white)
levels(cls_data_sub$mdl_white)

str(cls_data_sub$mdl_single); levels(cls_data_sub$mdl_single)
cls_data_sub$mdl_single<-revalue(cls_data_sub$mdl_single, c("0"="married", "1"="single"))

levels(cls_data_sub$mdl_single); levels(cls_data_sub$mdl_single)
levels(cls_data_sub$mdl_paidwork)


cls_data_sub$Zquals1 <-  as.factor(cls_data_sub$Zquals1)
str(cls_data_sub$Zquals1); summary(cls_data_sub$Zquals1)
levels(cls_data_sub$Zquals1)<-c( "Other qualification" ,"Degree or above","Other qualification",
                                 "Other qualification","Other qualification","Other qualification",
                                 "no qualifications","no qualifications","no qualifications")
str(cls_data_sub$Zquals1); summary(cls_data_sub$Zquals1)

cls_data_sub$SLive7 <- as.factor(cls_data_sub$SLive7)
levels(cls_data_sub$SLive7)

cls_data_sub$Nslivarr <-  as.factor(cls_data_sub$Nslivarr)
levels(cls_data_sub$SLive7)


#========================================================
# face to face and push to web 

summary(cls_data_sub$Mode)
table(cls_data_sub$Mode,cls_data_sub$Samplegroup)
cls_data_mode<-subset(cls_data_sub,Mode == "Web"| Mode == "Face to face")
table(cls_data_mode$Mode,cls_data_mode$Samplegroup)
summary(cls_data_mode$Mode)

#===============================================================================
# subset face to face and push to web
#===============================================================================

cls_data1<-subset(cls_data_mode,Samplegroup=="F2F 2014-15 Q2"|Samplegroup=="Web/postal 2014-15 Q2 ABOS")
cls_data1$Samplegroup<-factor(cls_data1$Samplegroup);summary(cls_data1$Samplegroup)

#make mode effect outcome
cls_data1$cls1_outcome<-cls_data1$Samplegroup
levels(cls_data1$cls1_outcome) = c("F2F 2014-15 Q2" = 1, "Web/postal 2014-15 Q2 ABOS"= 0)
summary(cls_data1$cls1_outcome);summary(cls_data1)
summary(cls_data1$Teuse3_12)
cls_data1$cls1_outcome_p<-cls_data1$cls1_outcome #Outcome
levels(cls_data1$cls1_outcome_p)<-c("push_to_web","face_to_face")
summary(cls_data1$DiDWeight)

cls_data1$Data_collection_mode <- cls_data1$cls1_outcome 

cls_data1$mdl_agebanded <- as.factor(cls_data1$mdl_agebanded)

cls_data2 <- subset(cls_data1, mdl_agebanded!="-1") #remove missing values for age 

summary(cls_data2$mdl_agebanded)
cls_data2$mdl_agebanded <- factor(cls_data2$mdl_agebanded); levels(cls_data2$mdl_agebanded)
cls_data2$mdl_agebanded <- factor(cls_data2$mdl_agebanded)

cls_data2$ZTenure <- as.factor(cls_data2$ZTenure); summary(cls_data2$ZTenure); levels(cls_data2$ZTenure)
levels(cls_data2$ZTenure)<- c("not_known" ,"Mortgaged","Other (mainly private rent)","Outright ownership","Social rent")
summary(cls_data2$ZTenure); levels(cls_data2$ZTenure)

table(cls_data2$mdl_agebanded,cls_data2$cls1_outcome)
levels(cls_data2$mdl_agebanded)<- c("16to34","16to34","35to49","50to64","65to74","75plus") 


#GOR_rev
cls_data2$GOR_rev <- relevel(cls_data2$GOR_rev, ref="London")


#socio-demographic characteristics summary
cls_data2<- within(cls_data2, mdl_agebanded <- relevel(mdl_agebanded, ref = "16to34"))
cls_data2<- within(cls_data2, GOR_rev <- relevel(GOR_rev, ref = "London"))
contrasts(cls_data2$Zincome) <- contr.treatment(4)
cls_data2<- within(cls_data2, Zquals1<- relevel(Zquals1, ref = "no qualifications"))

cls_data2$Zincome <- as.factor(cls_data2$Zincome); levels(cls_data2$Zincome)
cls_data2 <- within(cls_data2, Zincome <- relevel(Zincome, ref = "Under £15k or nothing"))

cls_data2$mdl_male <-  as.factor(cls_data2$mdl_male); levels(cls_data2$mdl_male); summary(cls_data2$mdl_male)
levels (cls_data2$mdl_male) <- c("female", "male");levels(cls_data2$mdl_male); summary(cls_data2$mdl_male)

cls_data2$mdl_englishmainlanguage <- as.factor(cls_data2$mdl_englishmainlanguage)
summary(cls_data2$mdl_englishmainlanguage); levels(cls_data2$mdl_englishmainlanguage)
levels (cls_data2$mdl_englishmainlanguage) <- c("Others", "English")
summary(cls_data2$mdl_englishmainlanguage); levels(cls_data2$mdl_englishmainlanguage)
cls_data2 <- within(cls_data2, mdl_englishmainlanguage <- relevel(mdl_englishmainlanguage, ref = "English"))

cls_data2$mdl_paidwork <-  as.factor(cls_data2$mdl_paidwork); levels(cls_data2$mdl_paidwork); summary(cls_data2$mdl_paidwork)
levels (cls_data2$mdl_paidwork) <- c("No", "Yes")
levels(cls_data2$mdl_paidwork); summary(cls_data2$mdl_paidwork)

cls_data2$ZTenure <- as.factor(cls_data2$ZTenure)
levels(cls_data2$ZTenure); summary(cls_data2$ZTenure)
cls_data2 <- within(cls_data2, ZTenure <- relevel(ZTenure, ref = "Other (mainly private rent)"))

cls_data2$GOR_rev <- as.factor(cls_data2$GOR_rev)
levels(cls_data2$GOR_rev); summary(cls_data2$GOR_rev)

cls_data2 <- within(cls_data2, GOR_rev <- relevel(GOR_rev, ref = "London"))

crosstable(cls_data2, c(mdl_male), by = Data_collection_mode, total="both")

crosstable(cls_data2, c(mdl_single), by = Data_collection_mode, total="both")

crosstable(cls_data2, c(mdl_paidwork), by = Data_collection_mode, total="both")

crosstable(cls_data2, c(mdl_white), by = Data_collection_mode, total="both")

crosstable(cls_data2, c(mdl_englishmainlanguage), by = Data_collection_mode, total="both")



cls_data2  <- cls_data2 %>% 
  rename(
    Gender = mdl_male,
    Age = mdl_agebanded,
    Marital_status = mdl_single,
    Number_of_children = mdl_numberofchilden,
    Paid_work = mdl_paidwork,
    Income = Zincome,
    Ethnic_group = mdl_white,
    Number_of_adults_household = mdl_numberofadultsinhousehold,
    Education = Zquals1,
    Tenure = ZTenure,
    Region = GOR_rev,
    Main_language = mdl_englishmainlanguage,
  )

summary(cls_data2$Gender)


covariates_names <-c("Gender","Age","Marital_status","Number_of_children","Paid_work",
                     "Income","Ethnic_group","Number_of_adults_household","Education","Tenure","Region","Main_language")

variables_choice <-c("Gender","Age","Marital_status","Number_of_children","Paid_work",
                     "Income","Ethnic_group","Number_of_adults_household","Education","Tenure","Region","Main_language","Data_collection_mode")

#======================
#more data mgt 
cls_data2$Gender <- as.factor(cls_data2$Gender); summary(cls_data2$Gender)
cls_data2$Age <- as.factor(cls_data2$Age); summary(cls_data2$Age)
cls_data2$Marital_status <- as.factor(cls_data2$Marital_status); summary(cls_data2$Marital_status)
cls_data2$Number_of_children <- as.factor(cls_data2$Number_of_children); summary(cls_data2$Number_of_children)
cls_data2$Paid_work <- as.factor(cls_data2$Paid_work); summary(cls_data2$Paid_work)
cls_data2$Income <- as.factor(cls_data2$Income); summary(cls_data2$Income)
cls_data2$Ethnic_group <- as.factor(cls_data2$Ethnic_group); summary(cls_data2$Ethnic_group)
cls_data2$Number_of_adults_household <- as.factor(cls_data2$Number_of_adults_household); summary(cls_data2$Number_of_adults_household)
cls_data2$Education <- as.factor(cls_data2$Education); summary(cls_data2$Education)
cls_data2$Tenure <- as.factor(cls_data2$Tenure); summary(cls_data2$Tenure); levels(cls_data2$Tenure)
cls_data2 <- cls_data2[cls_data2$Tenure %in% c("Other (mainly private rent)","Mortgaged","Outright ownership","Social rent"), ] 
summary(cls_data2$Tenure)
cls_data2$Tenure <- factor(cls_data2$Tenure); levels(cls_data2$Tenure);summary(cls_data2$Tenure)
cls_data2$Region <- as.factor(cls_data2$Region); summary(cls_data2$Region)
cls_data2$Main_language <- as.factor(cls_data2$Main_language); summary(cls_data2$Main_language)


cls_data_unmatched <- cls_data2[variables_choice]

#Unmatched data frame 
cls_data_un <-data.frame(cls_data_unmatched) 

cls_data_unmatched$Gender <- as.factor(cls_data_unmatched$Gender); str(cls_data_unmatched$Gender)
cls_data_unmatched$Marital_status <- as.factor(cls_data_unmatched$Marital_status); str(cls_data_unmatched$Marital_status)

summary(cls_data_unmatched$Gender) 

x_vars <- c("Gender","Age","Marital_status","Number_of_children","Paid_work",
            "Income","Ethnic_group","Number_of_adults_household","Education","Tenure","Region","Main_language")


f2f_push2web_tab <- CreateTableOne(vars = x_vars,strata = "Data_collection_mode", data = cls_data_unmatched)

print(f2f_push2web_tab,smd=TRUE)

f2f_push2web_tab_mat <- print(f2f_push2web_tab, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE,smd = TRUE)


#more data mgt 
crosstable(cls_data_unmatched, c(Gender), by = Data_collection_mode, total="both")


write.csv(f2f_push2web_tab_mat, file = "Results/f2f_push2web/f2f_push2web_SMD unmatched.csv")

#===============================================================================

#Stratum cases 
cls_data3<-cls_data2; head(cls_data3)

ddply(cls_data3,"FinalStratum_collapsed",numcolwise(sum),by = Data_collection_mode)
table(cls_data3$FinalStratum_collapsed)

crosstable(cls_data3,Data_collection_mode, by = Region)
crosstable(cls_data3,Data_collection_mode, by = Tenure)
summary(as.factor(cls_data3$Tenure))

#===============================================================================
#Propensity score model

#Specify survey weights 
design_weights <- svydesign(id = ~1,strata =~ FinalStratum_collapsed ,weights = ~DiDWeight,data = cls_data2)

psm_f2f_push2web <- svyglm(Data_collection_mode ~ Gender + Age + Marital_status + Number_of_children + 
                                  Paid_work + Income + Ethnic_group + Number_of_adults_household + 
                                  Education + Tenure + Region + Main_language, design = design_weights, family = quasibinomial(link = "logit"),data=cls_data2) 

summary(psm_f2f_push2web)

psm_f2f_push2web_est <-  summary(psm_f2f_push2web)$coefficients

write.csv(psm_f2f_push2web_est, "Results/f2f_push2web/psm_f2f_push2web_est.csv")

NagelkerkeR2(psm_f2f_push2web)

#AUC (area under the curve): 
predicted <- predict(psm_f2f_push2web, cls_data2, type = "response")
auc_f2f_P2W <- auc(cls_data2$Data_collection_mode, predicted); auc_f2f_P2W

#===============================================================================
#Perform optimal matching with exact matching without replacement 


match_f2f_push2web <- matchit(Data_collection_mode ~ Gender + Age + Marital_status + Number_of_children + 
                                Paid_work + Income + Ethnic_group + Number_of_adults_household + 
                                Education + Tenure + Region + Main_language, 
                               method = "optimal",ratio=1, distance = "glm",  
                              s.weights = ~ DiDWeight,
                              exact = ~Number_of_adults_household + Main_language + Region + 
                                Income + Age + Number_of_children + Ethnic_group + Tenure + 
                                Education +  Paid_work , data = cls_data2)
summary(match_f2f_push2web)
plot(summary(match_f2f_push2web))

#Check covariate balance
(balance_match_f2f_push2web <- summary(match_f2f_push2web, standardize=T))


cls_f2f_push2web <- match.data(match_f2f_push2web, group="all")

#Summary of matched samples 
cls_f2f_push2web_matched <- cls_f2f_push2web[variables_choice]
cls_f2f_push2web_matched <- data.frame(cls_f2f_push2web_matched)
names(cls_f2f_push2web_matched)

summary(cls_f2f_push2web_matched$Data_collection_mode)

names(cls_f2f_push2web_matched)

table_matched_f2f_push2web <- CreateTableOne(vars= variables_choice,strata ="Data_collection_mode",data = cls_f2f_push2web_matched)

print(table_matched_f2f_push2web,smd=TRUE)

table_matched_f2f_push2web_mat <- print(table_matched_f2f_push2web, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE,smd=TRUE)

write.csv(table_matched_f2f_push2web_mat, file="Results/f2f_push2web/Optimal f2f_push2web_matched(smd)_040923.csv")

#===============================================================================
#Un Matched data
#====================

table1<-CreateTableOne(vars = variables_choice,strata = "Data_collection_mode",data = cls_data_unmatched)

print(table1,smd=TRUE)
table13Mat <- print(table1, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE,smd=TRUE)

write.csv(table13Mat, file="Results/f2f_push2web/f2f_push2web_unmatched (smd)_040923.csv")

#================================================================================
#================================================================================
#Proportions of attitudinal atttributes for matched data 

#FrndRel1 How often meet up in person with family members or friends 
chisq.test(table(factor(cls_data1$FrndRel1), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FrndRel1), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FrndRel1_um<-table(cls_data1$FrndRel1, cls_data1$cls1_outcome);cross_FrndRel1_um
prop.table(cross_FrndRel1_um,margin = 2)*100
summary(cls_data1$FrndRel1);

cross_FrndRel1<-table(cls_f2f_push2web$FrndRel1, cls_f2f_push2web $cls1_outcome);cross_FrndRel1
summary(cls_f2f_push2web $FrndRel1)
#===============================================================================

#FrndRel2 How often speak on the phone or video or audio call via the internet with family members or friends 
chisq.test(table(factor(cls_data1$FrndRel2), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FrndRel2), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FrndRel2_um<-table(cls_data1$FrndRel2, cls_data1$cls1_outcome);cross_FrndRel2_um
prop.table(cross_FrndRel2_um,margin = 2)*100
summary(cls_data1$FrndRel2)

cross_FrndRel2<-table(cls_f2f_push2web$FrndRel2, cls_f2f_push2web $cls1_outcome);cross_FrndRel2
summary(cls_f2f_push2web $FrndRel2)
#===============================================================================

#FrndRel3 How often email or write to family members or friends 
chisq.test(table(factor(cls_data1$FrndRel3), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FrndRel3), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FrndRel3_um<-table(cls_data1$FrndRel3, cls_data1$cls1_outcome);cross_FrndRel3_um
prop.table(cross_FrndRel3_um,margin = 2)*100
summary(cls_data1$FrndRel3)

cross_FrndRel3<-table(cls_f2f_push2web $FrndRel3, cls_f2f_push2web $cls1_outcome);cross_FrndRel3
summary(cls_f2f_push2web $FrndRel3)
#===============================================================================

#FrndRel4 How often exchange text messages or instant messages with family members or friends
chisq.test(table(factor(cls_data1$FrndRel4), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FrndRel4), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FrndRel4_um<-table(cls_data1$FrndRel4,cls_data1$cls1_outcome);cross_FrndRel4_um
prop.table(cross_FrndRel4_um,margin = 2)*100
summary(cls_data1$FrndRel4)

cross_FrndRel4<-table(cls_f2f_push2web$FrndRel4, cls_f2f_push2web $cls1_outcome);cross_FrndRel4
summary(cls_f2f_push2web $FrndRel4)
#===============================================================================

#FrndSat1 To what extent do you agree or disagree that if I needed help, there are people who would be there for me 
chisq.test(table(factor(cls_data1$FrndSat1), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FrndSat1), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FrndSat1_um<-table(cls_data1$FrndSat1,cls_data1$cls1_outcome);cross_FrndSat1_um
prop.table(cross_FrndSat1_um,margin = 2)*100
summary(cls_data1$FrndSat1)

cross_FrndSat1<-table(cls_f2f_push2web$FrndSat1, cls_f2f_push2web $cls1_outcome);cross_FrndSat1
summary(cls_f2f_push2web $FrndSat1)
#===============================================================================

#FrndSat2 To what extent do you agree or disagree that if I wanted company or to socialise, there are people I can call on 
chisq.test(table(factor(cls_data1$FrndSat2), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FrndSat2), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FrndSat2_um<-table(cls_data1$FrndSat2,cls_data1$cls1_outcome);cross_FrndSat2_um
prop.table(cross_FrndSat2_um,margin = 2)*100
summary(cls_data1$FrndSat2)

cross_FrndSat2<-table(cls_f2f_push2web $FrndSat2, cls_f2f_push2web $cls1_outcome);cross_FrndSat2
summary(cls_f2f_push2web $FrndSat2)
#===============================================================================

#SBeNeigh How strongly do you feel you belong to: Your immediate neighbourhood 								
chisq.test(table(factor(cls_data1$SBeNeigh), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $SBeNeigh), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_SBeNeigh_um<-table(cls_data1$SBeNeigh,cls_data1$cls1_outcome);cross_SBeNeigh_um
prop.table(cross_SBeNeigh_um,margin = 2)*100
summary(cls_data1$SBeNeigh)

cross_SBeNeigh<-table(cls_f2f_push2web $SBeNeigh, cls_f2f_push2web $cls1_outcome);cross_SBeNeigh
summary(cls_f2f_push2web $SBeNeigh)
#===============================================================================

#SchatN How often do you chat to any of your neighbours, more than to just say hello * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$SchatN), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $SchatN), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_SchatN_um<-table(cls_data1$SchatN,cls_data1$cls1_outcome);cross_SchatN_um
prop.table(cross_SchatN_um,margin = 2)*100
summary(cls_data1$SchatN)

cross_SchatN<-table(cls_f2f_push2web $SchatN, cls_f2f_push2web $cls1_outcome);cross_SchatN
summary(cls_f2f_push2web $SchatN)
#===============================================================================

#NComfort1 How comfortable would you be asking a neighbour to keep a set of keys to your home for emergencies 
chisq.test(table(factor(cls_data1$NComfort1), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $NComfort1), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_NComfort1_nu<-table(cls_data1$NComfort1, cls_data1$cls1_outcome);cross_NComfort1_nu
prop.table(cross_NComfort1_nu,margin = 2)*100
summary(cls_data1$NComfort1)

cross_NComfort1<-table(cls_f2f_push2web $NComfort1, cls_f2f_push2web $cls1_outcome);cross_NComfort1
summary(cls_f2f_push2web $NComfort1)
#===============================================================================

#NComfort3 How comfortable would you be asking a neighbour to collect a few shopping essentials if you were ill and at home on your own 
chisq.test(table(factor(cls_data1$NComfort3), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $NComfort3), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_NComfort3_nu<-table(cls_data1$NComfort3, cls_data1$cls1_outcome);cross_NComfort3_nu
prop.table(cross_NComfort3_nu,margin = 2)*100
summary(cls_data1$NComfort3)

cross_NComfort3<-table(cls_f2f_push2web $NComfort3, cls_f2f_push2web $cls1_outcome);cross_NComfort3
summary(cls_f2f_push2web $NComfort3)
#===============================================================================

#SPull Whether agree or disagree that: People in this neighbourhood pull together to improve the neighbourhood * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$SPull), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $SPull), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_SPull_nu<-table(cls_data1$SPull, cls_data1$cls1_outcome);cross_SPull_nu
prop.table(cross_SPull_nu,margin = 2)*100
summary(cls_data1$SPull)

cross_SPull<-table(cls_f2f_push2web $SPull, cls_f2f_push2web $cls1_outcome);cross_SPull
summary(cls_f2f_push2web $SPull)
#===============================================================================

#STrust Trust in people living in neighbourhood 
chisq.test(table(factor(cls_data1$STrust), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $STrust), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_STrust_nu<-table(cls_data1$STrust, cls_data1$cls1_outcome);cross_STrust_nu
prop.table(cross_STrust_nu,margin = 2)*100
summary(cls_data1$STrust)

cross_STrust<-table(cls_f2f_push2web $STrust, cls_f2f_push2web $cls1_outcome);cross_STrust
summary(cls_f2f_push2web $STrust)
#===============================================================================

#Slocsat Satisfaction with local area as a place to live * Samplegroup Sample group		
chisq.test(table(factor(cls_data1$Slocsat), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Slocsat), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Slocsat_nu<-table(cls_data1$Slocsat, cls_data1$cls1_outcome);cross_Slocsat_nu
prop.table(cross_Slocsat_nu,margin = 2)*100
summary(cls_data1$Slocsat)

cross_Slocsat<-table(cls_f2f_push2web $Slocsat, cls_f2f_push2web $cls1_outcome);cross_Slocsat
summary(cls_f2f_push2web $Slocsat)
#===============================================================================

#STogeth To what extent do you agree or disagree that this local area is a place where people from different backgrounds get on well together? * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$STogeth), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $STogeth), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_STogeth_nu<-table(cls_data1$STogeth, cls_data1$cls1_outcome);cross_STogeth_nu
prop.table(cross_STogeth_nu,margin = 2)*100
summary(cls_data1$STogeth)

cross_STogeth<-table(cls_f2f_push2web $STogeth, cls_f2f_push2web $cls1_outcome);cross_STogeth
summary(cls_f2f_push2web $STogeth)
#===============================================================================

#BetWors On the whole, do you think that over the past two years this area has got better or worse to live in or would you say that things haven't changed much? * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$BetWors), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $BetWors), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_BetWors_nu<-table(cls_data1$BetWors, cls_data1$cls1_outcome);cross_BetWors_nu
prop.table(cross_BetWors_nu,margin = 2)*100
summary(cls_data1$BetWors)

cross_BetWors<-table(cls_f2f_push2web $BetWors, cls_f2f_push2web $cls1_outcome);cross_BetWors
summary(cls_f2f_push2web $BetWors)
#===============================================================================

#SatAsset Generally how satisfied are you with the local services and amenities * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$SatAsset), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $SatAsset), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_SatAsset_nu<-table(cls_data1$SatAsset, cls_data1$cls1_outcome);cross_SatAsset_nu
prop.table(cross_SatAsset_nu,margin = 2)*100
summary(cls_data1$SatAsset)

cross_SatAsset<-table(cls_f2f_push2web $SatAsset, cls_f2f_push2web $cls1_outcome);cross_SatAsset
summary(cls_f2f_push2web $SatAsset)
#===============================================================================

#CivParta In the last 12 months have you done any of the following: Contacted a local offical such as local councillor, MP etc 
chisq.test(table(factor(cls_data1$CivParta), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivParta), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivParta_un<-table(cls_data1$CivParta, cls_data1$cls1_outcome);cross_CivParta_un
prop.table(cross_CivParta_un,margin = 2)*100
summary(cls_data1$CivParta)

cross_CivParta<-table(cls_f2f_push2web $CivParta, cls_f2f_push2web $cls1_outcome);cross_CivParta
summary(cls_f2f_push2web $CivParta)
#===============================================================================

#CivPartb In the last 12 months have you done any of the following: Attended a public rally or meeting, taken part in a public demonstration or protest
chisq.test(table(factor(cls_data1$CivPartb), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivPartb), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivPartb_un<-table(cls_data1$CivPartb, cls_data1$cls1_outcome);cross_CivPartb_un
prop.table(cross_CivPartb_un,margin = 2)*100
summary(cls_data1$CivPartb)

cross_CivPartb<-table(cls_f2f_push2web $CivPartb, cls_f2f_push2web $cls1_outcome);cross_CivPartb
summary(cls_f2f_push2web $CivPartb)
#===============================================================================

#CivPartc In the last 12 months have you done any of the following: Signed a paper petition or an online/e-petition
chisq.test(table(factor(cls_data1$CivPartc), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivPartc), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivPartc_un<-table(cls_data1$CivPartc, cls_data1$cls1_outcome);cross_CivPartc_un
prop.table(cross_CivPartc_un,margin = 2)*100
summary(cls_data1$CivPartc)

cross_CivPartc<-table(cls_f2f_push2web $CivPartc, cls_f2f_push2web $cls1_outcome);cross_CivPartc
summary(cls_f2f_push2web $CivPartc)
#===============================================================================

#CivPartd In the last 12 months have you done any of the following: None of the above 
chisq.test(table(factor(cls_data1$CivPartd), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivPartd), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivPartd_un<-table(cls_data1$CivPartd, cls_data1$cls1_outcome);cross_CivPartd_un
prop.table(cross_CivPartd_un,margin = 2)*100
summary(cls_data1$CivPartd)

cross_CivPartd<-table(cls_f2f_push2web $CivPartd, cls_f2f_push2web $cls1_outcome);cross_CivPartd
summary(cls_f2f_push2web $CivPartd)
#===============================================================================

#CivConsa In the last 12 months have you taken part in a consultation about local serives or problems in your local area through: completing a paper or online questionnaire 
chisq.test(table(factor(cls_data1$CivConsa), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivConsa), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivConsa_un<-table(cls_data1$CivConsa, cls_data1$cls1_outcome);cross_CivConsa_un
prop.table(cross_CivConsa_un,margin = 2)*100
summary(cls_data1$CivConsa)

cross_CivConsa<-table(cls_f2f_push2web $CivConsa, cls_f2f_push2web $cls1_outcome);cross_CivConsa
summary(cls_f2f_push2web $CivConsa)
#===============================================================================

#CivConsb In the last 12 months have you taken part in a consultation about local serives or problems in your local area through: attending a public meeting
chisq.test(table(factor(cls_data1$CivConsb), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivConsb), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivConsb_un<-table(cls_data1$CivConsb, cls_data1$cls1_outcome);cross_CivConsb_un
prop.table( cross_CivConsb_un,margin = 2)*100
summary(cls_data1$CivConsb)

cross_CivConsb<-table(cls_f2f_push2web $CivConsb, cls_f2f_push2web $cls1_outcome);cross_CivConsb
summary(cls_f2f_push2web $CivConsb)
#===============================================================================

#CivConsd In the last 12 months have you taken part in a consultation about local serives or problems in your local area through: None of the above
chisq.test(table(factor(cls_data1$CivConsd), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivConsd), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivConsd_un<-table(cls_data1$CivConsd, cls_data1$cls1_outcome);cross_CivConsd_un
prop.table( cross_CivConsd_un,margin = 2)*100
summary(cls_data1$CivConsd)

cross_CivConsd<-table(cls_f2f_push2web $CivConsd, cls_f2f_push2web $cls1_outcome);cross_CivConsd
summary(cls_f2f_push2web $CivConsd)
#===============================================================================

#CivAct28 None of these 
chisq.test(table(factor(cls_data1$CivAct28), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $CivAct28), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_CivAct28_un<-table(cls_data1$CivAct28, cls_data1$cls1_outcome);cross_CivAct28_un
prop.table(cross_CivAct28_un,margin = 2)*100
summary(cls_data1$CivAct28)

cross_CivAct28<-table(cls_f2f_push2web $CivAct28, cls_f2f_push2web $cls1_outcome);cross_CivAct28
summary(cls_f2f_push2web $CivAct28)
#===============================================================================

#PAffLoc Agreement that: You can influence decisions affecting your local area * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$PAffLoc), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $PAffLoc), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_PAffLoc_un<-table(cls_data1$PAffLoc, cls_data1$cls1_outcome);cross_PAffLoc_un
prop.table( cross_PAffLoc_un,margin = 2)*100
summary(cls_data1$PAffLoc)

cross_PAffLoc<-table(cls_f2f_push2web $PAffLoc, cls_f2f_push2web $cls1_outcome);cross_PAffLoc
summary(cls_f2f_push2web $PAffLoc)
#===============================================================================

#PInfl How important is it for you personally to feel that you can influence decisions in your local area? * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$PInfl), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $PInfl), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_PInfl_un<-table(cls_data1$PInfl, cls_data1$cls1_outcome);cross_PInfl_un
prop.table( cross_PInfl_un,margin = 2)*100
summary(cls_data1$PInfl)

cross_PInfl<-table(cls_f2f_push2web $PInfl, cls_f2f_push2web $cls1_outcome);cross_PInfl
summary(cls_f2f_push2web $PInfl)
#===============================================================================

#FifGp2 Formal volunteering in last 12 months * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$FifGp2), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $FifGp2), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_FifGp2_un<-table(cls_data1$FifGp2, cls_data1$cls1_outcome);cross_FifGp2_un
prop.table(cross_FifGp2_un,margin = 2)*100
summary(cls_data1$FifGp2)

cross_FifGp2<-table(cls_f2f_push2web $FifGp2, cls_f2f_push2web $cls1_outcome);cross_FifGp2
summary(cls_f2f_push2web $FifGp2)

#===============================================================================

#IHlp1 Unpaid help given to individuals (not including relatives): Keeping in touch with someone who has difficulty getting out and about (visiting in person, telephoning or e-mailing)
chisq.test(table(factor(cls_data1$IHlp1), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp1), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp1_un<-table(cls_data1$IHlp1, cls_data1$cls1_outcome);cross_IHlp1_un
prop.table(cross_IHlp1_un,margin = 2)*100
summary(cls_data1$IHlp1)


cross_IHlp1<-table(cls_f2f_push2web $IHlp1, cls_f2f_push2web $cls1_outcome);cross_IHlp1
summary(cls_f2f_push2web $IHlp1)
#===============================================================================

#IHlp2 Unpaid help given to individuals (not including relatives): Doing shopping, collecting pension or paying bills 
chisq.test(table(factor(cls_data1$IHlp2), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp2), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp2_un<-table(cls_data1$IHlp2, cls_data1$cls1_outcome);cross_IHlp2_un
prop.table(cross_IHlp2_un,margin = 2)*100
summary(cls_data1$IHlp2)

cross_IHlp2<-table(cls_f2f_push2web $IHlp2, cls_f2f_push2web $cls1_outcome);cross_IHlp2
summary(cls_f2f_push2web $IHlp2)
#===============================================================================

#IHlp3 Unpaid help given to individuals (not including relatives): Cooking, cleaning, laundry, gardening or other routine household jobs 
chisq.test(table(factor(cls_data1$IHlp3), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp3), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp3_un<-table(cls_data1$IHlp3, cls_data1$cls1_outcome);cross_IHlp3_un
prop.table(cross_IHlp3_un,margin = 2)*100
summary(cls_data1$IHlp3)

cross_IHlp3<-table(cls_f2f_push2web $IHlp3, cls_f2f_push2web $cls1_outcome);cross_IHlp3
summary(cls_f2f_push2web $IHlp3)
#===============================================================================

#IHlp4 Unpaid help given to individuals (not including relatives): Decorating, or doing any kind of home or car repairs  
chisq.test(table(factor(cls_data1$IHlp4), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp4), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp4_un<-table(cls_data1$IHlp4, cls_data1$cls1_outcome);cross_IHlp4_un
prop.table(cross_IHlp4_un,margin = 2)*100
summary(cls_data1$IHlp4)

cross_IHlp4<-table(cls_f2f_push2web $IHlp4, cls_f2f_push2web $cls1_outcome);cross_IHlp4
summary(cls_f2f_push2web $IHlp4)
#===============================================================================

#IHlp5 Unpaid help given to individuals (not including relatives): Baby sitting or caring for children  
chisq.test(table(factor(cls_data1$IHlp5), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp5), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp5_un<-table(cls_data1$IHlp5, cls_data1$cls1_outcome);cross_IHlp5_un
prop.table(cross_IHlp5_un,margin = 2)*100
summary(cls_data1$IHlp5)

cross_IHlp5<-table(cls_f2f_push2web $IHlp5, cls_f2f_push2web $cls1_outcome);cross_IHlp5
summary(cls_f2f_push2web $IHlp5)
#===============================================================================)

#IHlp7 Unpaid help given to individuals (not including relatives): Looking after a property or a pet for someone who is away
chisq.test(table(factor(cls_data1$IHlp7), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp7), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp7_un<-table(cls_data1$IHlp7, cls_data1$cls1_outcome);cross_IHlp7_un
prop.table(cross_IHlp7_un,margin = 2)*100
summary(cls_data1$IHlp7)

cross_IHlp7<-table(cls_f2f_push2web $IHlp7, cls_f2f_push2web $cls1_outcome);cross_IHlp7
summary(cls_f2f_push2web $IHlp7)
#===============================================================================

#IHlp8 Unpaid help given to individuals (not including relatives): Giving advice
chisq.test(table(factor(cls_data1$IHlp8), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp8), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp8_un<-table(cls_data1$IHlp8, cls_data1$cls1_outcome);cross_IHlp8_un
prop.table(cross_IHlp8_un,margin = 2)*100
summary(cls_data1$IHlp8)

cross_IHlp8<-table(cls_f2f_push2web $IHlp8, cls_f2f_push2web $cls1_outcome);cross_IHlp8
summary(cls_f2f_push2web $IHlp8)
#===============================================================================

#IHlp9 Unpaid help given to individuals (not including relatives): Writing letters or filling in forms 
chisq.test(table(factor(cls_data1$IHlp9), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp9), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp9_un<-table(cls_data1$IHlp9, cls_data1$cls1_outcome);cross_IHlp9_un
prop.table(cross_IHlp9_un,margin = 2)*100
summary(cls_data1$IHlp9)

cross_IHlp9<-table(cls_f2f_push2web $IHlp9, cls_f2f_push2web $cls1_outcome);cross_IHlp9
summary(cls_f2f_push2web $IHlp9)
#===============================================================================

#IHlp11 Unpaid help given to individuals (not including relatives): Transporting or escorting someone (for example to a hospital or on an outing)
chisq.test(table(factor(cls_data1$IHlp11), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp11), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp11_un<-table(cls_data1$IHlp11, cls_data1$cls1_outcome);cross_IHlp11_un
prop.table(cross_IHlp11_un,margin = 2)*100
summary(cls_data1$IHlp11)

cross_IHlp11<-table(cls_f2f_push2web $IHlp11, cls_f2f_push2web $cls1_outcome);cross_IHlp11
summary(cls_f2f_push2web $IHlp11)
#===============================================================================

#IHlp13 Unpaid help given to individuals (not including relatives): No help given in last 12 months 
chisq.test(table(factor(cls_data1$IHlp13), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $IHlp13), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_IHlp13_un<-table(cls_data1$IHlp13, cls_data1$cls1_outcome);cross_IHlp13_un
prop.table(cross_IHlp13_un,margin = 2)*100
summary(cls_data1$IHlp13)

cross_IHlp13<-table(cls_f2f_push2web $IHlp13, cls_f2f_push2web $cls1_outcome);cross_IHlp13
summary(cls_f2f_push2web $IHlp13)
#===============================================================================

#GGroup2a Gave to charity in last 4 weeks: Money to collecting tins (e.g. door-to-door collection, in the street, in a pub, at work, on a shop counter, etc)
chisq.test(table(factor(cls_data1$GGroup2a), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2a), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2a_un<-table(cls_data1$GGroup2a, cls_data1$cls1_outcome);cross_GGroup2a_un
prop.table(cross_GGroup2a_un,margin = 2)*100
summary(cls_data1$GGroup2a)

cross_GGroup2a<-table(cls_f2f_push2web $GGroup2a, cls_f2f_push2web $cls1_outcome);cross_GGroup2a
summary(cls_f2f_push2web $GGroup2a)
#===============================================================================

#GGroup2b Gave to charity in last 4 weeks: Collection at church, mosque, other place of worship 
chisq.test(table(factor(cls_data1$GGroup2b), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2b), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2b_un<-table(cls_data1$GGroup2b, cls_data1$cls1_outcome);cross_GGroup2b_un
prop.table(cross_GGroup2b_un,margin = 2)*100
summary(cls_data1$GGroup2b)

cross_GGroup2b<-table(cls_f2f_push2web $GGroup2b, cls_f2f_push2web $cls1_outcome);cross_GGroup2b
summary(cls_f2f_push2web $GGroup2b)
#===============================================================================

#GGroup2c Gave to charity in last 4 weeks: Collections using a charity envelope/cheque in the post 
chisq.test(table(factor(cls_data1$GGroup2c), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2c), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2c_un<-table(cls_data1$GGroup2c, cls_data1$cls1_outcome);cross_GGroup2c_un
prop.table(cross_GGroup2c_un,margin = 2)*100
summary(cls_data1$GGroup2c)


cross_GGroup2c<-table(cls_f2f_push2web $GGroup2c, cls_f2f_push2web $cls1_outcome);cross_GGroup2c
summary(cls_f2f_push2web $GGroup2c)
#===============================================================================

#GGroup2e Gave to charity in last 4 weeks: Direct debit or standing order 
chisq.test(table(factor(cls_data1$GGroup2e), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2e), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2e_un<-table(cls_data1$GGroup2e, cls_data1$cls1_outcome);cross_GGroup2e_un
prop.table(cross_GGroup2e_un,margin = 2)*100
summary(cls_data1$GGroup2e)

cross_GGroup2e<-table(cls_f2f_push2web $GGroup2e, cls_f2f_push2web $cls1_outcome);cross_GGroup2e
summary(cls_f2f_push2web $GGroup2e)
#===============================================================================

#GGroup2f Gave to charity in last 4 weeks: Giving to people begging on the street 
chisq.test(table(factor(cls_data1$GGroup2f), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2f), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2f_un<-table(cls_data1$GGroup2f, cls_data1$cls1_outcome);cross_GGroup2f_un
prop.table(cross_GGroup2f_un,margin = 2)*100
summary(cls_data1$GGroup2f)

cross_GGroup2f<-table(cls_f2f_push2web $GGroup2f, cls_f2f_push2web $cls1_outcome);cross_GGroup2f
summary(cls_f2f_push2web $GGroup2f)
#===============================================================================

#GGroup2g Gave to charity in last 4 weeks: Donation - in person or on phone (excluding online or via text message) 
chisq.test(table(factor(cls_data1$GGroup2g), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2g), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2g_un<-table(cls_data1$GGroup2g, cls_data1$cls1_outcome);cross_GGroup2g_un
prop.table(cross_GGroup2g_un,margin = 2)*100
summary(cls_data1$GGroup2g)

cross_GGroup2g<-table(cls_f2f_push2web $GGroup2g, cls_f2f_push2web $cls1_outcome);cross_GGroup2g
summary(cls_f2f_push2web $GGroup2g)
#===============================================================================

#GGroup2h Gave to charity in last 4 weeks: Donation - online/via website 
chisq.test(table(factor(cls_data1$GGroup2h), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2h), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2h_un<-table(cls_data1$GGroup2h, cls_data1$cls1_outcome);cross_GGroup2h_un
prop.table(cross_GGroup2h_un,margin = 2)*100
summary(cls_data1$GGroup2h)

cross_GGroup2h<-table(cls_f2f_push2web $GGroup2h, cls_f2f_push2web $cls1_outcome);cross_GGroup2h
summary(cls_f2f_push2web $GGroup2h)
#===============================================================================

#GGroup2k Gave to charity in last 4 weeks: Buying raffle tickets 
chisq.test(table(factor(cls_data1$GGroup2k), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2k), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2k_un<-table(cls_data1$GGroup2k, cls_data1$cls1_outcome);cross_GGroup2k_un
prop.table(cross_GGroup2k_un,margin = 2)*100
summary(cls_data1$GGroup2k)

cross_GGroup2k<-table(cls_f2f_push2web $GGroup2k, cls_f2f_push2web $cls1_outcome);cross_GGroup2k
summary(cls_f2f_push2web $GGroup2k)
#===============================================================================

#GGroup2l Gave to charity in last 4 weeks: Buying goods from charity shop or catalogue 
chisq.test(table(factor(cls_data1$GGroup2l), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2l), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2l_un<-table(cls_data1$GGroup2l, cls_data1$cls1_outcome);cross_GGroup2l_un
prop.table(cross_GGroup2l_un,margin = 2)*100
summary(cls_data1$GGroup2l)

cross_GGroup2l<-table(cls_f2f_push2web $GGroup2l, cls_f2f_push2web $cls1_outcome);cross_GGroup2l
summary(cls_f2f_push2web $GGroup2l)
#===============================================================================

#GGroup2m Gave to charity in last 4 weeks: Making a purchase where the price includes a charitable donation/or where you can add a charitable donation
chisq.test(table(factor(cls_data1$GGroup2m), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2m), cls_f2f_push2web $cls1_outcome), correct=FALSE)
cross_GGroup2m_un<-table(cls_data1$GGroup2m, cls_data1$cls1_outcome);cross_GGroup2m_un
prop.table(cross_GGroup2m_un,margin = 2)*100
summary(cls_data1$GGroup2m)
cross_GGroup2m<-table(cls_f2f_push2web $GGroup2m, cls_f2f_push2web $cls1_outcome);cross_GGroup2m
summary(cls_f2f_push2web $GGroup2m)
#===============================================================================

#GGroup2n Gave to charity in last 4 weeks: Buying tickets or spending money at fundraising events
chisq.test(table(factor(cls_data1$GGroup2n), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2n), cls_f2f_push2web $cls1_outcome), correct=FALSE)
cross_GGroup2n_un<-table(cls_data1$GGroup2n, cls_data1$cls1_outcome);cross_GGroup2n_un
prop.table(cross_GGroup2n_un,margin = 2)*100
summary(cls_data1$GGroup2n)
cross_GGroup2n<-table(cls_f2f_push2web $GGroup2n, cls_f2f_push2web $cls1_outcome);cross_GGroup2n
summary(cls_f2f_push2web $GGroup2n)
#===============================================================================
#GGroup2o Gave to charity in last 4 weeks: Sponsorship (not online) 
chisq.test(table(factor(cls_data1$GGroup2o), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2o), cls_f2f_push2web $cls1_outcome), correct=FALSE)
cross_GGroup2o_un<-table(cls_data1$GGroup2o, cls_data1$cls1_outcome);cross_GGroup2o_un
prop.table(cross_GGroup2o_un,margin = 2)*100
summary(cls_data1$GGroup2o)
cross_GGroup2o<-table(cls_f2f_push2web $GGroup2o, cls_f2f_push2web $cls1_outcome);cross_GGroup2o
summary(cls_f2f_push2web $GGroup2o)
#===============================================================================

#GGroup2p Gave to charity in last 4 weeks: Sponsorship (online) 
chisq.test(table(factor(cls_data1$GGroup2p), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2p), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_GGroup2p_un<-table(cls_data1$GGroup2p, cls_data1$cls1_outcome);cross_GGroup2p_un
prop.table(cross_GGroup2p_un,margin = 2)*100
summary(cls_data1$GGroup2p)

cross_GGroup2p<-table(cls_f2f_push2web $GGroup2p, cls_f2f_push2web $cls1_outcome);cross_GGroup2p
summary(cls_f2f_push2web $GGroup2p)
#===============================================================================

#GGroup2u Gave to charity in last 4 weeks: Did not give to charity
chisq.test(table(factor(cls_data1$GGroup2u), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $GGroup2u), cls_f2f_push2web $cls1_outcome), correct=FALSE)
cross_GGroup2u_un<-table(cls_data1$GGroup2u, cls_data1$cls1_outcome);cross_GGroup2u_un
prop.table(cross_GGroup2t_un,margin = 2)*100
summary(cls_data1$GGroup2u)
cross_GGroup2u<-table(cls_f2f_push2web $GGroup2u, cls_f2f_push2web $cls1_outcome);cross_GGroup2u
summary(cls_f2f_push2web $GGroup2u)
#===============================================================================

#Givech3 Given money to charity in past 4 weeks 								
chisq.test(table(factor(cls_data1$Givech3), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Givech3), cls_f2f_push2web $cls1_outcome), correct=FALSE)
cross_Givech3_un<-table(cls_data1$Givech3, cls_data1$cls1_outcome);cross_Givech3_un
prop.table(cross_Givech3_un,margin = 2)*100
summary(cls_data1$Givech3)
cross_Givech3<-table(cls_f2f_push2web $Givech3, cls_f2f_push2web $cls1_outcome);cross_Givech3
summary(cls_f2f_push2web $Givech3)
#===============================================================================

#Teuse3_1 Would encourage to donate: Having more information about the different charities or organisations that I could support 
chisq.test(table(factor(cls_data1$Teuse3_1), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_1), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_1_un<-table(cls_data1$Teuse3_1, cls_data1$cls1_outcome);cross_Teuse3_1_un
prop.table(cross_Teuse3_1_un,margin = 2)*100
summary(cls_data1$Teuse3_1)

cross_Teuse3_1<-table(cls_f2f_push2web $Teuse3_1, cls_f2f_push2web $cls1_outcome);cross_Teuse3_1
summary(cls_f2f_push2web $Teuse3_1)
#===============================================================================

#Teuse3_3 Would encourage to donate: Receiving letter/email of thanks from the charity or organisation
chisq.test(table(factor(cls_data1$Teuse3_3), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_3), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_3_un<-table(cls_data1$Teuse3_3, cls_data1$cls1_outcome);cross_Teuse3_3_un
prop.table(cross_Teuse3_3_un,margin = 2)*100
summary(cls_data1$Teuse3_3)

cross_Teuse3_3<-table(cls_f2f_push2web $Teuse3_3, cls_f2f_push2web $cls1_outcome);cross_Teuse3_3
summary(cls_f2f_push2web $Teuse3_3)
#===============================================================================

#Teuse3_4 Would encourage to donate: Receiving information from the charity or organisation explaining what has
chisq.test(table(factor(cls_data1$Teuse3_4), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_4), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_4_un<-table(cls_data1$Teuse3_4, cls_data1$cls1_outcome);cross_Teuse3_4_un
prop.table(cross_Teuse3_4_un,margin = 2)*100
summary(cls_data1$Teuse3_4)

cross_Teuse3_4<-table(cls_f2f_push2web $Teuse3_4, cls_f2f_push2web $cls1_outcome);cross_Teuse3_4
summary(cls_f2f_push2web $Teuse3_4)
#===============================================================================

#Teuse3_6 Would encourage to donate: Confidence that the charity or organisation uses the money efficiently 
chisq.test(table(factor(cls_data1$Teuse3_6), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_6), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_6_un<-table(cls_data1$Teuse3_6, cls_data1$cls1_outcome);cross_Teuse3_6_un
prop.table(cross_Teuse3_6_un,margin = 2)*100
summary(cls_data1$Teuse3_6)

cross_Teuse3_6<-table(cls_f2f_push2web $Teuse3_6, cls_f2f_push2web $cls1_outcome);cross_Teuse3_6
summary(cls_f2f_push2web $Teuse3_6)
#===============================================================================

#Teuse3_7 Would encourage to donate: Being able to give money by tax efficient methods 
chisq.test(table(factor(cls_data1$Teuse3_7), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_7), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_7_un<-table(cls_data1$Teuse3_7, cls_data1$cls1_outcome);cross_Teuse3_7_un
prop.table(cross_Teuse3_7_un,margin = 2)*100
summary(cls_data1$Teuse3_7)

cross_Teuse3_7<-table(cls_f2f_push2web $Teuse3_7, cls_f2f_push2web $cls1_outcome);cross_Teuse3_7
summary(cls_f2f_push2web $Teuse3_7)
#===============================================================================

#Teuse3_8 Would encourage to donate: More generous tax relief 
chisq.test(table(factor(cls_data1$Teuse3_8), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_8), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_8_un<-table(cls_data1$Teuse3_8, cls_data1$cls1_outcome);cross_Teuse3_8_un
prop.table(cross_Teuse3_8_un,margin = 2)*100
summary(cls_data1$Teuse3_8)

cross_Teuse3_8<-table(cls_f2f_push2web $Teuse3_8, cls_f2f_push2web $cls1_outcome);cross_Teuse3_8
summary(cls_f2f_push2web $Teuse3_8)
#===============================================================================

#Teuse3_9 Would encourage to donate: Being asked by a friend or family member 
chisq.test(table(factor(cls_data1$Teuse3_9), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_9), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_9_un<-table(cls_data1$Teuse3_9, cls_data1$cls1_outcome);cross_Teuse3_9_un
prop.table(cross_Teuse3_9_un,margin = 2)*100
summary(cls_data1$Teuse3_9)

cross_Teuse3_9<-table(cls_f2f_push2web $Teuse3_9, cls_f2f_push2web $cls1_outcome);cross_Teuse3_9
summary(cls_f2f_push2web $Teuse3_9)
#===============================================================================

#Teuse3_10 Would encourage to donate: If I had more money 
chisq.test(table(factor(cls_data1$Teuse3_10), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_10), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_10_un<-table(cls_data1$Teuse3_10, cls_data1$cls1_outcome);cross_Teuse3_10_un
prop.table(cross_Teuse3_10_un,margin = 2)*100
summary(cls_data1$Teuse3_10)

cross_Teuse3_10<-table(cls_f2f_push2web $Teuse3_10, cls_f2f_push2web $cls1_outcome);cross_Teuse3_10
summary(cls_f2f_push2web $Teuse3_10)
#===============================================================================

#Teuse3_13 Would encourage to donate: None of these 
chisq.test(table(factor(cls_data1$Teuse3_13), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Teuse3_13), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Teuse3_13_un<-table(cls_data1$Teuse3_13, cls_data1$cls1_outcome);cross_Teuse3_13_un
prop.table(cross_Teuse3_13_un,margin = 2)*100
summary(cls_data1$Teuse3_13)

cross_Teuse3_13<-table(cls_f2f_push2web $Teuse3_13, cls_f2f_push2web $cls1_outcome);cross_Teuse3_13
summary(cls_f2f_push2web $Teuse3_13)
#===============================================================================

#LocAtt Do you agree or disagree: when people get involved in their local area they can change the way the area is run * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$LocAtt), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $LocAtt), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_LocAtt13_un<-table(cls_data1$LocAtt, cls_data1$cls1_outcome);cross_LocAtt13_un
prop.table(cross_LocAtt13_un,margin = 2)*100
summary(cls_data1$LocAtt)

cross_LocAtt_13<-table(cls_f2f_push2web $LocAtt, cls_f2f_push2web $cls1_outcome);cross_LocAtt_13
summary(cls_f2f_push2web $LocAtt)
#===============================================================================

#LocInvNc Personally been involved in the last 12 months: Trying to stop something happening in my local area 
chisq.test(table(factor(cls_data1$LocInvNc), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $LocInvNc), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_LocInvNc_un<-table(cls_data1$LocInvNc, cls_data1$cls1_outcome);cross_LocInvNc_un
prop.table(cross_LocInvNc_un,margin = 2)*100
summary(cls_data1$LocInvNc)

cross_LocInvNc<-table(cls_f2f_push2web $LocInvNc, cls_f2f_push2web $cls1_outcome);cross_LocInvNc
summary(cls_f2f_push2web $LocInvNc)
#===============================================================================

#LocInvNg Personally been involved in the last 12 months: Trying to stop something happening in my local area 
chisq.test(table(factor(cls_data1$LocInvNg), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $LocInvNg), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_LocInvNg_un<-table(cls_data1$LocInvNg, cls_data1$cls1_outcome);cross_LocInvNg_un
prop.table(cross_LocInvNg_un,margin = 2)*100
summary(cls_data1$LocInvNg)

cross_LocInvNg<-table(cls_f2f_push2web $LocInvNg, cls_f2f_push2web $cls1_outcome);cross_LocInvNg
summary(cls_f2f_push2web $LocInvNg)
#===============================================================================

#WellB1 On a scale from 0 to 10, how satisfied are you with your life as a whole nowadays 
chisq.test(table(factor(cls_data1$WellB1), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $WellB1), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_WellB1_un<-table(cls_data1$WellB1, cls_data1$cls1_outcome);cross_WellB1_un
prop.table(cross_WellB1_un,margin = 2)*100
summary(cls_data1$WellB1)

cross_WellB1<-table(cls_f2f_push2web $WellB1, cls_f2f_push2web $cls1_outcome);cross_WellB1
summary(cls_f2f_push2web $WellB1)
#===============================================================================

#WellB2 On a scale from 0 to 10, how happy did you feel yesterday 
chisq.test(table(factor(cls_data1$WellB2), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $WellB2), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_WellB2_un<-table(cls_data1$WellB2, cls_data1$cls1_outcome);cross_WellB2_un
prop.table(cross_WellB2_un,margin = 2)*100
summary(cls_data1$WellB2)

cross_WellB2<-table(cls_f2f_push2web $WellB2, cls_f2f_push2web $cls1_outcome);cross_WellB2
summary(cls_f2f_push2web $WellB2)
#===============================================================================

#WellB3 On a scale from 0 to 10, how anxious did you feel yesterday 
chisq.test(table(factor(cls_data1$WellB3), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $WellB3), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_WellB3_un<-table(cls_data1$WellB3, cls_data1$cls1_outcome);cross_WellB3_un
prop.table(cross_WellB3_un,margin = 2)*100
summary(cls_data1$WellB3)

cross_WellB3<-table(cls_f2f_push2web $WellB3, cls_f2f_push2web $cls1_outcome);cross_WellB3
summary(cls_f2f_push2web $WellB3)
#===============================================================================

#WellB4 On a scale from 0 to 10, to what extent do you feel that the things you do in your life are worthwhile 
chisq.test(table(factor(cls_data1$WellB4), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $WellB4), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_WellB4_un<-table(cls_data1$WellB4, cls_data1$cls1_outcome);cross_WellB4_un
prop.table(cross_WellB4_un,margin = 2)*100
summary(cls_data1$WellB4); summary(cls_data1$cls1_outcome)

cross_WellB4<-table(cls_f2f_push2web $WellB4, cls_f2f_push2web $cls1_outcome);cross_WellB4
summary(cls_f2f_push2web $WellB4)
#===============================================================================

#LonOft How often do you feel lonely? * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$LonOft), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $LonOft), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_LonOft_un<-table(cls_data1$LonOft, cls_data1$cls1_outcome);cross_LonOft_un
summary(cls_data1$LonOft); summary(cls_data1$cls1_outcome)

cross_LonOft<-table(cls_f2f_push2web $LonOft, cls_f2f_push2web $cls1_outcome);cross_LonOft
summary(cls_f2f_push2web $LonOft)
#===============================================================================

#Relig2 What is your religion even if you are not currently practising? * Samplegroup Sample group									
chisq.test(table(factor(cls_data1$Relig2), cls_data1$cls1_outcome), correct=FALSE)
chisq.test(table(factor(cls_f2f_push2web $Relig2), cls_f2f_push2web $cls1_outcome), correct=FALSE)

cross_Relig2_un<-table(cls_data1$Relig2, cls_data1$cls1_outcome);cross_Relig2_un
summary(cls_data1$Relig2); summary(cls_data1$cls1_outcome)

cross_Relig2<-table(cls_f2f_push2web $Relig2, cls_f2f_push2web $cls1_outcome);cross_Relig2
summary(cls_f2f_push2web $Relig2)
#===============================================================================
#END
#===============================================================================