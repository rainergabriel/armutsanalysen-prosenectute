############################################################################################
############################################################################################
####
#### Purpose of script: Analyse SAS 22 data for poverty and non take up of welfare benefits 
#### Initial creation Date: 2022-08-03
#### Version Release: draft/working version
#### ---------------------------------------------------------------------------------------
####   ＿＿＿＿＿＿＿__＿＿
####  |￣￣￣￣￣￣￣￣￣￣|
####  | Created by         |
####  |  Rainer Gabriel    |
####  | Have a             |
####  |      nice day      |
####  |＿＿＿＿＿＿＿__＿＿|
####  (\__/)||
####  (•ㅅ•)||
####  / 　 づ
#### Email: rainer.gabriel@zhaw.ch
#### --------------------------------
####
############################################################################################
############################################################################################


# libraries ---------------------------------------------------------------

# if(!require(readxl)) {install.packages("readxl")}
# if(!require(sf)) {install.packages("sf",dependencies=TRUE)}
#   if(!require(eply)) {install.packages("eply")}
#     if(!require(ggthemes)) {install.packages("ggthemes")}
#       if(!require(rcartocolor)) {install.packages("rcartocolor")}
#         if(!require(ggtext)) {install.packages("ggtext")}
# if(!require(DescTools)) {install.packages("DescTools")}
# if(!require(PropCIs)) {install.packages("PropCIs")}
# if(!require(svglite)) {install.packages("svglite")}
# if(!require(Cairo)) {install.packages("Cairo",dependencies=TRUE)}
# if(!require(tidyverse)) {install.packages("tidyverse")}
# if(!require(questionr)) {install.packages("questionr")}
# if(!require(survey)) {install.packages("survey")}
# if(!require(parsnip)) {install.packages("parsnip")}
# if(!require(tidymodels)) {install.packages("tidymodels", dependencies=TRUE)}
# if(!require(finalfit)) {install.packages("finalfit", dependencies=TRUE)}
# if(!require(stargazer)) {install.packages("stargazer", dependencies=TRUE)}



library(tidyverse)
library(haven)
library(eply)
library(tidyr)
library(knitr)
library(questionr)
library(ggplot2)
library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)
library(questionr)
library(ggthemes)
library(survey)
library(ggtext)
library(DescTools)
library(PropCIs)
library(svglite)
library(parsnip)
library(finalfit)
library(ggrepel)
library(stargazer)


# loading data ------------------------------------------------------------
# 
# rm(list=ls())
# load(file="/Users/gabn/switchdrive/Altersmonitoring und Alterssurvey PS-CH/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2022-11-22.Rdata") #workmachine
# load(file="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2022-11-22.Rdata") #homemachine

load("~/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2022-11-22.Rdata")

############################################################################################
############################################################################################
####SETTING UP 
############################################################################################
############################################################################################

# subsetting nur pensionierte -------------------------------------------------------------

table(sas.22$canton_22)

sas.22 <- sas.22 %>% filter(pi_ageinterview_22>=65)

# setup target variables --------------------------------------------------

# creating a variable based on the categorical response for incomes where there are virtual fixed amounts 
sas.22$fi_income_22_category
levels(as.factor(sas.22$fi_income_22_category))
sas.22$virtual.amount.income.cat <- ifelse(sas.22$fi_income_22_category==0, median(0:2000), 
                                           ifelse(sas.22$fi_income_22_category==1, median(2000:2300), 
                                                  ifelse(sas.22$fi_income_22_category==2, median(2300:3000), 
                                                         ifelse(sas.22$fi_income_22_category==3, median(3000:4000), 
                                                                ifelse(sas.22$fi_income_22_category==4, median(4000:4800), 
                                                                       ifelse(sas.22$fi_income_22_category==5, median(4800:6000), 
                                                                              ifelse(sas.22$fi_income_22_category==6, median(6000:8000), 
                                                                                     ifelse(sas.22$fi_income_22_category==7, median(8000:15000),
                                                                                            NA))))))))
sas.22 %>%  count(virtual.amount.income.cat)

#merge the "virtual-fixed-amount-for-the-categorical-responses" and income responses 
sas.22$income.amount.merged <- sas.22$fi_income_22_amount
filter <- which(is.na(sas.22$fi_income_22_amount))
sas.22$income.amount.merged[filter] <- sas.22$virtual.amount.income.cat[filter]
length(which(is.na(sas.22$income.amount.merged))) #about 10% missing across both variables which is acceptable
sas.22 %>%  count(income.amount.merged)
plot(log(sas.22$income.amount.merged))
sas.22 %>% filter(income.amount.merged<500) %>% count(income.amount.merged)
min(sas.22$income.amount.merged,na.rm=TRUE)
max(sas.22$income.amount.merged,na.rm=TRUE)


#creating the equivalizing factor based on modified OECD methodology 
sas.22 %>% count(pi_nbhoushold_22)
length(which(is.na(sas.22$pi_nbhoushold_22)))

sas.22$household.equiv.weighting.factor <- ifelse(sas.22$pi_nbhoushold_22==1,1,
                                                  ifelse(sas.22$pi_nbhoushold_22==2,(1+(1*0.5)),
                                                         ifelse(sas.22$pi_nbhoushold_22==3,(1+(2*0.5)),
                                                                ifelse(sas.22$pi_nbhoushold_22==4,(1+(3*0.5)),
                                                                       ifelse(sas.22$pi_nbhoushold_22==5,(1+(4*0.5)), 
                                                                              ifelse(sas.22$pi_nbhoushold_22==6,(1+(5*0.5)), 
                                                                                     ifelse(sas.22$pi_nbhoushold_22==7,(1+(6*0.5)), 
                                                                                            ifelse(sas.22$pi_nbhoushold_22==8,(1+(7*0.5)), 
                                                                                                   ifelse(sas.22$pi_nbhoushold_22==9,(1+(8*0.5)), 
                                                                                                          NA)))))))))
sas.22 %>% count(household.equiv.weighting.factor)


#correcting those who live with children (weighted with 0.3) and not with adults (weighted 0.5)
filter <- which(sas.22$pi_nbhoushold_22==2 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1.3
filter <- which(sas.22$pi_nbhoushold_22==3 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+0.5+0.3
filter <- which(sas.22$pi_nbhoushold_22==4 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(2*0.5)+0.3
filter <- which(sas.22$pi_nbhoushold_22==5 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(3*0.5)+0.3
filter <- which(sas.22$pi_nbhoushold_22==6 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(4*0.5)+0.3
filter <- which(sas.22$pi_nbhoushold_22==7 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(5*0.5)+0.3

sas.22 %>% count(household.equiv.weighting.factor)

# calculating equivalized household income variable

sas.22$equiv.hh.income <- sas.22$income.amount.merged/(sas.22$household.equiv.weighting.factor)
length(which(is.na(sas.22$equiv.hh.income)))

boxplot(log((sas.22$equiv.hh.income)))

                                                                
# creating a binary poverty variable 

sas.22$poverty.bn <- ifelse(is.na(sas.22$equiv.hh.income),NA, 
                            ifelse(sas.22$equiv.hh.income<=2279,1,0) #based on 2020 SKOS / BFS richtlinien
                            )


# create armutsgefährdung variable 
 
rel.poverty.line <- 2506 #gemäss BFS 2020

sas.22$rel.poverty.bn <- ifelse(is.na(sas.22$equiv.hh.income),NA, 
                                ifelse(sas.22$equiv.hh.income<=rel.poverty.line,1,0) #based on 60% median income (silc)
)

# check the deprivation variable 
sas.22$fi_suddenexp_22
sas.22 %>% count(fi_suddenexp_22)
sas.22$fi_endsmeet_22

sas.22 <- sas.22 %>% mutate(
  cant.face.sudden.exp = case_when(
  fi_suddenexp_22 == 0 ~ 1,
  fi_suddenexp_22 == 1 ~ 0), 
  cant.holiday.away = case_when(
    mu_affordholiday_22 == 0 ~ 1,
    mu_affordholiday_22 == 1 ~ 0),
  difficulty.make.ends.meet = case_when(
    fi_endsmeet_22>=2 ~ 0, 
    fi_endsmeet_22<2 ~ 1)
  ) 
sas.22 %>% select(difficulty.make.ends.meet, cant.holiday.away, cant.face.sudden.exp)

# difficulties making ends meet 

prop.table(table(sas.22$fi_endsmeet_22))
sas.22$fi_endsmeet_22


#multi poverty

sas.22$poor.deprived.subjectivepoor <- NA
complete <- complete.cases(subset(sas.22, select=c(poverty.bn,fi_endsmeet_22, cant.face.sudden.exp)))
length(which(!is.na(complete)))
filter <- which(sas.22$poverty.bn==1 
                & sas.22$difficulty.make.ends.meet==1
                & sas.22$cant.face.sudden.exp==1
                )
length(filter)
sas.22$poor.deprived.subjectivepoor[filter] <- 1
filter <- which(sas.22$poverty.bn==0 
                | sas.22$difficulty.make.ends.meet==0
                | sas.22$cant.face.sudden.exp==0
)
sas.22$poor.deprived.subjectivepoor[filter] <- 0
sas.22 %>%  count(poor.deprived.subjectivepoor) 
sas.22 %>%  count(poor.deprived.subjectivepoor) %>% summarise(n/sum(n))
# einkommensquellen 

# 1pill
sas.22 %>% count(fi_incsourc_22_1pill)
prop.table( wtd.table(x=as.factor(sas.22$fi_incsourc_22_1pill), 
                      weights=sas.22$cross.design.weights.sample_22))

# 2pill
prop.table( wtd.table(x=as.factor(sas.22$fi_incsourc_22_2pill), 
                      weights=sas.22$cross.design.weights.sample_22))


# EL
sas.22 %>% count(fi_incsourc_22_el)
prop.table( wtd.table(x=as.factor(sas.22$fi_incsourc_22_el), 
            weights=sas.22$cross.design.weights.sample_22))

# kantonale beihilfen
sas.22 %>% count(fi_incsourc_22_hous) %>% summarise(n/sum(n))

# erwerb
sas.22 %>% count(fi_incsourc_22_employment) %>% summarise(n/sum(n))

#unterstützung durch freunde und familie 
sas.22 %>% count(fi_incsourc_22_friendfam) %>% summarise(n/sum(n))

#cashout 
prop.table(wtd.table(x = as.factor(sas.22$fi_cashout2pil_22),
                     weights = sas.22$cross.design.weights.sample_22))

prop.table(wtd.table(x = as.factor(sas.22$fi_2pil4home_22),
                     weights = sas.22$cross.design.weights.sample_22))

#typologie einkommensquellen 

sas.22 %>% count(fi_incsourc_22_2pill)
sas.22 %>% count(fi_incsourc_22_employment)

# first, merge AHV with EL
sas.22$fi_incsourc_22_1pill.corr <- sas.22$fi_incsourc_22_1pill
filter <- which(sas.22$fi_incsourc_22_el==1)
sas.22$fi_incsourc_22_1pill.corr[filter] <- 1

sas.22 <- sas.22 %>% mutate(inc.source.type =case_when(
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 0~ "Nur 1. Säule",
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 1~ "1. Säule + Arbeit",
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 1 &fi_incsourc_22_employment == 0~ "1. + 2. Säule",
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 1 & fi_incsourc_22_employment == 1~ "1. + 2. Säule + Arbeit",
  fi_incsourc_22_1pill.corr == 0 & fi_incsourc_22_2pill == 1 & fi_incsourc_22_employment == 0~ "Nur 2. Säule",
  fi_incsourc_22_1pill.corr == 0 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 1 ~ "Nur Arbeit", 
  fi_incsourc_22_1pill.corr == 0 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 0 ~ "Andere"))



sas.22 %>% count(inc.source.type)

  sas.22$inc.source.type.rcd <- ifelse(
  sas.22$inc.source.type == "Andere" |  
  sas.22$inc.source.type =="Nur 2. Säule" | 
  sas.22$inc.source.type == "Nur Arbeit",
  "Andere",
  sas.22$inc.source.type)
  
  sas.22 %>% count(inc.source.type.rcd)
  


table(sas.22$inc.source.type, sas.22$pi_age.group_22)


# setup covariates


#sex
sas.22 %>% count(pi_sex_22)


# age group 
sas.22 %>% count(pi_age.group_22)

length(which(sas.22$pi_ageinterview_22>85))
sas.22 <- sas.22 %>% mutate(age.group.3cat =  case_when(
   # pi_ageinterview_22>=55 & pi_ageinterview_22 <=64 ~"55-64",
   pi_ageinterview_22>=65 & pi_ageinterview_22 <=74   ~"65-74",
   pi_ageinterview_22>=75   ~"75+")) 

prop.table(table(sas.22$age.group.3cat))

# migration 

 sas.22 %>%  count(pi_swissnational_22)
 
 #civstat recode
 
 sas.22 <- sas.22 %>% mutate(civstat.rcd = case_when(
   pi_civstat_22 == 0 | pi_civstat_22 == 1 ~ "Verheiratet / Regist.Part.", 
   pi_civstat_22 == 2 ~ "Ledig", 
   pi_civstat_22 == 3 ~ "Geschieden", 
   pi_civstat_22 == 4 ~ "Verwitwet"
 )) %>%  mutate(civstat.rcd=factor(civstat.rcd, levels=c("Verheiratet / Regist.Part.", 
                                                         "Ledig", 
                                                         "Geschieden", 
                                                         "Verwitwet"), ordered=TRUE))

 sas.22 %>%  count(civstat.rcd)

  # education 
 sas.22$se_edu_22
 sas.22 %>% count(se_edu_22)
 sas.22 <- sas.22 %>% mutate(edu.rcd = case_when(
   se_edu_22<4 ~ "low", 
   se_edu_22>=3 & se_edu_22<=7 ~"avg", 
   se_edu_22>=8 ~ "high"
 ))
 sas.22 %>% count(edu.rcd)
 sas.22$edu.rcd <- as.factor(sas.22$edu.rcd)
 is.list(sas.22$edu.rcd)
 is.factor(sas.22$edu.rcd)
 
 
 # household typology
 
 levels(as.factor(sas.22$household.typology))
 
 sas.22 <- sas.22 %>% mutate(household.typology = case_when(
   (pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_part == 1) | 
     (pi_nbhoushold_22 == 2 & pi_nbhoushold_22_spous == 1 ) ~ "(Ehe)Paarhaushalt"  , 
   pi_nbhoushold_22 == 1 ~ "Einzelhaushalt", 
   (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 1 &  pi_nbhoushold_22_part == 1) | 
     (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 1 &  pi_nbhoushold_22_spous == 1) ~ "3-er Familienhaushalt",
   pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_child == 1 ~ "Alleinerziehende", 
     pi_nbhoushold_22 >= 4 |
     (pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_hmate  == 1)  |
     (pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_part  == 0 &  pi_nbhoushold_22_part  == 0 &  pi_nbhoushold_22_child == 0)  |
     (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 0)  |
     (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 1)~ "Andere"
 ))  %>% mutate(household.typology=factor(household.typology, levels=c("Einzelhaushalt", 
                                                                   "Alleinerziehende",
                                                                      "(Ehe)Paarhaushalt", 
                                                                   "3-er Familienhaushalt",
                                                                      "Andere"), 
                                         ordered=TRUE))

sas.22 %>% count(household.typology)

#gemeindegrösse

sas.22 %>% count(env_municipalitysizecat_22)
levels(sas.22$env_municipalitysizecat_22)
sas.22 <- sas.22 %>% mutate(villagesize = case_when(
  env_municipalitysizecat_22 == "<1000"  |
    env_municipalitysizecat_22 == "1000-1999" ~ "<2000",
    env_municipalitysizecat_22 =="2000-4999" ~ "2000-4999", 
  env_municipalitysizecat_22 == "5000-9999"~ "5000-9999", 
    env_municipalitysizecat_22 == "10'000-19'999" ~ "10000-19999",
    env_municipalitysizecat_22 =="20'000-49'999" ~ "20000-49999",
  env_municipalitysizecat_22 == "50'000-99'999"   |
    env_municipalitysizecat_22 ==">100'000"  ~ ">50000"
)) %>% mutate(villagesize = factor(villagesize, ordered=TRUE, levels=c("<2000",
                                                                       "2000-4999",
                                                                       "5000-9999",
                                                                       "10000-19999",
                                                                       "20000-49999",
                                                                       ">50000")))
sas.22 %>%  count(villagesize)

# DEGURBA umcodieren

sas.22 <- sas.22 %>% mutate(degurba.rcd = case_when(
  env_degurba_22 == 1 ~ "Städtische Gemeinde", 
  env_degurba_22 == 2 ~ "Peri-urbane Gemeinde",
  env_degurba_22 == 3 ~ "Ländliche Gemeinde" 
)) 
sas.22 %>% count(degurba.rcd)

# Vermögen 

sas.22$fi_homeowner_22
sas.22 %>% count(fi_homeowner_22) 
sas.22 %>% count(fi_homeowner_22) %>% summarise(n/sum(n))

sas.22 <- sas.22 %>% mutate(homeowner.rcd = case_when(
  fi_homeowner_22 == 1 ~ "Eigenheimbesitz", 
  fi_homeowner_22!=1 ~ "Kein Eigenheimbesitz"
))

boxplot(log(sas.22$fi_valrealest_22))
filter <- which(sas.22$fi_valrealest_22<100000)
filter <- which(sas.22$fi_valrealest_22<5000)

length(filter) # Hypothese: diejenigen mit wert <100'000 haben das Haus wohl Geschenkt bekommen. Auschluss aber nicht plausibel und verändert Resultat nicht. 
length(which(!is.na(sas.22$fi_valrealest_22)))

# merge numerical direct-response variable and categorical in one "merged value variable" for realestate
sas.22$realest.merged.val <- sas.22$fi_valrealest_22
length(which(is.na(sas.22$realest.merged.val)))
length(which(!is.na(sas.22$realest.merged.val)))


sas.22$fi_valrestestcat_22
length(which(!is.na(sas.22$fi_valrestestcat_22)))

sas.22$virtual.realest.amount <- ifelse(sas.22$fi_valrestestcat_22==0,median(0:250000),
                                        ifelse(sas.22$fi_valrestestcat_22==1,median(250000:500000), 
                                               ifelse(sas.22$fi_valrestestcat_22==2,median(500000:1000000),
                                               ifelse(sas.22$fi_valrestestcat_22==3,median(1100000),NA))))
length(which(!is.na(sas.22$fi_valrestestcat_22))) == length(which(!is.na(sas.22$virtual.realest.amount)))
sas.22 %>% count(virtual.realest.amount)

filter <- which(is.na(sas.22$fi_valrealest_22))
length(filter)
sas.22$realest.merged.val[filter] <- sas.22$virtual.realest.amount[filter]
length(which(is.na(sas.22$realest.merged.val)))

filter <- which(is.na(sas.22$realest.merged.val))
length(filter)
sas.22$realest.merged.val[filter] <- 0 # this hypothesis is plausible: those who are not homeowners and have no value reported for real estate have value 0
hist(log(sas.22$realest.merged.val))
length(which(!is.na(sas.22$realest.merged.val)))

sas.22 %>% select(Respondent_ID,homeowner.rcd,fi_valrealest_22,virtual.realest.amount,realest.merged.val)

length(which(is.na(sas.22$realest.merged.val)))
length(which(sas.22$realest.merged.val==0))


#Assets

sas.22$fi_assetsamount_22
length(which(is.na(sas.22$fi_assetsamount_22)))
length(which(!is.na(sas.22$fi_assetsamount_22)))


sas.22$assetamount.merged <- sas.22$fi_assetsamount_22

sas.22$fi_assetscat_22
length(which(!is.na(sas.22$fi_assetscat_22)))
sas.22$virtual.asset.amount <- ifelse(sas.22$fi_assetscat_22==0,median(0:4000),
                                      ifelse(sas.22$fi_assetscat_22==1,median(4000:10000), 
                                             ifelse(sas.22$fi_assetscat_22==2,median(10000:37500), 
                                                    ifelse(sas.22$fi_assetscat_22==3,median(37500:60000),
                                                           ifelse(sas.22$fi_assetscat_22==4,median(60000:100000),
                                                                  ifelse(sas.22$fi_assetscat_22==5,median(100000:112500),
                                                                         ifelse(sas.22$fi_assetscat_22==6,median(112500:300000),
                                                                                ifelse(sas.22$fi_assetscat_22==7,median(300000:500000),
                                                                                       ifelse(sas.22$fi_assetscat_22==8,median(500000:1000000),
                                                                                              ifelse(sas.22$fi_assetscat_22==9,median(1000000:1500000),
                                                                                                     NA))))))))))
                                       
length(which(!is.na(sas.22$fi_assetscat_22))) == length(which(!is.na(sas.22$virtual.asset.amount)))

filter <- which(is.na(sas.22$fi_assetsamount_22))
length(filter)
sas.22$assetamount.merged[filter] <- sas.22$virtual.asset.amount[filter]
length(which(!is.na(sas.22$assetamount.merged)))


  
  
  # morgage
sas.22$fi_morgage_22
sas.22 %>% count(fi_morgage_22)

sas.22$fi_summorgage_22
length(which(!is.na(sas.22$fi_summorgage_22)))

#again, merge the one-shot and categorical (virtual) into one
sas.22$morgage.merged.amount <- sas.22$fi_summorgage_22

sas.22$virtual.morgage.amount <- ifelse(sas.22$fi_summorgagecat_22==0,median(0:250000),
                                        ifelse(sas.22$fi_summorgagecat_22==1,median(250000:500000), 
                                               ifelse(sas.22$fi_summorgagecat_22==2,median(500000:1000000),
                                                      ifelse(sas.22$fi_summorgagecat_22==3,median(1000001),NA))))
length(which(!is.na(sas.22$fi_summorgagecat_22))) == length(which(!is.na(sas.22$virtual.morgage.amount)))

filter <- which(sas.22$fi_morgage_22==1 & is.na(sas.22$fi_summorgage_22))
length(filter)
sas.22$morgage.merged.amount[filter] <- sas.22$virtual.morgage.amount[filter]
length(which(!is.na(sas.22$morgage.merged.amount)))

length(which(is.na(sas.22$morgage.merged.amount)))
filter <- which(sas.22$realest.merged.val==0 & is.na(sas.22$morgage.merged.amount))
length(filter)
sas.22$morgage.merged.amount[filter] <- 0
# create merged wealth variable(s)


sas.22$gross.wealth.sum <- sas.22$assetamount.merged + sas.22$realest.merged.val
sas.22$net.wealth.sum <- sas.22$gross.wealth.sum - sas.22$morgage.merged.amount
filter <- which(sas.22$net.wealth.sum<0)
length(filter)
sas.22$net.wealth.sum[filter] <- NA 
length(which(is.na(sas.22$net.wealth.sum)))
sas.22 %>% count(sas.22$net.wealth.sum)

# create wealth indicators mirroring those in BFS reports
sas.22$less.10000.assets <- NA
filter <- which(sas.22$assetamount.merged<10000)
sas.22$less.10000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=10000)
sas.22$less.10000.assets[filter] <- 0
svymean(~less.10000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight

sas.22$less.20000.assets <- NA
filter <- which(sas.22$assetamount.merged<20000)
sas.22$less.20000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=20000)
sas.22$less.20000.assets[filter] <- 0
svymean(~less.20000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight


sas.22$poor.less.20000.assets <- NA
filter <- which(sas.22$assetamount.merged<20000 & sas.22$poverty.bn==1)
sas.22$poor.less.20000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=20000)
sas.22$poor.less.20000.assets[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$poor.less.20000.assets[filter] <- 0
sas.22 %>% count(poor.less.20000.assets) 
svymean(~poor.less.20000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight



sas.22$less.30000.assets <- NA
filter <- which(sas.22$assetamount.merged<30000)
sas.22$less.30000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$less.30000.assets[filter] <- 0
svymean(~less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight


sas.22$poor.less.30000.assets.but.house <- NA
filter <- which(sas.22$assetamount.merged<30000 & sas.22$poverty.bn==1 & sas.22$fi_homeowner_22==1)
sas.22$poor.less.30000.assets.but.house[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$poor.less.30000.assets.but.house[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$poor.less.30000.assets.but.house[filter] <- 0
filter <- which(sas.22$fi_homeowner_22==0)
sas.22$poor.less.30000.assets.but.house[filter] <- 0
svymean(~poor.less.30000.assets.but.house, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~poor.less.30000.assets.but.house, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight



sas.22$less.10000.assets <- NA
filter <- which(sas.22$assetamount.merged<10000)
sas.22$less.10000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=10000)
sas.22$less.10000.assets[filter] <- 0
sas.22 %>% count(less.10000.assets)



sas.22$over.100000.assets <- NA
filter <- which(sas.22$assetamount.merged<=100000)
sas.22$over.100000.assets[filter] <- 0
filter <- which(sas.22$assetamount.merged>100000)
sas.22$over.100000.assets[filter] <- 1
sas.22 %>% count(over.100000.assets)


sas.22$over.10e6.assets <- NA
filter <- which(sas.22$assetamount.merged<=1000000)
sas.22$over.10e6.assets[filter] <- 0
filter <- which(sas.22$assetamount.merged>1000000)
sas.22$over.10e6.assets[filter] <- 1
sas.22 %>% count(over.10e6.assets)
svytable(~over.10e6.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight

length(which(is.na(sas.22$net.wealth.sum)))
sas.22$over.10e6.netwealth <- NA
filter <- which(sas.22$net.wealth.sum<=1000000)
sas.22$over.10e6.netwealth[filter] <- 0
filter <- which(sas.22$assetamount.merged>1000000)
sas.22$over.10e6.netwealth[filter] <- 1
sas.22 %>% count(over.10e6.assets)
svytable(~over.10e6.netwealth, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight



sas.22$over.100.assets.and.houseowner <- NA
filter <- which(sas.22$assetamount.merged>100000 & sas.22$fi_homeowner_22==1)
sas.22$over.100.assets.and.houseowner[filter] <- 1
filter <- which(sas.22$assetamount.merged<=100000)
sas.22$over.100.assets.and.houseowner[filter] <- 0
filter <- which(sas.22$fi_homeowner_22==0)
sas.22$over.100.assets.and.houseowner[filter] <- 0
sas.22 %>% count(over.100.assets.and.houseowner) 
svymean(~over.100.assets.and.houseowner, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~over.100.assets.and.houseowner, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight


length(which(is.na(sas.22$realest.merged.val)))
length(which(sas.22$realest.merged.val==0))


sas.22$poor.nothomeowner.less.30000.assets <- NA
filter <- which(sas.22$assetamount.merged<30000 & sas.22$poverty.bn==1 & sas.22$realest.merged.val==0 )
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 0
filter <- which(sas.22$realest.merged.val>1)
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 0
sas.22 %>% count(poor.nothomeowner.less.30000.assets) 
svymean(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight





sas.22$multi.poor <- NA
filter <- which(
  sas.22$assetamount.merged<30000 & 
    sas.22$poverty.bn==1 & 
    sas.22$fi_homeowner_22==0 &
    sas.22$inc.source.type =="Nur 1. Säule" )
sas.22$multi.poor[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$multi.poor[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$multi.poor[filter] <- 0
filter <- which(sas.22$fi_homeowner_22==1)
sas.22$multi.poor[filter] <- 0
filter <- which(sas.22$inc.source.type !="Nur 1. Säule")
sas.22$multi.poor[filter] <- 0
sas.22 %>% count(multi.poor) 
svymean(~multi.poor, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight




# multidimensional indicators wealth, specific to household constellation, marital status and EL-specific asset-levels

levels(as.factor(sas.22$civstat.rcd))

sas.22 <- sas.22 %>% mutate(less30kassets.livingalone.unmarried = case_when(
  civstat.rcd != "Verheiratet / Regist.Part." & pi_nbhoushold_22==1 & sas.22$assetamount.merged<30000 ~ 1, 
  civstat.rcd != "Verheiratet / Regist.Part." & pi_nbhoushold_22==1 & sas.22$assetamount.merged>=30000 ~ 0 
)) 

sas.22 %>% count(less30kassets.livingalone.unmarried) 

sas.22 <- sas.22 %>% mutate(less30kassets.poor.livingalone.unmarried = case_when(
  less30kassets.livingalone.unmarried == 1 & poverty.bn == 1 ~ 1, 
  (less30kassets.livingalone.unmarried == 1 & poverty.bn == 0) | 
    (less30kassets.livingalone.unmarried == 0 & poverty.bn == 1) | 
    (less30kassets.livingalone.unmarried == 0 & poverty.bn == 0) ~ 0))

sas.22 %>% count(less30kassets.poor.livingalone.unmarried)

sas.22 <- sas.22 %>% mutate(less50kassets.couplehhold.married = case_when(
  civstat.rcd == "Verheiratet / Regist.Part." & pi_nbhoushold_22==2 & sas.22$assetamount.merged<50000 ~ 1, 
  civstat.rcd == "Verheiratet / Regist.Part." & pi_nbhoushold_22==2 & sas.22$assetamount.merged>=50000 ~ 0 
))
sas.22 %>% count(less50kassets.couplehhold.married)


sas.22 <- sas.22 %>% mutate(less50kassets.poor.couplehhold.married = case_when(
  less50kassets.couplehhold.married == 1 & poverty.bn == 1 ~ 1, 
  (less50kassets.couplehhold.married == 1 & poverty.bn == 0) | 
    (less50kassets.couplehhold.married == 0 & poverty.bn) == 1| 
    (less50kassets.couplehhold.married == 0 & poverty.bn == 0) ~ 0))

sas.22 %>% count(less50kassets.poor.couplehhold.married) 



# other deprivation variables 

sas.22$mu_affordholiday_22

sas.22$difficulty.make.ends.meet

sas.22$


# explanatory variables 

sas.22$srh.bn <- ifelse(sas.22$ph_srh_22==0,1,0)
sas.22 %>% count(sas.22$srh.bn)

sas.22 <- sas.22 %>% mutate(srh.3cat = case_when(
  ph_srh_22 == 0 ~ "Schlechte Gesundheit", 
  ph_srh_22 == 1 ~ "Mittelmässige Gesundheit", 
  ph_srh_22 >1 ~ "Gute bis ausgezeichnete Gesundheit"
))
sas.22 %>% count(srh.3cat)

sas.22 %>% count(mh_3item.loneliness.score_22)
sas.22$lonely.3cat <- ifelse(sas.22$mh_3item.loneliness.score_22>6,"(Sehr) Einsam",
                           ifelse(sas.22$mh_3item.loneliness.score_22<=6 & sas.22$mh_3item.loneliness.score_22>3,"Etwas einsam",
                                  ifelse(is.na(sas.22$mh_3item.loneliness.score_22),NA,"Nicht einsam"))) 
sas.22$lonely.3cat <- factor(sas.22$lonely.3cat, levels=c("(Sehr) Einsam","Etwas einsam", "Nicht einsam"))
sas.22 %>% count(sas.22$lonely.3cat) 

sas.22$feeling.lonely <- NA
filter <- which(sas.22$mh_3item.loneliness.score_22>6)
sas.22$feeling.lonely[filter] <- 1
filter <- which(sas.22$mh_3item.loneliness.score_22<=6)
sas.22$feeling.lonely[filter] <- 0
sas.22 %>% count(feeling.lonely)

# genlifsat

levels(as.factor(sas.22$ls_genlifesatdiener_cat_22))
sas.22$low.life.sat.bn <- NA
filter <- which(sas.22$ls_genlifesatdiener_cat_22<3)
sas.22$low.life.sat.bn[filter] <- 1
filter <- which(sas.22$ls_genlifesatdiener_cat_22>=3)
sas.22$low.life.sat.bn[filter] <- 0
sas.22 %>% count(low.life.sat.bn)

############################################################################################
############################################################################################
#### SAMPLE description
############################################################################################
############################################################################################


# set survey design for population estimates 
dsurvey.pop <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)


svytable(~edu.rcd, dsurvey.pop) # estimate on number of people affected by income poverty 
svytable(~rel.poverty.bn, dsurvey.pop) # estimate on number of people affected by rel poverty 

dsurvey <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22)
^è¨














~º ft
# prop
# prop <- as_tibble(prop)
# prop
# prop$cat <- c("men", 
#               "women")
# names(prop) <- c("prop", "se","cat")
# prop
# sample.char <- prop
# 
# prop <- svymean(~as.factor(age.group.3cat), dsurvey,na.rm=TRUE)
# prop
# prop <- as_tibble(prop)
# prop
# prop$cat <- c("65-74", 
#               "75+")
# names(prop) <- c("prop", "se","cat")
# prop
# sample.char <- rbind(sample.char, prop)
# sample.char
# 
# prop <- svymean(~as.factor(edu.rcd), dsurvey,na.rm=TRUE)
# prop
# prop <- as_tibble(prop)
# prop
# prop$cat <- c("Sek II", 
#               "Tert", 
#               "oblig")
# names(prop) <- c("prop", "se","cat")
# prop
# sample.char <- rbind(sample.char, prop)
# sample.char
# 
# prop <- svymean(~as.factor(pi_swissnational_22), dsurvey,na.rm=TRUE)
# prop
# prop <- as_tibble(prop)
# prop
# prop$cat <- c("ausl", 
#               "CH")
# names(prop) <- c("prop", "se","cat")
# prop
# sample.char <- rbind(sample.char, prop)
# sample.char
# 
# write.csv(sample.char, file="table_sample-char_2022-08-27.csv")



############################################################################################
############################################################################################
#### ANALYSES
############################################################################################
############################################################################################

# set survey design for population estimates 
dsurvey.pop <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)
svytable(~poverty.bn, dsurvey.pop) # estimate on number of people affected by income poverty 
svytable(~rel.poverty.bn, dsurvey.pop) # estimate on number of people affected by rel poverty 


# set survey design for survey package (and subsequent weighted tables)
dsurvey <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22)


# Incomes ----------------------------------------------------------------- 



# estimate mean amount for incomes 
svyby(~equiv.hh.income, by=~pi_sex_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)

svyby(~income.amount.merged, by=~household.typology, dsurvey, svymean, na.rm=TRUE, ci=TRUE)

# Overview Poverty Measures -----------------------------------------------

# absolute poverty 
raw.table<-svymean(~poverty.bn, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","category"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- result.table1

# relative poverty 
raw.table<-svymean(~rel.poverty.bn, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsgefährdet"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- rbind(merged.table, result.table1 )
merged.table 


# EL Bezug
raw.table<-svymean(~fi_incsourc_22_el, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "EL-Beziehende (zuhause lebend)"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- rbind(merged.table, result.table1 )
merged.table 


merged.table$category <- factor(merged.table$category, ordered=TRUE)

p <- 
  ggplot(merged.table, aes(x = forcats::fct_rev(category), y=est)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(est, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 4)+
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(title = "Zentrale Armutsindikatoren für die Bevölkerung ab 65 in der Schweiz", 
       caption = "Quelle: Schweizer Alterssurvey 2022 \n Absolute Armutsgrenze (BFS, 2020): 2279 CHF \n Armutsgefährdungsgrenze (BFS, 2020): 2506 CHF") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p)


ggsave(
  plot = print(p),
  filename="graph1_armutsindikatoren.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 200,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph1_armutsindikatoren.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 200,
  units = "mm",
  dpi = 300
)


rm(merged.table,merged.table2,raw.table,raw.table1)

length(which(complete.cases(
  subset(sas.22, select=c(
    poverty.bn, 
    fi_incsourc_22_el, 
    rel.poverty.bn)))))


# sociodemographic factors ------------------------------------------------

# sex 
raw.table1<-svyby(~poverty.bn,by=~pi_sex_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Männer","Frauen")
raw.table1$category <- name
raw.table1
merged.table2 <- raw.table1

#age group
raw.table1<-svyby(~poverty.bn,by=~age.group.3cat, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


#civil status
raw.table1<-svyby(~poverty.bn,by=~civstat.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2

# migration 
raw.table1<-svyby(~poverty.bn,by=~pi_swissnational_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Ausländer:Innen","Schweizer:Innen")
raw.table1$category <- name
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2

# bildung 

raw.table1<-svyby(~poverty.bn,by=~edu.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


#grösse 
raw.table1<-svyby(~poverty.bn,by=~degurba.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2




merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2$se <-NULL

names(merged.table2)
is.character(merged.table2$category)
merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
is.double(merged.table2$sample.mean.pov)
is.numeric(merged.table2$lwr.ci)
is.numeric(merged.table2$upr.ci)

merged.table2




# absolute poverty 
raw.table<-svymean(~poverty.bn, dsurvey,na.rm=TRUE) # weighted tables using survey packages
pov.mean.line <- raw.table[1]


levels(as.factor(merged.table2$category))

levels=c(         "Frauen", 
         "Männer", 
         "65-74", 
         "75+", 
         "Verheiratet / Regist.Part.", 
         "Ledig", 
         "Geschieden", 
         "Verwitwet",
         "Schweizer:Innen", 
         "Ausländer:Innen", 
         "low", 
         "avg", 
         "high", 
         "Städtische Gemeinde", 
         "Peri-urbane Gemeinde", 
         "Ländliche Gemeinde")

labels=c(
         "Frauen", 
         "Männer", 
         "Altersklasse 65-74", 
         "Altersklasse 75+", 
         "Verheiratet / Regist.Part.", 
         "Ledig", 
         "Geschieden", 
         "Verwitwet",
         "Schweizer:Innen", 
         "Ausländer:Innen", 
         "Obligatorische Schule", 
         "Sekundarstufe II", 
         "Tertiärstufe", 
         "Städtische Gemeinde", 
         "Peri-urbane Gemeinde", 
         "Ländliche Gemeinde")

length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category, 
                                 levels=levels, 
                                 labels=labels, 
                                 ordered=TRUE)
merged.table2


p2 <- 
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) + 
  coord_flip() + 
  geom_hline(yintercept = pov.mean.line, color='red', lwd=0.5) +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(title = "Armutsbetroffenheit bei spezifischen Bevölkerungsgruppen", 
       subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p2)

ggsave(
  plot = print(p2),
  filename="graph2_spezifischearmut.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 450,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph2_spezifischearmut.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 450,
  units = "mm",
  dpi = 300
)


length(which(complete.cases(
  subset(sas.22, select=c(
    poverty.bn, 
    civstat.rcd, 
    degurba.rcd, 
    edu.rcd, 
    pi_swissnational_22)))))




# sociodemographic factors and nichtkompensierbare armut ------------------


# sex 
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~pi_sex_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Männer","Frauen")
raw.table1$category <- name
raw.table1
merged.table2 <- raw.table1

#age group
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~age.group.3cat, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


#civil status
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~civstat.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2

# migration 
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~pi_swissnational_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Ausländer:Innen","Schweizer:Innen")
raw.table1$category <- name
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2

# bildung 

raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~edu.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


#grösse 
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~degurba.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1
merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2




merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2$se <-NULL

names(merged.table2)
is.character(merged.table2$category)
merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
is.double(merged.table2$sample.mean.pov)
is.numeric(merged.table2$lwr.ci)
is.numeric(merged.table2$upr.ci)

merged.table2




# multi poverty 
raw.table<-svymean(~poor.nothomeowner.less.30000.assets, dsurvey,na.rm=TRUE) # weighted tables using survey packages
pov.mean.line <- raw.table[1]


levels(as.factor(merged.table2$category))

levels=c(         "Frauen", 
                  "Männer", 
                  "65-74", 
                  "75+", 
                  "Verheiratet / Regist.Part.", 
                  "Ledig", 
                  "Geschieden", 
                  "Verwitwet",
                  "Schweizer:Innen", 
                  "Ausländer:Innen", 
                  "low", 
                  "avg", 
                  "high", 
                  "Städtische Gemeinde", 
                  "Peri-urbane Gemeinde", 
                  "Ländliche Gemeinde")

labels=c(
  "Frauen", 
  "Männer", 
  "Altersklasse 65-74", 
  "Altersklasse 75+", 
  "Verheiratet / Regist.Part.", 
  "Ledig", 
  "Geschieden", 
  "Verwitwet",
  "Schweizer:Innen", 
  "Ausländer:Innen", 
  "Obligatorische Schule", 
  "Sekundarstufe II", 
  "Tertiärstufe", 
  "Städtische Gemeinde", 
  "Peri-urbane Gemeinde", 
  "Ländliche Gemeinde")

length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category, 
                                 levels=levels, 
                                 labels=labels, 
                                 ordered=TRUE)
merged.table2


p2 <- 
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) + 
  coord_flip() + 
  geom_hline(yintercept = pov.mean.line, color='red', lwd=0.5) +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(title = "Nicht kompensierbare Armutsbetroffenheit bei spezifischen Bevölkerungsgruppen", 
       subtitle = "Nicht kompensierbar armutsbetroffen = Einkommen unterhalb Armutsgrenze,\n Vermögen >30'000 CHF und kein Wohneigentum",
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p2)

ggsave(
  plot = print(p2),
  filename="graph2.2_spezifischearmut.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 450,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph2.2_spezifischearmut.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 450,
  units = "mm",
  dpi = 300
)


length(which(complete.cases(
  subset(sas.22, select=c(
    poverty.bn, 
    civstat.rcd, 
    degurba.rcd, 
    edu.rcd, 
    pi_swissnational_22)))))




# Map Absolute Armut ------------------------------------------------------



t<- prop.table(wtd.table(x = sas.22$poverty.bn,
                         y=sas.22$canton_22, 
                         weights = sas.22$cross.design.weights.sample_22),2)


t <- as.data.frame(t)
t$Var2 <- as.character(t$Var2)
t
t[3,2]<-"AR"
t[4,2]<-"AI"
t
t[3,1] <- 1
t
t[3,3] <-  t[4,3] 
t

filter <- which(t$Var1==0)
t <- t[-filter,]
t
t
t
names(t) <- c("Var1", "Canton", "incidence")

t$incidence <- t$incidence*100
t

swiss_lakes <- st_read("g2s22.shp")
swiss_cantons <- st_read("G1K22.shp")

ggplot()+
  geom_sf(data = swiss_cantons)

levels(as.factor(swiss_cantons$KTNAME))

swiss_cantons <- swiss_cantons %>% select(KTNAME) %>% 
  mutate(KTNAME = case_when(
    KTNAME=="Aargau" ~ "AG", 
    KTNAME=="Basel-Landschaft" ~ "BL",
    KTNAME=="Fribourg / Freiburg" ~ "FR",
    KTNAME=="Graubünden / Grigioni / Grischun" ~ "GR",
    KTNAME=="Neuchâtel" ~ "NE",
    KTNAME=="Schaffhausen" ~ "SH",
    KTNAME=="St. Gallen" ~ "SG",
    KTNAME=="Uri" ~ "UR",
    KTNAME=="Zug" ~ "ZG",
    KTNAME=="Appenzell Ausserrhoden" ~ "AR",
    KTNAME=="Basel-Stadt" ~ "BS",
    KTNAME=="Genève" ~ "GE",
    KTNAME=="Jura" ~ "JU",
    KTNAME=="Nidwalden" ~ "NW",
    KTNAME=="Schwyz" ~ "SZ",
    KTNAME=="Thurgau" ~ "TG",
    KTNAME=="Valais / Wallis" ~ "VS",
    KTNAME=="Zürich" ~ "ZH",
    KTNAME=="Appenzell Innerrhoden" ~ "AI",
    KTNAME=="Bern / Berne" ~ "BE",
    KTNAME=="Zürich" ~ "ZH",
    KTNAME=="Glarus" ~ "GL",
    KTNAME=="Luzern" ~ "LU",
    KTNAME=="Obwalden" ~ "OW",
    KTNAME=="Solothurn" ~ "SO",
    KTNAME=="Ticino" ~ "TI",
    KTNAME=="Vaud" ~ "VD"
  ))
levels(as.factor(swiss_cantons$KTNAME))
length(swiss_cantons$KTNAME)==length(t$Canton)
zz <- match(as.character(swiss_cantons$KTNAME),as.character(t$Canton))
which(is.na(zz))
t$Canton
head(swiss_cantons)
swiss_cantons <- swiss_cantons %>% 
  left_join(t, c("KTNAME" = "Canton"))
swiss_cantons %>%  select(KTNAME,incidence)

ggplot()+
  geom_sf(data = swiss_cantons, fill = NA) +
  geom_sf(data = swiss_lakes,  fill = "#d1eeea", color = "#d1eeea") +
  theme_void()

table(t$incidence)

swiss_cantons <- swiss_cantons %>% 
  mutate(incidence_cat = case_when(
    incidence <=10 ~ "<10%",
    incidence >10 & incidence<=15 ~ "10-15%",
    incidence >15 & incidence<=20  ~ "15-20%",
    incidence >20  ~ ">20%"
  )) %>% 
  mutate(incidence_cat = factor(incidence_cat, levels = c("<10%", "10-15%","15-20%",">20%")))

swiss_cantons %>%  select(KTNAME, incidence_cat)

ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl") +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") 

ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl",
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(70 / 5, units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1,
                                          byrow = T,
                                          label.position = "bottom")) +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") 



p2 <- ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl",
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(70 / 5, units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1,
                                          byrow = T,
                                          label.position = "bottom")) +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  ggrepel::geom_label_repel(
    data = swiss_cantons,
    aes(label = paste0(KTNAME,":",round(incidence, digits = 1)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  labs(title = "<b style='color:#541f3f'> Absolute Armutsquote Bevölkerung 65+ </b>",
       subtitle = "<span style='font-size:10pt'>Basierend auf den Äquivalenzeinkommen der Haushalte </span>",
       caption = "Source: Schweizer Alterssurvey | 2022") +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()) 
print(p2)

ggsave(
  plot = print(p2),
  filename="graph3_CHkarte.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 350,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph3_CHkarte.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 350,
  units = "mm",
  dpi = 300
)



# Map Multiple Armut ------------------------------------------------------


raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~canton_22, dsurvey, svymean, na.rm=TRUE, ci=FALSE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1



raw.table1$canton_22 <- as.character(raw.table1$canton_22)
filter <- which(raw.table1$canton_22=="ARAI")
raw.table1$canton_22[filter] <- "AR"

temp <- raw.table1[filter,]
temp 
raw.table1 <- rbind(raw.table1,temp)
raw.table1
raw.table1[26,1] <- "AI"

raw.table1 <- raw.table1 %>% select(-se)
raw.table1
names(raw.table1) <- c("Canton", "incidence")

raw.table1$incidence <- raw.table1$incidence*100
raw.table1$incidence

swiss_lakes <- st_read("g2s22.shp")
swiss_cantons <- st_read("G1K22.shp")

ggplot()+
  geom_sf(data = swiss_cantons)

levels(as.factor(swiss_cantons$KTNAME))

swiss_cantons <- swiss_cantons %>% select(KTNAME) %>% 
  mutate(KTNAME = case_when(
    KTNAME=="Aargau" ~ "AG", 
    KTNAME=="Basel-Landschaft" ~ "BL",
    KTNAME=="Fribourg / Freiburg" ~ "FR",
    KTNAME=="Graubünden / Grigioni / Grischun" ~ "GR",
    KTNAME=="Neuchâtel" ~ "NE",
    KTNAME=="Schaffhausen" ~ "SH",
    KTNAME=="St. Gallen" ~ "SG",
    KTNAME=="Uri" ~ "UR",
    KTNAME=="Zug" ~ "ZG",
    KTNAME=="Appenzell Ausserrhoden" ~ "AR",
    KTNAME=="Basel-Stadt" ~ "BS",
    KTNAME=="Genève" ~ "GE",
    KTNAME=="Jura" ~ "JU",
    KTNAME=="Nidwalden" ~ "NW",
    KTNAME=="Schwyz" ~ "SZ",
    KTNAME=="Thurgau" ~ "TG",
    KTNAME=="Valais / Wallis" ~ "VS",
    KTNAME=="Zürich" ~ "ZH",
    KTNAME=="Appenzell Innerrhoden" ~ "AI",
    KTNAME=="Bern / Berne" ~ "BE",
    KTNAME=="Zürich" ~ "ZH",
    KTNAME=="Glarus" ~ "GL",
    KTNAME=="Luzern" ~ "LU",
    KTNAME=="Obwalden" ~ "OW",
    KTNAME=="Solothurn" ~ "SO",
    KTNAME=="Ticino" ~ "TI",
    KTNAME=="Vaud" ~ "VD"
  ))
levels(as.factor(swiss_cantons$KTNAME))
length(swiss_cantons$KTNAME)==length(t$Canton)
zz <- match(as.character(swiss_cantons$KTNAME),as.character(raw.table1$Canton))
which(is.na(zz))
head(swiss_cantons)
swiss_cantons <- swiss_cantons %>% 
  left_join(raw.table1, c("KTNAME" = "Canton"))
swiss_cantons %>%  select(KTNAME,incidence)

ggplot()+
  geom_sf(data = swiss_cantons, fill = NA) +
  geom_sf(data = swiss_lakes,  fill = "#d1eeea", color = "#d1eeea") +
  theme_void()

table(raw.table1$incidence)

swiss_cantons <- swiss_cantons %>% 
  mutate(incidence_cat = case_when(
    incidence <=1 ~ "<1%",
    incidence >1 & incidence<=2.5 ~ "1-2.5%",
    incidence >2.5 & incidence<=5  ~ "2.5-5%",
    incidence >5  ~ ">5%"
  )) %>% 
  mutate(incidence_cat = factor(incidence_cat, levels = c("<1%","1-2.5%","2.5-5%",">5%")))

swiss_cantons %>% count(incidence_cat)
swiss_cantons %>%  select(KTNAME, incidence_cat)

ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl") +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") 

ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl",
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(70 / 5, units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1,
                                          byrow = T,
                                          label.position = "bottom")) +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") 



p2 <- ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl",
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(70 / 5, units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1,
                                          byrow = T,
                                          label.position = "bottom")) +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  ggrepel::geom_label_repel(
    data = swiss_cantons,
    aes(label = paste0(KTNAME,":",round(incidence, digits = 1)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  labs(title = "<b style='color:#541f3f'> Anteil nicht kompensierbar armutsbetroffene Bevölkerung ab 65</b>",
       subtitle = "<span style='font-size:10pt'>Nicht kompensierbar armutsbetroffen = Einkommen unterhalb Armutsgrenze, Vermögen >30'000 CHF und kein Wohneigentum   </span>",
       caption = "Source: Schweizer Alterssurvey | 2022") +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()) 
print(p2)

ggsave(
  plot = print(p2),
  filename="graph3.2_CHkarte.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 350,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph3.2_CHkarte.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 350,
  units = "mm",
  dpi = 300
)



# regional differences  ---------------------------------------------------

# cantonal differences 
rm(merged.table,merged.table2,raw.table,raw.table1)


raw.table1<-svyby(~poverty.bn,by=~canton_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1

merged.table2 <- raw.table1
merged.table2


merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2

order <- order(merged.table2$sample.mean.pov, decreasing = FALSE)
merged.table2 <- merged.table2[order,]

line <- median(merged.table2$sample.mean.pov)
line 
p <- ggplot(merged.table2, aes( y = sample.mean.pov, x = reorder(category, +sample.mean.pov))) + geom_col(fill="#C8777B",alpha=0.5 ) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = line, color="#541F3F", lwd=0.7, alpha=0.4) +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5)+ 
ylab("Absolute Armutsquote") + xlab("Kantone") + labs(title = "Absolute Armutsquote", 
  subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze")+
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-4, size = 4)
print(p)




# regional differences  multi poor ---------------------------------------------------

# cantonal differences 
rm(merged.table,merged.table2,raw.table,raw.table1)


raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~canton_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1

merged.table2 <- raw.table1
merged.table2


merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2

order <- order(merged.table2$sample.mean.pov, decreasing = FALSE)
merged.table2 <- merged.table2[order,]

line <- median(merged.table2$sample.mean.pov)
line 
p <- ggplot(merged.table2, aes( y = sample.mean.pov, x = reorder(category, +sample.mean.pov))) + geom_col(fill="#C8777B",alpha=0.5 ) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = line, color="#541F3F", lwd=0.7, alpha=0.4) +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5)+ 
  ylab("Quote Auswegslos Armutsbetroffen") + xlab("Kantone") + labs(title = "Quote Auswegslos Armutsbetroffen", 
                                                        subtitle = "")+
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-4, size = 4)
print(p)


# Weitere Armutsindikatoren  ----------------------------------------------

rm(merged.table,merged.table2,raw.table,raw.table1)


# absolute poverty 
raw.table<-svymean(~poverty.bn, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","category"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- result.table1

# sudden expenses
raw.table<-svymean(~cant.face.sudden.exp, dsurvey, na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","category"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- rbind(merged.table, result.table1)
merged.table




# making ends meet

raw.table<-svymean(~difficulty.make.ends.meet, dsurvey, na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Hat Schwierigkeiten über die Runden zu kommen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","category"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1


merged.table <- rbind(merged.table, result.table1)
merged.table 

merged.table$category <- factor(merged.table$category, ordered=TRUE)

# making ends meet

raw.table<-svymean(~poor.deprived.subjectivepoor, dsurvey, na.rm=TRUE) # weighted tables using survey packages
raw.table<-svytable(~poor.deprived.subjectivepoor, dsurvey.pop) # weighted tables using survey packages

raw.table
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen, kann keine unvorhergesehenen Ausgaben tätigen und hat Schwierigkeiten über die Runden zu kommen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","category"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1


merged.table <- rbind(merged.table, result.table1)
merged.table 

levels(as.factor(merged.table$category))
merged.table$category <- factor(merged.table$category, levels=c("Armutsbetroffen", 
                                                                "Kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten"  ,
                                                                "Hat Schwierigkeiten über die Runden zu kommen"  ,
                                                                "Armutsbetroffen, kann keine unvorhergesehenen Ausgaben tätigen und hat Schwierigkeiten über die Runden zu kommen"), 
                                ordered=TRUE)
                                                                
    

# create table 


p <- 
  ggplot(merged.table, aes(x = forcats::fct_rev(category), y=est)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  coord_flip() +
  geom_text(aes(label={scales::percent(est, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2.5, size = 4)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    labs(title = "Weitere Armutsindikatoren Bevölkerung ab 65 Schweiz", 
       subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p)



ggsave(
  plot = print(p),
  filename="graph4_verwandte-indikatoren.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph4_verwandte-indikatoren.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)




# set survey design for population estimates 
dsurvey.pop <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)
svytable(~poor.deprived.subjectivepoor, dsurvey.pop) # estimate on number of people affected by income poverty 


length(which(complete.cases(
  subset(sas.22, select=c(
    poverty.bn, 
    cant.face.sudden.exp, 
    difficulty.make.ends.meet)))))



# Armutsinidkatoren bei nichtkompensierbar --------------------------------



# ends meet 
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~cant.face.sudden.exp, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1

raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Kompensierbar oder gar nicht armutsbetroffen und kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten",
          "Gar nicht, oder kompensierbar armutsbetroffen und kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten")
raw.table1$category <- name
raw.table1
merged.table2 <- raw.table1
merged.table2

#ends meet 
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~difficulty.make.ends.meet, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Kompensierbar oder gar nicht armutsbetroffen und Schwierigkeiten über die Runden zu kommen",
          "Gar nicht oder kompensierbar armutsbetroffen und Schwierigkeiten über die Runden zu kommen")
raw.table1$category <- name

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2



merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se


levels(as.factor(merged.table2$category))

levels=c( "Kompensierbar oder gar nicht armutsbetroffen und kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten",
          "Gar nicht, oder kompensierbar armutsbetroffen und kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten",
          "Kompensierbar oder gar nicht armutsbetroffen und Schwierigkeiten über die Runden zu kommen", 
          "Gar nicht oder kompensierbar armutsbetroffen und Schwierigkeiten über die Runden zu kommen")                                         


labels=c( "Kompensierbar oder gar nicht armutsbetroffen und kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten",
          "Nicht kompensierbar armutsbetroffen und kann sich keine unvorhergesehenen Ausgaben über 2000 Fr. leisten",
          "Kompensierbar oder gar nicht armutsbetroffen und Schwierigkeiten über die Runden zu kommen", 
          "Nicht kompensierbar armutsbetroffen und Schwierigkeiten über die Runden zu kommen")                

length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category, 
                                 levels=levels, 
                                 labels=labels, 
                                 ordered=TRUE)
merged.table2

raw.table<-svymean(~poor.nothomeowner.less.30000.assets, dsurvey,na.rm=TRUE) # weighted tables using survey packages

pov.mean.line <- raw.table[1]
  

p2 <- 
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  geom_hline(yintercept = pov.mean.line, color='red', lwd=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  labs(title = "Armut und weitere Lebensbereiche", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p2)



ggsave(
  plot = print(p2),
  filename="graph_armutsindikatoren-auswegslos.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 300,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph_armutsindikatoren-auswegslos.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 300,
  units = "mm",
  dpi = 300
)






# Konfiguration Einkommensquellen -----------------------------------------


# 1. typ einkommensquellen
raw.table<-svymean(~inc.source.type.rcd, dsurvey, na.rm=TRUE) # weighted tables using survey packages
result.table1 <- raw.table
result.table1
names(result.table1)
result.table1 <- tidyr::as_tibble(result.table1)
result.table1
result.table1$lwr.ci <- result.table1$mean - result.table1$SE
result.table1$upr.ci <- result.table1$mean + result.table1$SE
result.table1$category <- names(raw.table)
result.table1$category <- gsub("inc.source.type.rcd","",result.table1$category)
result.table1

result.table1$mean <- as.numeric(result.table1$mean)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

svytable(~inc.source.type.rcd, dsurvey.pop)


p <- ggplot(result.table1, aes( y = mean, x = "", fill=reorder(category, +mean))) + geom_bar(width=1, stat="identity" ) +
  geom_text(aes(label = {scales::percent(mean, accuracy=0.1)}), position = position_stack(vjust =  0.5)) +
    scale_fill_brewer(palette="Blues") + scale_y_continuous(labels = scales::percent)+labs(fill = "")+
     theme_minimal() + ylab("") + xlab("")+labs(title = "Einkommensquellen der Bevölkerung ab 65 Jahren", 
                                                caption = "Quell              ée: Schweizer Alterssurvey 2022")
p

  

ggsave(
  plot = print(p),
  filename="graph5_einkommensquellen.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 200,
  height = 300,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph5_einkommensquellen.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 200,
  height = 300,
  units = "mm",
  dpi = 300
)



# Einkommensquelle und Armutsbetroffenheit --------------------------------

raw.table1<-svyby(~poverty.bn,by=~inc.source.type.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1

raw.table1$lwr.ci <- raw.table1$sample.mean.pov-raw.table1$se
raw.table1$upr.ci <- raw.table1$sample.mean.pov+raw.table1$se

raw.table1$se <-NULL
raw.table1

merged.table2 <- raw.table1

levels(as.factor(merged.table2$category))

merged.table2$category <- factor(merged.table2$category, levels=c(
  "Nur 1. Säule", 
  "1. Säule + Arbeit" ,
  "1. + 2. Säule", 
  "1. + 2. Säule + Arbeit"  , 
  "Andere"
), ordered=TRUE)

raw.table<-svymean(~poverty.bn, dsurvey,na.rm=TRUE) # weighted tables using survey packages
pov.mean.line <- raw.table[1]

p2 <- 
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  geom_hline(yintercept = pov.mean.line, color='red', lwd=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2.5, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(title = "Armutsbetroffenheit bei verschiedenen Konfiguration von Einkommensquellen", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent Armutsbetroffene") + xlab("")
print(p2)


ggsave(
  plot = print(p2),
  filename="graph6_einkommensquellen.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 350,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph6_einkommensquellen.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 350,
  units = "mm",
  dpi = 300
)



# Vermögen ----------------------------------------------------------------


svymean(~over.10e6.netwealth
        , dsurvey, na.rm=TRUE) # weighted tables using survey packages

# over 10e6
raw.table<-svymean(~over.10e6.netwealth
                     , dsurvey, na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Mehr als 1'000'000 CHF Nettovermögen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- result.table1
merged.table 


# 
# # Eigenheimbesitz und >100k
# raw.table<-svymean(~over.100.assets.and.houseowner
#                    , dsurvey, na.rm=TRUE) # weighted tables using survey packages
# est <- raw.table[1]
# lwr.ci<-confint(raw.table,level = 0.95)[1]
# upr.ci<-confint(raw.table,level = 0.95)[2]
# category <- "Mehr als 100'000 CHF liquides Haushaltsvermögen und Wohneigentumsbesitz"
# result.table1 <- cbind(est,lwr.ci,upr.ci,category)
# result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
# result.table1$est <- as.numeric(result.table1$est)
# result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
# result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
# result.table1
# 
# merged.table <- rbind(merged.table,result.table1)
# merged.table 

# poor.less.30000.assets.but.house
raw.table<-svymean(~poor.less.30000.assets.but.house
                   , dsurvey, na.rm=TRUE) 
raw.table 
# weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen, weniger als 30'000 CHF liquides Haushaltsvermögen aber Wohneigentum"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- rbind(merged.table,result.table1)
merged.table 

svytable(~poor.less.30000.assets.but.house, dsurvey.pop)

svytable(~poor.less.30000.assets.but.house, dsurvey.pop)


# multiarmut 
raw.table<-svymean(~poor.nothomeowner.less.30000.assets
                   , dsurvey, na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen, kein Wohneigentum und weniger als 30'000 CHF liquides Haushaltsvermögen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- rbind(merged.table,result.table1)
merged.table 

svytable(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight


levels(as.factor(merged.table$category))

levels=c("Mehr als 1'000'000 CHF Nettovermögen"  ,
         "Armutsbetroffen, weniger als 30'000 CHF liquides Haushaltsvermögen aber Wohneigentum", 
         "Armutsbetroffen, kein Wohneigentum und weniger als 30'000 CHF liquides Haushaltsvermögen")
                                                                     
                   

labels=c("Mehr als 1'000'000 CHF Nettovermögen"  ,
         "Armutsbetroffen, weniger als 30'000 CHF liquides Haushaltsvermögen aber Wohneigentum", 
         "Armutsbetroffen, kein Wohneigentum und weniger als 30'000 CHF liquides Haushaltsvermögen")


length(labels)==length(levels)

merged.table$category <- factor(merged.table$category, 
                                 levels=levels, 
                                 labels=labels, 
                                 ordered=TRUE)
merged.table



p2 <- 
  ggplot(merged.table, aes(x = forcats::fct_rev(category), y=est)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  coord_flip() +
  geom_text(aes(label={scales::percent(est, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-4, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(title = "Vermögenswerte", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p2)



ggsave(
  plot = print(p2),
  filename="graph7_vermoegen.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 200,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph7_vermoegen.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 200,
  units = "mm",
  dpi = 300
)



length(which(complete.cases(
  subset(sas.22, select=c(
    assetamount.merged, 
    homeowner.rcd, 
    poverty.bn)))))


#  other indicators  -------------------------------------------


# health 
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~sas.22$srh.bn, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1

raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Kompensierbar oder gar nicht armutsbetroffen und schlechte Gesundheit","Nicht kompensierbar armutsbetroffen und schlechte Gesundheit")
raw.table1$category <- name
raw.table1
merged.table2 <- raw.table1
merged.table2

#loneliness
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~feeling.lonely, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Kompensierbar oder gar nicht armutsbetroffen und einsam","Nicht kompensierbar armutsbetroffen und einsam")
raw.table1$category <- name

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


#lw life sat
raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~low.life.sat.bn, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Kompensierbar oder gar nicht armutsbetroffen und eher unzufrieden","Nicht kompensierbar armutsbetroffen und eher unzufrieden")
raw.table1$category <- name

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se


levels(as.factor(merged.table2$category))

levels=c( "Kompensierbar oder gar nicht armutsbetroffen und schlechte Gesundheit",
          "Nicht kompensierbar armutsbetroffen und schlechte Gesundheit",
          "Kompensierbar oder gar nicht armutsbetroffen und einsam", 
          "Nicht kompensierbar armutsbetroffen und einsam", 
          "Kompensierbar oder gar nicht armutsbetroffen und eher unzufrieden",
          "Nicht kompensierbar armutsbetroffen und eher unzufrieden")                                         
          

labels=c( "Kompensierbar oder gar nicht armutsbetroffen und schlechte Gesundheit",
          "Nicht kompensierbar armutsbetroffen und schlechte Gesundheit",
          "Kompensierbar oder gar nicht armutsbetroffen und einsam", 
          "Nicht kompensierbar armutsbetroffen und einsam", 
          "Kompensierbar oder gar nicht armutsbetroffen und eher unzufrieden",
          "Nicht kompensierbar armutsbetroffen und eher unzufrieden")    


length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category, 
                                 levels=levels, 
                                 labels=labels, 
                                 ordered=TRUE)
merged.table2

raw.table<-svymean(~poor.nothomeowner.less.30000.assets, dsurvey,na.rm=TRUE) # weighted tables using survey packages


pov.mean.line <- raw.table[1]

p2 <- 
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  geom_hline(yintercept = pov.mean.line, color='red', lwd=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  labs(title = "Armut und weitere Lebensbereiche", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p2)



ggsave(
  plot = print(p2),
  filename="graph8_andere-bereiche.png",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 300,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph8_andere-bereiche.svg",
  path="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/5_Auswertung/Armut und Nichtbezug/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 300,
  units = "mm",
  dpi = 300
)





# regressions -------------------------------------------------------------


#estimating logit regressions and bootstrap-simulations for predicted probabilities 

sas.22$pi_swissnational_22.fct <- factor(sas.22$pi_swissnational_22, levels=c("1","0"))

# poverty 
pov.model <- glm(poverty.bn ~ pi_sex_22 + pi_ageinterview_22 + edu.rcd + pi_swissnational_22.fct, family=binomial, data = sas.22)

coef.model <- exp(coef(pov.model))
                  
stargazer(pov.model, coef=list(coef.model),  
          type="text", 
          report="vc*",
          single.row=TRUE,
          p.auto = FALSE,
          digits=2)

stargazer(pov.model, coef=list(coef.model),  
          type="html", 
          report="vc*",
          single.row=TRUE,
          p.auto = FALSE,
          digits=2,
          out="regtable_income-poverty.html", 
          title="Logit-Regressionsmodell für Einkommensarmut",
          covariate.labels=c(
            "Frauen",  
            "Alter",
            "Tertiärbildung (Ref. Sek. II)",
            "Obligatorische Schulbildung",
            "Ausländer:In",
            "Konstante"),
          dep.var.caption="Odds-Ratio", 
          dep.var.labels=c(""))



pov.model <- glm(poverty.bn ~ pi_sex_22 + pi_ageinterview_22 + edu.rcd + pi_swissnational_22, family=binomial, data = sas.22)


predict(pov.model, data.frame(pi_sex_22 = 1, 
                              pi_ageinterview_22 = 68, 
                              edu.rcd = "low",
                              pi_swissnational_22 = 1),
        type = "response")


newdata <- data.frame(pi_sex_22 = c(1,1,1,1,1,1,1,
                                    2,2,2,2,2,2,2), 
           pi_ageinterview_22 = c(78,78,67,80,67, 69,70, 
                                  78,78,67,80,67, 69,70), 
           edu.rcd = c("low", "high", "avg","avg","avg","low", "high",
                       "low", "high", "avg","avg","avg","low", "high"),
           pi_swissnational_22 = c(1,1,1,1,0,0,0,
                                   1,1,1,1,0,0,0))

predict.table <- boot_predict(pov.model, newdata = newdata, R = 1000, confint_sep="-", estimate_name="Schätzung Armutsrisiko")

predict.table <- as_tibble(predict.table)

predict.table <- predict.table %>% mutate(
  Geschlecht=case_when(pi_sex_22 == 1 ~ "Mann", 
                       pi_sex_22 == 2 ~ "Frau"),
  Alter=pi_ageinterview_22,
  Bildung=case_when(edu.rcd=="low" ~ "Obligatorische Schule", 
                    edu.rcd=="avg" ~ "Sekundarschule II", 
                    edu.rcd=="high" ~ "Tertiärstufe"),
  Nationalität=case_when(pi_swissnational_22==1 ~ "Schweiz", 
                                   pi_swissnational_22==0 ~ "Italien")) %>% 
  select(Geschlecht,Alter,Bildung,Nationalität,'Schätzung Armutsrisiko')

predict.table 
write.csv(predict.table, file="table_predicted-probabilities-poverty.csv")


# auswegslos arm


# poverty 
pov.model2 <- glm(poor.nothomeowner.less.30000.assets ~ pi_sex_22 + pi_ageinterview_22 + edu.rcd + pi_swissnational_22.fct, family=binomial, data = sas.22)



summary(pov.model2)

coef.model2 <- exp(coef(pov.model2))

stargazer(pov.model2, coef=list(coef.model2),  
          type="text", 
          report="vc*",
          single.row=TRUE,
          p.auto = FALSE,
          digits=2)

stargazer(pov.model2, coef=list(coef.model2),  
          type="html", 
          report="vc*",
          single.row=TRUE,
          p.auto = FALSE,
          digits=2,
          out="regtable_auswegslosarm.html", 
          title="Logit-Regressionsmodell für Einkommensarmut",
          covariate.labels=c(
            "Frauen",  
            "Alter",
            "Tertiärbildung (Ref. Sek. II)",
            "Obligatorische Schulbildung",
            "Ausländer:In",
            "Konstante"),
          dep.var.caption="Odds-Ratio", 
          dep.var.labels=c(""))


pov.model2 <- glm(poor.nothomeowner.less.30000.assets ~ pi_sex_22 + pi_ageinterview_22 + edu.rcd + pi_swissnational_22, family=binomial, data = sas.22)


predict(pov.model2, data.frame(pi_sex_22 = 1, 
                              pi_ageinterview_22 = 68, 
                              edu.rcd = "low",
                              pi_swissnational_22 = 1),
        type = "response")


newdata <- data.frame(pi_sex_22 = c(1,1,1,1,1,1,1,
                                    2,2,2,2,2,2,2), 
                      pi_ageinterview_22 = c(78,78,67,80,67, 69,70, 
                                             78,78,67,80,67, 69,70), 
                      edu.rcd = c("low", "high", "avg","avg","avg","low", "high",
                                  "low", "high", "avg","avg","avg","low", "high"),
                      pi_swissnational_22 = c(1,1,1,1,0,0,0,
                                              1,1,1,1,0,0,0))

predict.table2 <- boot_predict(pov.model2, newdata = newdata, R = 1000, confint_sep="-", estimate_name="Schätzung Armutsrisiko")

predict.table2 <- as_tibble(predict.table2)

predict.table2 <- predict.table2 %>% mutate(
  Geschlecht=case_when(pi_sex_22 == 1 ~ "Mann", 
                       pi_sex_22 == 2 ~ "Frau"),
  Alter=pi_ageinterview_22,
  Bildung=case_when(edu.rcd=="low" ~ "Obligatorische Schule", 
                    edu.rcd=="avg" ~ "Sekundarschule II", 
                    edu.rcd=="high" ~ "Tertiärstufe"),
  Nationalität=case_when(pi_swissnational_22==1 ~ "Schweiz", 
                         pi_swissnational_22==0 ~ "Italien")) %>% 
  select(Geschlecht,Alter,Bildung,Nationalität,'Schätzung Armutsrisiko')

predict.table2 
write.csv(predict.table2, file="table_predicted-probabilities-poverty2.csv")


