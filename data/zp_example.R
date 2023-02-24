library(foreign); library(designmatch);library(car)
library(MatchIt); library(mice); library(sandwich)
library(psych);library(twang);library(MatchThem);library(cobalt)
library(lavaan); library(nbpMatching);library(tidyverse)

zp <- read.csv("data/zp_synth.csv")


zp$K4_read<-car::recode(zp$K4_1451, 'c(1,2)=0; c(3,4,5)=1; else=NA')
zp$K5_read<-car::recode(zp$K5_2501, 'c(1,2)=0; c(3,4,5)=1; else=NA')
zp$K6_read<-car::recode(zp$K6_2501, 'c(1,2)=0; c(3,4,5)=1; else=NA')
zp$K7_read<-car::recode(zp$K7_2501, 'c(1,2)=0; c(3,4,5)=1; else=NA')

zp <- zp[!is.na(zp$K6_read), ]

zp <- zp %>% transmute(
    Gender = ADR_GenderChild,
    Anxiety_age17 = K7_652+K7_653+K7_656+K7_658,
    Reading_age15 = K6_read,
    AnxietyDepression_age13 = K5_nSBQ_ANXDEP,
    Trust_age13 = K5_GTrust,
    SelfControl_age13 = K5_SCTRL,
    SubstanceUse1_age13 = K5_1311,
    SubstanceUse2_age13 = K5_1321,
    SubstanceUse3_age13 = K5_1331,
    SubstanceUse4_age13 = K5_1341,
    TeacherBond_age13 = K5_SchoolTeach,
    ClassBond_age13 = K5_SchoolClass,
    SchoolDifficulties_age13 = K5_SchoolDiffic,
    Achievements1_age13 = T4.3_ACHI01,
    Achievements2_age13 = T4.3_ACHI02,
    Achievements3_age13 = T4.3_ACHI03,
    K5_nSBQ_REAGGR,
    K5_nSBQ_PHYSAGGR,
    K5_nSBQ_PROAGGR,
    K5_nSBQ_INDAGGR,
    K5_nSBQ_PROSO,
    K5_DevVar19,
    K5_involv,
    K5_ResilAdult,
    K5_ResilFriends,
    K5_BullVict4
  )

# write_csv(zp,file="data/zp_eg.csv")
#zp <- read_csv("data/zp_eg.csv")
set.seed(555)
predmatrix<-matrix(nrow=dim(zp)[2], ncol=dim(zp)[2], data=rep(1,dim(zp)[2]*dim(zp)[2]))
predmatrix[c(1),]<-0 
predmatrix[,c(1)]<-0
diag(predmatrix)<-0
zp_mice_all<-mice(zp, m=10, predictorMatrix=predmatrix)
zp_read_all_imp<-complete(zp_mice_all)

# age 17 PSM outcome analyses not including prior reading:
Age17_matches<-matchthem(Reading_age15~
                           Gender + AnxietyDepression_age13 + Trust_age13 + SelfControl_age13 + 
                           SubstanceUse1_age13 + SubstanceUse2_age13 + 
                           SubstanceUse3_age13 + SubstanceUse4_age13 + 
                           TeacherBond_age13 + ClassBond_age13 + SchoolDifficulties_age13 + 
                           Achievements1_age13 + Achievements2_age13 + Achievements3_age13 + 
                           K5_nSBQ_REAGGR + K5_nSBQ_PHYSAGGR + K5_nSBQ_PROAGGR + K5_nSBQ_INDAGGR + 
                           K5_nSBQ_PROSO + K5_DevVar19 + 
                           K5_involv + K5_ResilAdult + K5_ResilFriends + K5_BullVict4,
                datasets=zp_mice_all, approach='within', method='nearest',link='logit', tols=.10, ratio=1)

bal.tab(Age17_matches) #assess quality of matching

#anxiety age 17 outcome analysis (no adjustemtn:
summary(pool(with(Age17_matches, 
                  lm(Anxiety_age17~Reading_age15))))

#anxiety age 17 outcome analysis adjusting for matching variables

