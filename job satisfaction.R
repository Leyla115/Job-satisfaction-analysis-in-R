
con<-url("http://kodu.ut.ee/~avork/files/oppetoo/micro/piaacHW2PII.Rdata")
load(con) #Load the data
summary(dfr)


library("nnet") 
mlogit.modell<- multinom(jobsat ~ ageg5lfs + male + edcat7 + immig + indep + workreq + relwage + geo, data = dfr, Hess=TRUE)
summary(mlogit.modell)
library(stargazer)
stargazer(mlogit.modell, type="text", no.space=TRUE)

table(dfr$jobsat)
#a) Interpret the results (the signs of the beta coefficients).

#age 20-24 compare to age 25-29 have lower probability(-0.014) to be satisfied ther jobs;and neutral: have higher prob(0.188) to be very satisfied compare to nonstisfied
#age 35-39 compare to age 25-29 have lower prob to be satisfied; tobe very satisfied
#age 45-49 compare to age 25-29 have higher prob (0.240) to be satistied; lower prob to be very satisfied

#male compare to female have lower prob to be satisfied; and very satisfied

#People with MA degree compared to people with upper have lower prob to satisfied: and very satisfied

#people in itali have higher prob compare to estonia to be satisfied:  and very satisfied

#people in japan have lower prob compare to estonia to satisfies: very satisfied


#b)  Calculate relative risk ratios

exp(coef(mlogit.modell))


#age 20-24 have 0.94 times lower RRR than aged 25-29 to be neutral compate to ninsatisfied
#male have 0.68 times lower RRR than female (or 1-0.68=0.32 0r 32 % lower relative risk) to be neurtal compare to non satisfied



#c) Test if the assumptions of IIA are satisfied. For this you need to use package mlogit.
library(mlogit)
dfrmlogit <- mlogit.data(dfr, varying = NULL,  shape = "wide", choice = "jobsat")

mlogit.modell <- mlogit(jobsat ~ 0 | ageg5lfs + male + edcat7 + immig + indep + workreq + relwage + geo, data=dfrmlogit, reflevel = "NotSatisfied",  alt.subset=c("NotSatisfied", "Neutral", "Satisfied", "VerySatisfied"))

mlogit.modell.nochoice2<- mlogit(jobsat ~ 0 | ageg5lfs + male + edcat7 + immig + indep + workreq + relwage + geo,  data=dfrmlogit, reflevel = "NotSatisfied", alt.subset=c("NotSatisfied", "Satisfied", "VerySatisfied"))

hmftest(mlogit.modell, mlogit.modell.nochoice2)
# => We cannot reject IIA assumption, as p=1. if p>0.05 we cannor rejet IIA


mlogit.modell.nochoice3 <- mlogit(jobsat ~ 0 | ageg5lfs + male + edcat7 + immig + indep + workreq + relwage + geo, data=dfrmlogit, reflevel = "NotSatisfied",  alt.subset=c("NotSatisfied", "Neutral", "VerySatisfied"))

hmftest(mlogit.modell, mlogit.modell.nochoice3)


mlogit.modell.nochoice1 <- mlogit(jobsat ~ 0 | ageg5lfs + male + edcat7 + immig + indep + workreq + relwage + geo, data=dfrmlogit, reflevel = "NotSatisfied",  alt.subset=c("Neutral", "Satisfied", "VerySatisfied"))
hmftest(mlogit.modell, mlogit.modell.nochoice1)


mlogit.modell.nochoice4 <- mlogit(jobsat ~ 0 | ageg5lfs + male + edcat7 + immig + indep + workreq + relwage + geo, data=dfrmlogit, reflevel = "NotSatisfied",  alt.subset=c("NotSatisfied", "Neutral", "Satisfied"))
hmftest(mlogit.modell, mlogit.modell.nochoice4)


#d) Can we merge options 1 and 2 ("NotSatisfied", "Neutral") using Wald test?
  library(aod)
wald.test(b = coef(mlogit.modell),  #coefficients from the model
          Sigma = vcov(mlogit.modell),  #covariance matrix from the model
          Terms = setdiff(grep("Neutral:", names(coef(mlogit.modell))),1))


#p<0.05  jecet null hp  so we cannot merge

#e)marginal effect

library(fastDummies)

dfd2 <- fastDummies::dummy_cols(dfr, select_columns = c("ageg5lfs", "edcat7", "geo", "workreq"), 
                               remove_selected_columns = TRUE)
dfd2 <-  dfd2 %>% 
  mutate(male = as.numeric(as.character(male)), #make 1-0 variables, not factors
         immig = as.numeric(as.character(immig))) %>% 
  #drop baseline categories
  dplyr::select(-`ageg5lfs_25-29`, -edcat7_UpSec, -geo_est, -`workreq_This level necessary`)
head(dfd2)
table(dfr$workreq)
#again, make long format
dfdlongg <- mlogit.data(dfd2, 
                       varying = NULL,  #variables that are alternative specific - we do not have these
                       shape = "wide",  #we have data in wide form, each row is one observation
                       choice = "jobsat") #dependent variable


mlogit.modell2 <- mlogit(jobsat ~ 0 | male +relwage +indep + immig +
                           ageg5lfs_20.24 +ageg5lfs_30.34+ageg5lfs_35.39 + ageg5lfs_40.44 +ageg5lfs_45.49 +ageg5lfs_50.54 +ageg5lfs_55.59 +ageg5lfs_60.65+
                           edcat7_LowSec + edcat7_PostSec + edcat7_TertProf + edcat7_TertBA + edcat7_TertMA +
                           geo_ita + geo_jpn+ workreq_Lower.level.sufficient +workreq_Higher.level.needed,data=dfdlongg, reflevel = "NotSatisfied")

summary(mlogit.modell2)

(c.names <- names(mlogit.modell2$model)[c(2:22)])  #we need names
names(mlogit.modell2$model)

ME.mnl <- sapply(c.names, function(x) 
  stats::effects(mlogit.modell2, covariate=x, data=dfdlongg),  #command "effects" calculates effect of changes
  simplify=FALSE)
round((AME.mnl <- t(sapply(ME.mnl, colMeans))),4)


