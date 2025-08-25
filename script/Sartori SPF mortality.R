# Anne E. Sartori
# 

# Survival Analysis for Understanding the Association Between Systolic Blood
# Pressure and Mortality: The Framingham Heart Study

# BS 852, Fall, 2024

###########PREPARE WORKSPACE AND PACKAGES###########
rm(list=ls())

library(survival)
library(dplyr)  

# Read data
framdat4 <- read.csv("framdat4.csv", header=T, na.strings=c("."))
framwork <- framdat4 #Create working dataset

#Exploratory data analysis deleted from this file for brevity

###########ANALYSIS PLAN###########

# Since all the variables in the data set could impact health and survival, I take this approach:

# I exclude DPF4 and HTN4, because they're too closely related to our main var of interest, SPF4
# (e.g., HTN is created largely from the blood pressure variables)
# I don't include CHD and T2D because these are competing risks, which we have been instructed to ignore,
# and also have a lot of missing data


# 1. I estimate crude Cox Prop. Hazard (CPH) models with each covariate separately to assess association with survival
#    Each covariate is individually statistically significant--I include them in my multivariable model 
#    except WEIGHT (bc similar concept to BMI) and SMOKE (similar concept to CIGS4ot smoke)
# 2. I evaluate whether the other covariates confound the effect of SPF4 to report in the Methods section.
# 3. I estimate 3 CPH models with the predictor of interest (SPF4) and covariates: 1) with systolic blood pressure (SPF4) and age;
#    2) with SPF and all the covariates, including an interaction between SPF4 and sex; 3) with SPF4 and all the covariates,
#    including time-varying covariates where appropriate 
# 4. I SUBSET THE DATA and rerun the analyses separately by sex, including looking at menopause for women only. 
#    In parts (3) and (4), I stratify by age because it's a predictor of mortality and much research presented in BH 852 
#     based on the Framingham Heart Study stratified by age.
 

###########DATA PREPARATION###########

age <- as.numeric(framdat4$AGE4)

# Numeric version of sex (1 is male) for later plotting
framwork$SEX <- as.numeric(framwork$SEX)
framwork$SEX <- ifelse(framwork$SEX == 2, 0,
                  ifelse(framwork$SEX == 1, 1, NA))
table(framdat4$SEX,framwork$SEX, useNA="ifany") #Check for accuracy.

# Convert to factors
framwork$HTN4 <- factor(framwork$HTN4, levels = c(0, 1), labels = c("No Hyper", "Hypertension"))
framwork$CHD <- factor(framwork$CHD, levels = c(0, 1), labels = c("No CHD", "CHD"))
framwork$T2D <- factor(framwork$T2D, levels = c(0, 1), labels = c("T2 Diabetes", "No Diabetes2"))

# Create age strata. Age range[34,69] 35 years
summary(framwork$AGE4)
framwork$age.group <- rep(1, nrow(framdat4))
framwork$age.group[ which(framdat4$AGE >44 & framdat4$AGE <55)] <-2
framwork$age.group[ which(framdat4$AGE >54 & framdat4$AGE <65)] <-3
framwork$age.group[ which(framdat4$AGE >64 & framdat4$AGE <75)] <-4
framwork$age.group <- factor(framwork$age.group, levels = c(1,2,3,4), labels = c("34-44", "45-54", "55-64", "65-74"))
table(framwork$age.group, useNA="ifany")

#Create interaction term SPF and male to use in later models. 
SPFSEX <-framdat4$SPF4*framdat4$SEX 

###########Cox Proportional Hazard Models###########


########### 1. Crude Models for evaluating association between covariates and survival###########

#SPF 
m1 <- coxph(Surv(SURV, DTH) ~ SPF4, data=framwork)
summary(m1)
#Chol 
m2 <- coxph(Surv(SURV, DTH) ~ CHOL4, data=framwork) 
summary(m2)
#Age  
m3 <- coxph(Surv(SURV, DTH) ~ AGE4, data=framwork) 
summary(m3)
#cigs 
m4 <- coxph(Surv(SURV, DTH) ~ CIGS4, data=framwork) 
summary(m4)
#BMI
m5 <- coxph(Surv(SURV, DTH) ~ BMI4, data=framwork) 
summary(m5)
m6 <- coxph(Surv(SURV,DTH) ~ FVC4, data=framwork)
summary(m6) 

########### 2. EVALUATE CONFOUNDING IN SIMPLE MODELS###########

# CIGS
mspluscigs <- coxph(Surv(SURV, DTH) ~ SPF4 + CIGS4, data=framwork)
summary(mspluscigs)

# CHOL4
mspluschol <- coxph(Surv(SURV, DTH) ~ SPF4 + CHOL4, data=framwork)
summary(mspluschol)

# FVC4
msplusFVC <- coxph(Surv(SURV, DTH) ~ SPF4 + FVC4, data=framwork)
summary(msplusFVC)

#AGE4
msplusage <- coxph(Surv(SURV, DTH) ~ SPF4 + AGE4, data=framwork)
summary(msplusage)

# All the other variables together, except age, SEX, MENO4 which I plan to include (no)
mfull <- coxph(Surv(SURV, DTH) ~ SPF4 + CHOL4+CIGS4+DPF4+WGT4+FVC4+BMI4+ HTN4+CHD+T2D, data=framwork)
summary(mfull)

#####################################################################################################
#REMOVE MISSING, CALCULATE DESCRIPTIVE STATS FOR TABLE, FIT FINAL MODELS, AND CHECK PROPORTIONAL HAZARDS

framNM <- subset(framwork, !is.na(SURV) & !is.na(DTH) & !is.na(SPF4) & !is.na(SEX) & !is.na(AGE4) 
                 & !is.na(CHOL4) & !is.na(CIGS4) &  !is.na(BMI4) & !is.na(FVC4)) & !is.na(SPFSEX)


###########DESCRIPTIVE STATS FOR TABLE 1 IN APPENDIX###########

#MENO4 missing for men, so this is the percent for women
mean(framNM$MENO4, na.rm =TRUE)
table(framNM$SEX)

# Summarize vars by sex.
summary_bysex <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(SPF4, na.rm = TRUE),
    min = min(SPF4, na.rm = TRUE),
    max = max(SPF4, na.rm = TRUE),
    Count = n()
  )
summary_bysex

agesummary_bysex <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(AGE4, na.rm = TRUE),
    min = min(AGE4, na.rm = TRUE),
    max = max(AGE4, na.rm = TRUE),
    Count = n()
  )

agesummary_bysex

cholsummary_bysex <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(CHOL4, na.rm = TRUE),
    min = min(CHOL4, na.rm = TRUE),
    max = max(CHOL4, na.rm = TRUE),
  )
cholsummary_bysex

cholsummary_bysex <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(CHOL4, na.rm = TRUE),
    min = min(CHOL4, na.rm = TRUE),
    max = max(CHOL4, na.rm = TRUE),
  )

bmisummary_bysex  <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(BMI4, na.rm = TRUE),
    min = min(BMI4, na.rm = TRUE),
    max = max(BMI4, na.rm = TRUE),
  )
bmisummary_bysex

cigssummary_bysex  <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(CIGS4, na.rm = TRUE),
    min = min(CIGS4, na.rm = TRUE),
    max = max(CIGS4, na.rm = TRUE),
  )
cigssummary_bysex

fvcsummary_bysex  <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(FVC4, na.rm = TRUE),
    min = min(FVC4, na.rm = TRUE),
    max = max(FVC4, na.rm = TRUE),
  )
fvcsummary_bysex

survsummary_bysex  <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(SURV, na.rm = TRUE),
    min = min(SURV, na.rm = TRUE),
    max = max(SURV, na.rm = TRUE),
  )
survsummary_bysex

survsummary_bysex  <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(SURV, na.rm = TRUE),
    min = min(SURV, na.rm = TRUE),
    max = max(SURV, na.rm = TRUE),
  )
survsummary_bysex

dthsummary_bysex  <- framNM %>%
  group_by(SEX) %>%
  summarise(
    mean = mean(DTH, na.rm = TRUE),
    min = min(DTH, na.rm = TRUE),
    max = max(DTH, na.rm = TRUE),
  )
dthsummary_bysex


########### 3. TABLE 2: SURVIVAL MODELS STRATIFIED BY AGE###########

#m00 is Model 1 in Table 2: stratified by age, adjust for age only
m00 <- coxph(Surv(SURV, DTH) ~ SPF4 + AGE4 + strata(age.group), data=framNM)
summary(m00)
cox.zph(m00) #none significant: reject hypothesis of nonproportional hazard

# m03.s is Model 2 in Table 2
m03.s <- coxph(Surv(SURV, DTH) ~ SPF4 + SEX + SPFSEX +  CHOL4 +AGE4
               + CIGS4  + BMI4 + FVC4, data=framNM)
summary(m03.s)
cox.zph(m03.s)  # test proportional hazards assumption

# PLRT to see if SPF vars matter together : No
m03s_noSPF <- update(m03.s, .~. - SPF - SPFSEX)

# Compare models using anova
anova_noSPF <- anova(m03.s, m03s_noSPF)
print(anova_noSPF)


# TIME-VARYING MODEL 

# In m03.s, the interaction of SEX and SPF4 violate proportional hazards
# I also include time-varying SEX and SPF4 in the time-varying model to 
# compare the effect of SPF over time for men and women

# m03.t is Model 3 in Table 2
m03.t <- coxph(Surv(SURV, DTH) ~ SPF4 + SEX + SPFSEX +  CHOL4 + CIGS4  + BMI4 + AGE4+ FVC4 + tt(SEX) + tt(SPF4) + tt(SPFSEX) , data=framNM, tt=function(x,t,...)x*t)
summary(m03.t)
summary(framNM$SPF4)
sd(framNM$SPF4)

# PLRT of joint significance of SPF vars
m03.t_noSPF <- coxph(Surv(SURV, DTH) ~  SEX + CHOL4 + CIGS4  + BMI4 + AGE4 + FVC4+ tt(SEX)  , data=framNM, tt=function(x,t,...)x*t)
summary(m03.t_noSPF)

m03.t$loglik
m03.t_noSPF$loglik  
CHItest <- -2*(m03.t_noSPF$loglik[[2]]-m03.t$loglik[[2]])
CHItest #4 df: SPF, interaction, and 2 tt terms

pchisq(CHItest, 4, lower.tail = FALSE)

# Same test using anova
anova2_noSPF <- anova(m03.t, m03.t_noSPF)
print(anova2_noSPF)

# Holding constant the other variables, the hazard ratio for male to female is
# e^(beta_sex + (beta_tt(sex)*t + beta_spfsex*SPF4 + (beta_tt(spfsex))t*SPF4)

#Create a vector representing time from 0 to 22 years

time <- 0:22  # create a time vector

# Figure (b) on page 2: Plot the time varying ratio varying SPF, holding sex constant at male: man with mean SPF(=134) and a man one SD above the mean (134+24.28861=158.2886)

hazard_SPFtm <- exp((m03.t$coefficients[[1]])*(24.28861)+ (m03.t$coefficients[[3]])*(24.28861) 
                    + ((m03.t$coefficients[[10]])*(24.28861)*time) + ((m03.t$coefficients[[11]])*(24.28861)*time))

#Holding sex constant at female compare hazard for one SD of SPF higher to SPF at mean
hazard_SPFtf <- exp((m03.t$coefficients[[1]])*(24.28861) + ((m03.t$coefficients[[10]])*(24.28861)*time)) 

#COMBINE PLOTS
SPFwomenplot <- plot(time, hazard_SPFtf,  col = "blue", pch = 16, ylab="hazard ratio"
                     , main ="Hazard Ratio, One SD Increase\nin Systolic Blood Pressure at Exam 4", cex.main = .9)
points(time, hazard_SPFtm, col = "red", pch = 17)
legend("topright", legend = c("women", "men"),
       col = c("blue", "red"), pch = c(16, 17))


########### 4. SEPARATE ANALYSES FOR WOMEN AND MEN###########

# WOMEN
framNM.f <- subset(framNM, SEX==0)  #Subset of the data with women

#M03 is the model in the second column of Table 3
m03.f0 <- coxph(Surv(SURV, DTH) ~ SPF4 + CHOL4 + CIGS4  + BMI4 +AGE4 + FVC4, data=framNM.f)
summary(m03.f0)
cox.zph(m03.f0) #Now I find time-varying effects of cigs

#Same model with menopause: Last column of Table 3

m03.meno <- coxph(Surv(SURV, DTH) ~ SPF4 + AGE4 + MENO4 + CHOL4 + CIGS4  + BMI4 +FVC4, data=framNM.f)
summary(m03.meno)
cox.zph(m03.meno) 

##TIME-VARYING effect of CIGS isn't significant in the time-varying model I ran so not a priority for my 2-page report

# MEN

framNM.m <- subset(framNM, SEX==1) #Subset of the data with men

#Model 03.m is the first column of Table 3
m03.m <- coxph(Surv(SURV, DTH) ~ SPF4 + AGE4 +CHOL4 + CIGS4  + BMI4 +FVC4, data=framNM.m)
summary(m03.m)
cox.zph(m03.m) #For men, there are no time-varying effects

# Figure (a) on page 2: Plot SPF hazards grouped by SEX 

spf <- seq(0, 48, 2) # SPF values vector for plot of hazard ratio

hazard_spff <- exp((m03.f0$coefficients[[1]])*spf)
hazard_spfm <- exp((m03.m$coefficients[[1]])*spf)

#Combine graphs:
spfplot.f <- plot(spf, hazard_spff,  col = "blue", pch = 16,xlab="change in SPF", ylab="hazard ratio", 
                  main = "Hazard ratio for change in systolic blood pressure",
                  cex.main = 0.9)

points(spf, hazard_spfm, col = "red", pch = 17)
legend("topright", legend = c("women", "men"),
       col = c("blue", "red"), pch = c(16, 17))

#Full-sample SDs for use in Conclusion
sd(framNM$SPF4, na.rm=FALSE)
sd(framNM.m$SPF4)
sd(framNM.f$SPF4)

#NOTE: The report also includes some calculations done by calculator.