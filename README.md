# Anne E. Sartori


# Survival Analysis for Understanding the Association Between Systolic Blood
# Pressure and Mortality: The Framingham Heart Study

# This folder contains work for my final project for BS 852 (Statistics Methods in Epidemiology, Fall, 2024). 


# The report aims to understand the association between systolic blood pressure 
# and mortality using a subset of data from the Framingham Heart Study.

#  Report: https://github.com/annesartori1/blood-pressure-mortality/report/Sartori 852 Report.pdf).
#  Script: https://github.com/annesartori1/blood-pressure-mortality/script/Sartori SPF mortality.R
#  Skills: Survival analysis, Cox proportional hazard models, including proportional hazards test, time-varying covariates 

#  Data: framdat4.csv (private)
#  Description: 

#  Subjects had data measured at exam 4, and then the incidence of coronary heart disease (CHD), type 2 diabetes (T2D), and death (DTH) were recorded in the following 22 years of follow-up. 

# Variables included in the data file are:
# SEX: (1=Male)
# AGE4: age at exam 4;
# CHOL4: total cholesterol at exam 4;
# CIGS4: number of cigarettes smoked per day, at exam 4
# SMOKE: this is a variable that denotes smoking (1=yes) at exam 4;
# SPF4 and DPF4: systolic and diastolic blood pressure at exam 4;
# WGT4: weight in pounds at exam 4;
# FVC4: pulmonary function at exam 4;
# BMI4: bmi at exam 4;
# HTN4: hypertension at exam 4 (1 = yes)
# MENO4; menopause at exam 4 (1 = yes)
# DHT: mortality, 1 if subject died before the end of 22 years of follow up
# SURV: years --after exam 4-- at which death was recorded, or longest follow up if a subject dropped from the study before the end of 22 years.
# CHD: 1 if CHD occurred during follow up
# CHD_Surv: years after exam 4 at which CHD occurred, if CHD=1, otherwise longest follow up.
# T2D: type 2 diabetes after exam 4 (1 = yes)
# T2D_Surv: years after exam 4 at which T2D occurred, if T2D=1, otherwise longest follow up.
