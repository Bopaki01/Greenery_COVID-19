# Greenery_COVID-19

rm(list=ls())
file.choose()


###Modeling Greenness and COVID-19 infection 

###############Analytical Scale: Local Municipality
prevalence <- read.table( "C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev  Mar 2023\\Results\\Data Curation\\R Datasets\\Local_Mun.txt", header = TRUE)

attach(prevalence)
head(prevalence)

remove(Model1)

###############
##Test for Multicollinearity
library(car)

model_0 <- lm(cases~age+revenue_capita+pop+area+mean, data = prevalence)
vif(model_0)


################


library(glmmTMB)


        #### Spatial Scale: Local Municipality; Response Variable: Number of infections (Family = negative binomial)

Model1 <- glmmTMB(cases ~ max + age + pop + area + revenue_capita +
                    (1|province/district), data = prevalence, family = nbinom2)
summary(Model1)


Model2 <- glmmTMB(cases ~ mean + age + pop + area +  revenue_capita + 
                    (1|province/district), data = prevalence, family = nbinom2)
summary(Model2)


Model3 <- glmmTMB(cases ~ median + age + pop + area +  revenue_capita +
                    (1|province/district), data = prevalence, family = nbinom2)
summary(Model3)


Model4 <- glmmTMB(cases ~ range + age + pop + area +  revenue_capita +
                    (1|province/district), data = prevalence, family = nbinom2)
summary(Model4)


Model5 <- glmmTMB(cases ~ stdev + age + pop + area +  revenue_capita +
                    (1|province/district), data = prevalence, family = nbinom2)
summary(Model5)


Model6 <- glmmTMB(cases ~ forest + age + pop + area +  revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model6)


Model7 <- glmmTMB(cases ~ grassland + age + pop + area +  revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model7)



        #### Spatial scale: Local Municipality; Response Variable: Number of infections per population (Family = Beta)

Model8 <- glmmTMB(cases_pop ~ max + age + area + revenue_capita + 
                     (1|province/district), data = prevalence, family = beta_family)
summary(Model8)


Model9 <- glmmTMB(cases_pop ~ mean + age + area + revenue_capita +
                     (1|province/district), data = prevalence, family = beta_family)
summary(Model9)


Model10 <- glmmTMB(cases_pop ~ median + age + area + revenue_capita +
                     (1|province/district), data = prevalence, family = beta_family)
summary(Model10)


Model11 <- glmmTMB(cases_pop ~ range + age + area + revenue_capita +
                    (1|province/district), data = prevalence, family = beta_family)
summary(Model11)


Model12 <- glmmTMB(cases_pop ~ stdev + age + area + revenue_capita + 
                     (1|province/district), data = prevalence, family = beta_family)
summary(Model12)


Model13 <- glmmTMB(cases_pop ~ forest + age + area + revenue_capita + 
                     (1|province/district), data = prevalence, family = beta_family)
summary(Model13)


Model14 <- glmmTMB(cases_pop ~ grassland + age + area + revenue_capita +
                    (1|province/district), data = prevalence, family = beta_family)
summary(Model14)



        ### Spatial Scale: Local Municipality; Response Variable: Number of cases per surface area (Family = negative binomial)

Model15 <- glmmTMB(cases_area ~ max + age + pop + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model15)


Model16 <- glmmTMB(cases_area ~ mean + age + pop + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model16)


Model17 <- glmmTMB(cases_area ~ median + age + pop + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model17)


Model18 <- glmmTMB(cases_area ~ range + age + pop + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model18)


Model19 <- glmmTMB(cases_area ~ stdev + age + pop + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model19)


Model20 <- glmmTMB(cases_area ~ forest + age + pop  + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model20)


Model21 <- glmmTMB(cases_area ~ grassland + age + pop + revenue_capita +
                     (1|province/district), data = prevalence, family = nbinom2)
summary(Model21)


############### Analytical Scale: Municipal Ward
prevalence_2 <- read.table( "C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev  Mar 2023\\Results\\Data Curation\\R Datasets\\Mun_Ward.txt", header = TRUE)
attach(prevalence_2)
head(prevalence_2)
library(glmmTMB)

##Test for Multicollinearity
library(car)

model_00 <- lm(cases~age+revenue_capita+pop+area+mean, data = prevalence_2)
vif(model_00)
        

        #### Spatial Scale: Municipal Ward; Response Variable: Number of infections (Family = negative binomial)

Model22 <- glmmTMB(cases ~ max + age + pop + area +  revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model22)


Model23 <- glmmTMB(cases ~ mean + age + pop + area +  revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model23)


Model24 <- glmmTMB(cases ~ median + age + pop + area +  revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model24)


Model25 <- glmmTMB(cases ~ range + age + pop + area +  revenue_capita + 
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model25)


Model26 <- glmmTMB(cases ~ stdev + age + pop + area +  revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model26)


Model27 <- glmmTMB(cases ~ forest + age + pop + area +  revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model27)


Model28 <- glmmTMB(cases ~ grassland + age + pop + area +  revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model28)



        #### Spatial Scale: Municipal Ward; Response Variable: Number of infections per population (Family = Beta)

Model29 <- glmmTMB(cases_pop ~ max + age + area + revenue_capita +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~max + age + area + revenue_capita, family = beta_family)
summary(Model29)


Model30 <- glmmTMB(cases_pop ~ mean + age + area + revenue_capita +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~mean + age + area + revenue_capita, family = beta_family)
summary(Model30)


Model31 <- glmmTMB(cases_pop ~ median + age + area + revenue_capita +
                     (1|province/district/local), data = prevalence_2, 
                   ziformula = ~median + age + area + revenue_capita, family = beta_family)
summary(Model31)


Model32 <- glmmTMB(cases_pop ~ range + age + area + revenue_capita +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~range + age + area + revenue_capita, family = beta_family)
summary(Model32)


Model33 <- glmmTMB(cases_pop ~ stdev + age + area + revenue_capita + 
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~stdev + age + area + revenue_capita, family = beta_family)
summary(Model33)


Model34 <- glmmTMB(cases_pop ~ forest + age + area + revenue_capita +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~forest + age + area + revenue_capita, family = beta_family)
summary(Model34)


Model35 <- glmmTMB(cases_pop ~ grassland + age + area + revenue_capita +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~grassland + age + area + revenue_capita, family = beta_family)
summary(Model35)



        #### Spatial Scale: Municipal Ward; Response Variable: Number of infections per surface area (Family = negative binomial)

Model36 <- glmmTMB(cases_area ~ max + age + pop + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model36)


Model37 <- glmmTMB(cases_area ~ mean + age + pop + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model37)


Model38 <- glmmTMB(cases_area ~ median + age + pop + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model38)


Model39 <- glmmTMB(cases_area ~ range + age + pop + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model39)


Model40 <- glmmTMB(cases_area ~ stdev + age + pop + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model40)


Model41 <- glmmTMB(cases_area ~ forest + age + pop  + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model41)


Model42 <- glmmTMB(cases_area ~ grassland + age + pop + revenue_capita +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
summary(Model42)



############### Modeling Greenness and COVID-19 Hospitalisation
severity <- read.table("C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev  Mar 2023\\Results\\Data Curation\\R Datasets\\Hosp_Dist.txt", header = TRUE)
names(severity)
attach(severity)

library(glmmTMB)


        ####Spatial Scale: District Municipality; Response Variable: Number of Hospitalisations (Family = negative binomial)

Model43 <- glmmTMB(hosp ~ max + age + pop + area +  revenue_capita +
                    (1|province), data = severity, family = nbinom2)
summary(Model43)


Model44 <- glmmTMB(hosp ~ mean + age + pop + area +  revenue_capita +
                    (1|province), data = severity, family = nbinom2)
summary(Model44)


Model45 <- glmmTMB(hosp ~ median + age + pop + area +  revenue_capita +
                    (1|province), data = severity, family = nbinom2)
summary(Model45)


Model46 <- glmmTMB(hosp ~ range + age + pop + area +  revenue_capita +
                    (1|province), data = severity, family = nbinom2)
summary(Model46)


Model47 <- glmmTMB(hosp ~ stdev + age + pop + area +  revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model47)


Model48 <- glmmTMB(hospital ~ forest + age + pop + area +  revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model48)


Model49 <- glmmTMB(hospital ~ grassland + age + pop + area +  revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model49)



        #### Spatial Scale: District Municipality; Response Variable: Number of cases per population (Family = Beta)

Model50 <- glmmTMB(hosp_pop ~ max + age + area + revenue_capita + 
                     (1|province), data = severity, family = beta_family)
summary(Model50)


Model51 <- glmmTMB(hosp_pop ~ mean + age + area + revenue_capita +
                     (1|province), data = severity, family = beta_family)
summary(Model51)


Model52 <- glmmTMB(hosp_pop ~ median + age + area + revenue_capita +
                     (1|province), data = severity, family = beta_family)
summary(Model52)


Model53 <- glmmTMB(hosp_pop ~ range + age + area + revenue_capita +
                     (1|province), data = severity, family = beta_family)
summary(Model53)


Model54 <- glmmTMB(hosp_pop ~ stdev + age + area + revenue_capita + 
                     (1|province), data = severity, family = beta_family)
summary(Model54)


Model55 <- glmmTMB(hospital_pop ~ forest + age + area + revenue_capita + 
                     (1|province), data = severity, family = beta_family)
summary(Model55)


Model56 <- glmmTMB(hospital_pop ~ grassland + age + area + revenue_capita +
                     (1|province), data = severity, family = beta_family)
summary(Model56)



        ### Response Variable: Number of cases per surface area (Family = negative binomial)

Model57 <- glmmTMB(hosp_area ~ max + age + pop + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model57)


Model58 <- glmmTMB(hosp_area ~ mean + age + pop + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model58)


Model59 <- glmmTMB(hosp_area ~ median + age + pop + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model59)


Model60 <- glmmTMB(hosp_area ~ range + age + pop + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model60)


Model61 <- glmmTMB(hosp_area ~ stdev + age + pop + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model61)


Model62 <- glmmTMB(hospital_area ~ forest + age + pop  + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model62)


Model63 <- glmmTMB(hospital_area ~ grassland + age + pop + revenue_capita +
                     (1|province), data = severity, family = nbinom2)
summary(Model63)
