# Greener neighbourhoods show resilience to the spread but not severity of COVID-19 infection in South Africa 

rm(list=ls()) #To clear memory history 

file.choose()

## Section 1.  Test for multicollinearity between the three predictor variables: age, revenue, and population density. Three tests are conducted at three separate spatial scales: local municipality, Ward, and district level.

library(MASS)

library(car)

### Section 1.1.  VIF test at District Level (COVID-19 hospitalisations).

Data_1 <- read.table("C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev May 2023\\R 

Analysis\\vifHosp.txt", header = T)

names(Data_1)

Model_1V <- glm(hospital_pop~age+rev_capita+pop_dens, data = Data_1)

vif(Model_1V)


### Section 1.2. VIF test at Local municipality level (COVID-19 infection).

Data_2 <- read.table("C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev May 2023\\R Analysis\\vifLoc.txt", header = T)

names(vifLoc)

Model_2V <- glm(cases_pop~revenue_capita+age+pop_dens, data = Data_2)

vif(Model_2V)

#### After removing "age" to reduce collinearity.
Model_2Vi <- glm(cases_pop~revenue_capita+pop_dens, data = Data_2)

vif(Model_2Vi)


### Section 1.2.  VIF test at Municipal Ward (COVID-19 infection)
Data_3 <- read.table("C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev May 2023\\R Analysis\\vifWard.txt", header = T)

names(vifWard)

Model_3V <- glm(cases_pop~age+rev_capita+pop_dens, data = Data_3)

vif(Model_3V)


## Section 2. In this section, we model greenness and COVID-19 infection at Local Municipality and Ward scale. COVID-19 is measured in three way: count number of infections, number of infections per population, and number of infections per surface area. Greenness is measured as enhanced vegetation index (EVI), grassland cover, and forest cover. The three measures of greenness are each tested at two separate levels: Local municipality scale and Ward scale.

### Section 2.1. In this subsection, we focus on the local municipality scale.

prevalence <- read.table( "C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev May 2023\\R Analysis\\Used Texts\\Local_Mun.txt", header = TRUE)

attach(prevalence)

head(prevalence)

library(glmmTMB)


#### Section 2.1.1.  For Spatial Scale: Local Municipality; Response Variable: Number of infections (Family = negative binomial)

Model1 <- glmmTMB(cases ~ mean + revenue_capita + pop_dens +
                    (1|province/district), data = prevalence, family = nbinom2)
                    
summary(Model1)


Model2 <- glmmTMB(cases ~ grassland + revenue_capita + pop_dens +
                    (1|province/district), data = prevalence, family = nbinom2)
                    
summary(Model2)


Model3 <- glmmTMB(cases ~ forest + revenue_capita + pop_dens +
                    (1|province/district), data = prevalence, family = nbinom2)
                    
summary(Model3)


#### Section 2.1.2. For Spatial scale: Local Municipality; Response Variable: Number of infections per population (Family = Beta)

Model4 <- glmmTMB(cases_pop ~ mean + revenue_capita + pop_dens +
                    (1|province/district), data = prevalence, family = beta_family)
                    
summary(Model4)


Model5 <- glmmTMB(cases_pop ~ grassland + revenue_capita + pop_dens +
                    (1|province/district), data = prevalence, family = beta_family)
                    
summary(Model5)


Model6 <- glmmTMB(cases_pop ~ forest + revenue_capita + pop_dens +
                     (1|province/district), data = prevalence, family = beta_family)
                     
summary(Model6)

####  Section 2.1.3. For Spatial Scale: Local Municipality; Response Variable: Number of cases per surface area (Family = negative binomial)

Model7 <- glmmTMB(cases_area ~ mean + revenue_capita + pop_dens +
                     (1|province/district), data = prevalence, family = nbinom2)
                     
summary(Model7)


Model8 <- glmmTMB(cases_area ~ grassland + revenue_capita + pop_dens +
                     (1|province/district), data = prevalence, family = nbinom2)
                     
summary(Model8)


Model9 <- glmmTMB(cases_area ~ forest + revenue_capita + pop_dens +
                     (1|province/district), data = prevalence, family = nbinom2)
                     
summary(Model9)



### Section 2.2. In this section,we model greenness and COVID-19 infection at Ward scale.

prevalence_2 <- read.table("C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev May 2023\\R Analysis\\Used Texts\\Mun_Ward.txt", header = TRUE)

attach(prevalence_2)

head(prevalence_2)

library(glmmTMB)

#### Section 2.2.1. For Spatial Scale: Municipal Ward; Response Variable: Number of infections (Family = negative binomial)

Model10 <- glmmTMB(cases ~ mean + age + revenue_capita + pop_dens+ 
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
                     
summary(Model10)


Model11 <- glmmTMB(cases ~ grassland + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
                     
summary(Model11)


Model12 <- glmmTMB(cases ~ forest + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
                     
summary(Model12)

#### Section 2.2.2. For Spatial Scale: Municipal Ward; Response Variable: Number of infections per population (Family = Beta)

Model13 <- glmmTMB(cases_pop ~ mean + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~mean + age + pop_dens + revenue_capita, family = beta_family)
                   
summary(Model13)


Model14 <- glmmTMB(cases_pop ~ grassland + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~grassland + age + pop_dens + revenue_capita, family = beta_family)
                   
summary(Model14)


Model15 <- glmmTMB(cases_pop ~ forest + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2,
                   ziformula = ~forest + age + pop_dens + revenue_capita, family = beta_family)
                   
summary(Model15)



#### Section 2.2.3. For Spatial Scale: Municipal Ward; Response Variable: Number of infections per surface area (Family = negative binomial)

Model16 <- glmmTMB(cases_area ~ mean + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
                     
summary(Model16)


Model17 <- glmmTMB(cases_area ~ grassland + age + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
                     
summary(Model17)


Model18 <- glmmTMB(cases_area ~ forest + age  + revenue_capita + pop_dens +
                     (1|province/district/local), data = prevalence_2, family = nbinom2)
                     
summary(Model18)


## Section 3. In this section,we model greenness and COVID-19-related Hospitalizations at District level. COVID-19 hospitalisation is measured in three way: count number of hospitalisations, number of hospitalisations per population, and number of hospitalisations per surface area. Greenness is measured as enhanced vegetation index (EVI), grassland cover, and forest cover.

severity <- read.table( "C:\\Users\\pbopa\\OneDrive\\Documents\\University of Johannesburg\\Article 2_Covid19 and Greenery\\P III Rev May 2023\\R Analysis\\Used Texts\\Hosp_Dist.txt", header = TRUE)

names(severity)

attach(severity)

library(glmmTMB)

### Section 3.1.  For Spatial Scale: District Municipality; Response Variable: Number of Hospitalisations (Family = negative binomial)

Model19 <- glmmTMB(hosp ~ mean + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = nbinom2)
                     
summary(Model19)

Model20 <- glmmTMB(hosp ~ forest + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = nbinom2)
                     
summary(Model20)

Model21 <- glmmTMB(hosp ~ grassland + age +  revenue_capita + pop_dens +
                     (1|province), data = severity, family = nbinom2)
                     
summary(Model21)

### Section 3.2. For Spatial Scale: District Municipality; Response Variable: Number of cases per population (Family = Beta)

Model22 <- glmmTMB(hosp_pop ~ mean + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = beta_family)
                     
summary(Model22)


Model23 <- glmmTMB(hosp_pop ~ grassland + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = beta_family)
                     
summary(Model23)


Model24 <- glmmTMB(hosp_pop ~ forest + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = beta_family)
                     
summary(Model24) 


### Section 3.3. For Response Variable: Number of cases per surface area (Family = negative binomial)

Model25 <- glmmTMB(hosp_area ~ mean + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = nbinom2)
                     
summary(Model25)


Model26 <- glmmTMB(hosp_area ~ grassland + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = nbinom2)
                     
summary(Model26)


Model27 <- glmmTMB(hosp_area ~ forest + age + revenue_capita + pop_dens +
                     (1|province), data = severity, family = nbinom2)
                     
summary(Model27)




   
