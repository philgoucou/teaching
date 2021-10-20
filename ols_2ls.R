#------------------------------------------------------------------------------------
#---------------------------------- OLS regression  and 2OLS in R--------------------
#------------------------------------------------------------------------------------


#----------------------------------------simple OLS----------------------------------
install.packages("wooldridge")
library(wooldridge)
library(knitr)
#-----------------------
#Example 2.10 Wooldridge (page 70/912)
#Base de donn?es wage1
?wage1
#Estimation of  log of wage (lwage) and education (educ).
(log_wage_model <- lm(lwage ~ educ, data = wage1))
summary(log_wage_model)

#-----------------------
#Example 3.2 Wooldridge:  Hourly Wage Equation
#Base de donn?es wage1
?wage1
#Estimate the model regressing educ, exper, and tenure against log(wage).
hourly_wage_model <- lm(lwage ~ educ + exper +expersq+ tenure+construc, data = wage1)
#summary(log_wage_model)
summary(hourly_wage_model)

coefficients(hourly_wage_model) #afficher uniquement les coefficients de l'estimation

#-----------------------
#The built-in mtcars dataset in R is used 
#to visualise the bivariate relationship between 
#fuel efficiency (mpg) and engine displacement (disp)
library(dplyr)
library(ggplot2)
#Base de donn?es mtcars
?mtcars

#Repr?senter le nuage de poinds avec l'ajustement de Mayer
ggplot(mtcars, aes(x = disp, y = mpg)) +
  geom_point(colour = "blue")

#Calculate linear correlation
cor(mtcars$disp, mtcars$mpg)

#loglinear of the variables
ggplot(mtcars, aes(x = log(disp), y = log(mpg))) +
  geom_point(colour = "red") 

#calculer ? nouveau la corr?lation avec les variables lin?aris?es
cor(log(mtcars$disp), log(mtcars$mpg))

#An OLS linear model is now fit to the transformed data
ggplot(mtcars,aes(x = log(disp), y = log(mpg))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm")

#----------------
#Creation of the model
lmodel <- lm(log(mpg) ~ log(disp), data = mtcars)
summary(lmodel)

#----------------------------2OLS ---------------------------------------
#----------------
#2OLS in 2 steps
#Base de donn?es : mtcars

#La variable carb est utilis?e comme instrument pour disp
# First OLS : repression de disp sur carb
lm1=lm(disp~carb, data = mtcars)
summary(lm1)
coeff=coefficients(lm1) #estimated coefficients
disp_hat=mtcars$carb*coeff[2]+coeff[1] #estimated values for disp

# Second OLS : repression de mpg sur disp_hat (sur les estim?es de disp)
lm2=lm(log(mtcars$mpg)~disp_hat)
summary(lm2)


#-----------------------------2OLS with ivreg----------------------------
install.packages("ivreg", dependencies = TRUE)
library("ivreg")

#Example 1: with mtcars
#Estimons le mod?le lm2 avec la fonction ivreg
lm2_ivreg=ivreg(log(mpg)~disp |carb, data=mtcars)
summary(lm2_ivreg)

#comparer lm2 et lm2_ivreg
tableau_comparaison <- list(OLS_2steps = lm2, IV_regression = lm2_ivreg )

#Afficher le tableau
install.packages("modelsummary")
library(modelsummary)
msummary(tableau_comparaison)

#------------------------------------------------------------------------
#Example with SchoolingReturns data
#data("SchoolingReturns", package = "ivreg")
?SchoolingReturns
summary(SchoolingReturns[, 1:8])

#------------------------
#simple OLS
m_ols <- lm(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south,
            data = SchoolingReturns)

summary(m_ols)
#Les variables exog?nes  : ethnicity, smsa, south
#Les variables endog?nes : education, experience
#Instruments             : nearcollege, age


#------------------------
#ivreg
m_iv <- ivreg(log(wage) ~ ethnicity + smsa + south | education + poly(experience, 2) |
                nearcollege + poly(age, 2), data = SchoolingReturns)
m_iv
m_list <- list(OLS = m_ols, IV = m_iv)
msummary(m_list)

