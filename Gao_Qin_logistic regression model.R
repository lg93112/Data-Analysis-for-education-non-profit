# Logistic Regression Model in R for our project

# We intend to use 5 attributes to predict if a constituent is a donor or not based on the dataset we derived
# from SQL. We have age as a continuous attribute, gender as a binary attribute where 1 stands for MALE and 0 stands
# for FEMALE, married as a binary attribute where 1 stands for married person and 0 stands for unmarried person,
# events as a continuous variable where the value is the number of events a constituent attended, and volunteer
# as a binary attribute where 1 stands for a constituent is a volunteer or has relatives as volunteers and 0 vice 
# versa.

# We also have following assumptions for our attributes as introduced in class:

# Assumptions

# Linearity - There is a linear relationship between any continuous predictors and the logit
# of the variable outcome. Notice that here we only have two continuous predictor as age and events.

# Independence of Errors - Same as for ordinary regression. Cases of data should not be
# related.

# Multicollinearity - Predictors should not be too highly correlated. We can check these
# with VIF statistics.

# Firstly, we load our libraries and our data

install.packages("mlogit")

library(car)
library(mlogit)
library(readr)

# change the address in the bracket to the dataset address
donors <- read.table("Gao_Qin_dataset.txt", header = TRUE)
View(donors)

# =====================================================================

# Donors Model

# check head of our dataframe:
head(donors)
# build logistic regression model
model <- glm(donor ~ age + gender + married + events + volunteer,
              data = donors,
              family = binomial())

summary(model)

# Assessing the model

# -------------
# When building our model, we noticed a warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred
# This means that one or several of our predictors perfectly separates zeroes and ones in target variable,
# and thus causes perfect or quasi perfect separation. Thus, we need to modify our model
# After some observations and trials, we found that "events" is the attribute causing perfect separation.
# Attending events or not may directly decide if a constituent is donor or not, and thus we delete this
# attribute from our model. 

# Now, our model is revised as:
model <- glm(donor ~ age + gender + married + volunteer,
             data = donors,
             family = binomial())
summary(model)

# -------------
# From the coefficients table, we can also notice that Pr(>|z|) for gender is greatly larger than 0.05,
# which means that gender is not significant and thus we can remove it from our model

# So we derive our model as:
model <- glm(donor ~ age + married + volunteer,
             data = donors,
             family = binomial())
summary(model)
# This looks good for now, and we can do following assessment to evaluate our model

# Assessing the model
# The evaluations are based on the assessment introduced in class and Andy's book. 
# We follow the procedures discussed in Week 13's R script and made the evaluations to judge
# if our model is good or not to predict donors.

# -------------

# The Deviance Statistic

# The deviance statistic determines the overall fit of a logistic regression model.
# Larger values of the deviance statistic indicate badly fit models.

# We first look at the Null deviance which is 2772.6, this is the deviance of the model
# with no predictors. We want to see a decrease in size in the difference between the NULL
# and residual deviances. 

# In this case the Residual Deviance is 2506.2. The decrease in size indicates that 
# the model is better at predicitng whether someone was admitted after the predictor
# variables were added. 

# -------------

# Model Chi-Square Statistic

# The model chi-Square can be used to determine how much better it is at predicting an
# outcome with the predictors added than with only the constant. 

# To get the chi-square statistic we can subtract the deviance from the null deviance
# as follows:

model_chi <- model$null.deviance - model$deviance

model_chi

# We next need to get the difference in the degrees of freedom between the model and the
# null model:

model_chi_df <- model$df.null - model$df.residual 

model_chi_df

# finally, we need to calculate the probability associated with the Chi-Square Statistic:

model_chisq.prob <- 1 - pchisq(model_chi, model_chi_df)

model_chisq.prob

# We get a value of 0 - this probability is less than .05.
# A value of 0 and p value of 1 should mean that our model match the binominal distribution perfectly, 
# and therefore we can reject the null hypothesis that the model is not better than chance at predicting the outcome.
# However, we expect some value close to 0 but not exactly 0, which means perfectly fit. So we hold our doubts
# here that whether this 0 can be regarded as perfect or some problems with our models.

# -------------

# Coefficients

# The beta values for the coefficients can be found under the 'Estimate'
# column. 

# These represent the change in hte logit of the outcome variable associated with a one-unit
# change in the predictor variable - i.e. the natural logarithm of the odds of Y (donors)
# occuring. 

# To determine if a coefficient is signifcantly different from zero - meaning it is making
# a contribution to the model - it will be indicated in the Z value probability (Pr(>|z|).
# We have uses this check to remove gender from our model and we can see that now all three variable
# has Pr less than 0.05, and thus they are all significant.

# -------------

# Pseudo-R2

# In a logistic regression we don't have an R2 like we would in a linear or muli-variate
# Regression, there are however several analoguous tests that we can do to generate what
# are referred to as Pseudo-R2. 

# We used this function discussed in Andy's book and also in Week 13's script to calculate Pseudo-R2

logistic_r2 <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev /nullDev
  R.cs <- 1 - exp(-(nullDev - dev)/modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l,3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs,3), "\n")
  cat("Nagelkerke R^2          ", round(R.n,3), "\n")
}

logistic_r2(model)  

# As you can see, by all three measures the effect size of the model is small around 0.1 - 
# meaning we can't explain much of the variability in the outcome with just the
# variables we have. 

# -------------

# The Odds Ratio

# If the odds ratio is greater than 1 it means that as the predictor increases, the odds of the 
# outcome occuring increase.
# A value less than 1 means that as the predictor increases, the odds of the outcome occuring 
# decrease.

exp(model$coefficients)

# Here, we can see that for every 1 point increase in age, married or volunteer, the odds of a constituent 
# being a donor increase by 1.01, 3.93, and 1.69 times respectively.

# -------------

# Confidence Intervals

# Again, like with the other forms of regression we have looked at: 
# the improtant thing is that our ratios don't cross 1. 

exp(confint(model))

# -------------

# Checking Residuals

# It is important to check for outliers in residuals just like we did
# with linear regression:

donors$predicted_probabilities <- fitted(model) # Here we are getting the model predictions.
donors$standardized_residuals <- rstandard(model)
donors$studentized_residuals <- rstudent(model)
donors$dfbeta <- dfbeta(model)
donors$dffit <- dffits(model)
donors$leverage <- hatvalues(model)


View(donors)


# we want 95% of our cases to have standardized residuals within plus or minus 2, in a set of 2000 cases we'd expect 
# roughly 1900 to be around 2, or roughly 100 cases outside the limit. Let's see"

donors$standardized_residuals > 2 | donors$standardized_residuals < -2

# We want to store this data in our dataframe:

donors$large_residual <- donors$standardized_residuals > 2 | donors$standardized_residuals < -2

View(donors)

sum(donors$large_residual)

donors[donors$large_residual,c("donor","age","married","volunteer","standardized_residuals")]

# We have 0 residuals here, so we are good!

# -------------

# DFBeta should be less than 1.
# All our DFBeta are less than 1.

# -------------

# Testing for multicollinearity

# If the largest VIF is greater than 10 then there is a problem.
# If the average VIF is substantially greater than 1 then there may be bias in the model.
# If the the tolerence is below .1 that indicates a serious problem.
# Tolerence below .2 indicates a potential problem.

vif(model)
1/vif(model) # Tolerence
mean(vif(model)) # Average

# Here, we have VIF very close to 1, average VIF equal to 1, and tolerance all around 0.99
# It doesn't look like there is any collinearity in our model.

# -------------

# Linearity of the Logit

# First we need to create the variables we will use for the test:
# There is only one continuous variable now in our model as age, and we need to take log of it

donors$logage <- log(donors$age)*donors$age


View(donors)

model_test <- glm(donor ~ age + logage,
                   data = donors,
                   family = binomial())

summary(model_test)

# What we are looking for here is for all interactions to have Pr(>|z|) greater than .05
# In other words we don't want them to be significant.
# Here, we have all Pr larger thab 0.05, which shows linearity of logit for age.

# Now we have finished our assessment, I think overall our model is good.
# But we still hold doubts about some edge case here as:
# probability associated with the Chi-Square Statistic = 0 (p-value = 1) and no outliers in residuals (0)
# These two should mean perfection but we also wonder is this really a good thing or not?


