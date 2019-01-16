######################################################
########## Federal University of Pernambuco ##########
############ Political Science Department ############
######### Masters Degree in Political Science ########
######################################################
###### Paper for the discipline of data analysis #####
######################################################
############### Professor Davi Moreira ###############
######## Student: Stephanie Moura de Oliveira ########
######################################################
###################### Title: ########################
#### Is welfare state the priority? Refugees flow ####
##### through Europe and their target countries ######
######################################################

### Commented Script ###

# Open Packages

if(require(tidyverse) == F) install.packages('tidyverse'); require (tidyverse)
if(require(readxl) == F) install.packages('readxl'); require (readxl)
if(require(readr) == F) install.packages('readr'); require (readr)
if(require(ggplot2) == F) install.packages('ggplot2'); require (ggplot2)
if(require(stargazer) == F) install.packages('stargazer'); require (stargazer)

# Open Databases


library(readr)
base <- read_delim("Dados/Dados tratados/dataset-stephanie-preprocess.csv", 
                                           ";", escape_double = FALSE, trim_ws = TRUE)

# The selected variables were: 

  ##### As an independent variable:
      ### Applied, from the "unhcr"database, demonstrating the number of resettlement requests
# each year for each country on the analysis.

  ##### As dependent variables:
      ### ffp_ps - public services 
# (which using various algorithms, this variable is converted into a score representing 
# the significance of the public  services for each country. The smaller, the better);
      ### bs_ee - Equitable Education
# (The qualitative indicators reflect the evaluations provided by more than 100 experts 
# responding to the SGI’s survey of the state of affairs in various policy areas throughout 
# the OECD and EU. For these indicators, the rating scale ranges from 1 (worst) to 10 (best).);
       ### bs_h - Health
# (The qualitative indicators reflect the evaluations provided by more than 100 experts 
# responding to the SGI’s survey of the state of affairs in various policy areas throughout 
# the OECD and EU. For these indicators, the rating scale ranges from 1 (worst) to 10 (best).);
        ### fh_pr - Political rights (as a control variable)
# The specific list of rights considered varies over the years. Countries are graded between 
# 1 (most free) and 7 (least free).


# Make the summary of the variable in descriptive statistics and the normal histogram showing that most
# of cases is left, near zero, with some outliers.

summary(base$Applied)
hist(base$Applied)

# Tests with the logarithmic transformation: we notice that the distribution is almost normal.
# The log is used, once it's assumed that the distribution of the variable has a bias, therefore,
# one of the extremities has a long tail, and once it's measured as correlation or regression, it
# can be greatly influenced by peak distribution, outliers, among others. The transformation can 
# reduce the bias effect.

# log analysis

summary(log(base$Applied))
hist(log(base$Applied))

# transformation to log - creation of the variable in log

base$LogApplied <- log(base$Applied)
base$LogApplied[base$LogApplied == -Inf] <- NA

# descriptive summary of the chosen variables

summary(base$bs_ee)
summary(base$bs_h)
summary(base$ffp_ps)
summary(base$fh_pr)

ggplot(data = base, aes(bs_ee)) +
  geom_histogram()+
  theme_minimal()

ggplot(data = base, aes(bs_h)) +
  geom_histogram()+
  theme_minimal()

ggplot(data = base, aes(ffp_ps)) +
  geom_histogram()+
  theme_minimal()

ggplot(data = base, aes(fh_pr)) +
  geom_histogram()+
  theme_minimal()

ggplot(data = base, aes(LogApplied)) +
  geom_histogram()+
  theme_minimal()

# Creating separate databases by year

base2011 <- filter(base, year == 2011)
base2015 <- filter(base, year == 2015)

# Creating a correlation between the LogApplied and bs_h using pairwise

cor(base$bs_h, base$LogApplied, use = "p")

# The "fh_pr" variable is used as control because it is an indicator of democracy and
# of the country's development then wants to see the effect of public services controlling for
# this variable of political development of the country. The effect of not including it can be
# an overestimation of the effect of the other variable.

# It is going to be used linear regression models, process and present the results (summary)

# the linear regressions were made separately with the objective of not reducing too much the cases
# since the bs_ee and bs_h variables are present in different countries than the "ffp_ps" and "fh_pr" 

# there are four linear regression models, one for the political rights and public services variables
# in 2011, one for these variables in 2015, and one for the equitable education, health and political 
# rights variables in 2011 and other for these variables in 2015.

model1 <- lm(LogApplied ~ ffp_ps + fh_pr, data = base2011)
model2 <- lm(LogApplied ~ bs_ee + bs_h + fh_pr, data = base2011)
model3 <- lm(LogApplied ~ ffp_ps + fh_pr, data = base2015)
model4 <- lm(LogApplied ~ bs_ee + bs_h + fh_pr, data = base2015)

# Exploratory analysis of the data used in this paper.

summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Code to produce a table with the regression results
# Will be a table in a text type, with the title "Model Results", in an American 
# Journal of Political Science Style, including p-values

stargazer(model1, model2, model3, model4,
  type = "text",
  title = "Model Results",
  style = "ajps",
  p.auto = FALSE)

# dispersion graphics indicating the relationship between the variables and add regression line

# model1
ggplot(base2011, aes(x = ffp_ps, y = LogApplied)) + geom_point() + 
  labs( x ="Public Services", y = "Number of refuge requests") +
  geom_smooth(method="lm", se = F) +
  theme_minimal()

# model2
ggplot(base2011, aes(x = bs_ee + bs_h, y = LogApplied)) + geom_point() +
  labs( x ="Health and Equitable Education", y = "Number of refuge requests") +
  geom_smooth(method="lm", se = F) +
  theme_minimal()

# model 3
ggplot(base2015, aes(x = ffp_ps, y = LogApplied)) + geom_point() + 
  labs( x ="Public Services", y = "Number of refuge requests") +
  geom_smooth(method="lm", se = F) +
  theme_minimal()

# model 4
ggplot(base2015, aes(x = bs_ee + bs_h, y = LogApplied)) + geom_point() + 
  labs( x ="Health and Equitable Education", y = "Number of refuge requests") +
  geom_smooth(method="lm", se = F) +
  theme_minimal()

# homocedasticity: since there is no pattern visible in the dispersion of the error,
# then they are homocedastic

ggplot(data = model1, aes(x = fitted(model1), y=residuals(model1))) +
  geom_point() + 
  labs( x ="fitted (model1)", y = "residuals(model1)") +
  geom_hline (yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal()

ggplot(data = model2, aes(x = fitted(model2), y=residuals(model2))) +
  geom_point() + 
  labs( x ="fitted (model2)", y = "residuals(model2)") +
  geom_hline (yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal()

ggplot(data = model3, aes(x = fitted(model3), y=residuals(model3))) +
  geom_point() + 
  labs( x ="fitted (model3)", y = "residuals(model3)") +
  geom_hline (yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal()

ggplot(data = model4, aes(x = fitted(model4), y=residuals(model4))) +
  geom_point() + 
  labs( x ="fitted (model4)", y = "residuals(model4)") +
  geom_hline (yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal() 

