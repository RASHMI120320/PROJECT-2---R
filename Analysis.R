#ANALYSIS 
#Conducting a pairwise t-test to check how mean test_scores of 2 groups specified by summerschool are different 

# Filter year 5
df<-analysisdata%>%filter(year==5)
# Conduct a t test
t.test(test_score~summerschool,data=df)

#Creating a balancing table
# Load libraries
library(modelsummary)
library(estimatr)
# Filter and modify data
testdata<-filter(analysisdata,year==5)
testdata<-ungroup(testdata)
testdata<-mutate(testdata,Treated=ifelse(letter==1,"Letter","No Letter"))
testdata<-select(testdata,female,parental_schooling,parental_lincome,test_score,Treated)
testdata<-rename(testdata,`Female`=female,
                 `Parental schooling (years)`=parental_schooling,
                 `Parental income (log)`=parental_lincome,
                 `Test Score`=test_score)

# Table with balancing test
datasummary_balance(~Treated,
                    data = testdata,
                    title = "Balance of pre-treatment variables",
                    notes = "Notes: This is a brilliant table!",
                    fmt= '%.5f',
                    dinm_statistic = "p.value")


#OLS ESTIMATION

#  Ordinary Least Squares regression
model1<-lm(test_score~parental_schooling+parental_lincome+letter+female,data=filter(analysisdata,year==6))
# Summary of model1
summary(model1)


install.packages("fixest")
library("fixest")
# Select data
regdata<-filter(analysisdata,year==6)
# Regression
m1<-feols(test_score~letter+parental_lincome+female+parental_schooling, cluster="school_id",data=regdata)
# Summary of regression
summary(m1)
# Load flextable and modelsummary
library(modelsummary)
# Select data
regdata<-filter(analysisdata,year==6)
# Regressions
models<-list(
  m1<-feols(test_score~letter+parental_lincome+female+parental_schooling, cluster="school_id",data=regdata),
  m2<-feols(test_score~letter+parental_lincome+female+parental_schooling|school_id+year, cluster="school_id",data=regdata)
)
# Generate table
modelsummary(models, stars = TRUE,statistic = 'std.error',
             fmt= '%.4f',
             coef_omit= '(Intercept)')


#We want to explore the link between receiving a letter and attending the summer school
# Estimate LPM (the first stage)
models<-list(
  m1<-feols(summerschool ~letter, cluster="school_id",data=regdata),
  m2<-feols(summerschool ~letter+parental_schooling+parental_lincome+female,cluster="school_id",data=regdata)
)
# Store the mean of dependent variable in a data frame
added_stats<-tibble("Mean of Dep. ",m1=mean(regdata$summerschool),m2=mean(regdata$summerschool))
# Generate table
modelsummary(models, stars = TRUE,statistic = 'std.error',  
             fmt= '%.4f',add_rows = added_stats,
             coef_omit= '(Intercept)', output = 'flextable')


#  Estimate a binary outcomes model using a probit
probit_results <- glm(summerschool ~letter, data = regdata, family = binomial(link="probit"))
# Print the results
summary(probit_results)

#Calculating marginal effects

install.packages("margins")
# Load margins package
library("margins")
# Select data
regdata<-filter(analysisdata,year==6)
##  Estimate a binary outcomes model using a logit and probit
logit_results <- glm(summerschool ~letter+parental_lincome+female+parental_schooling, data = regdata, family = binomial(link="logit"))
probit_results <- glm(summerschool ~letter+parental_lincome+female+parental_schooling, data = regdata, family = binomial(link="probit"))
# Estimate linear probability model
lpm<-feols(summerschool ~letter+parental_schooling+parental_lincome+female, cluster="school_id",data=regdata)
#  Compute marginal effects
mfx_logit<- margins(logit_results)
mfx_probit<- margins(probit_results)
# Include in table
modelsummary(list("LPM"=lpm,"Logit"=mfx_logit,"Probit"=mfx_probit))

#Instrumental Variables
# Estimate IV specification with felm 
m1<-feols(test_score~parental_lincome+female+parental_schooling| # Outcome eq.
            0|                                                    # Fixed effects
            summerschool~letter                                   # First stage
          ,cluster="school_id"                                  # Cluster var
          ,data=regdata)
# Summary of results
summary(m1)

#Creating an IV table
# Estimate OLS
OLS<-feols(test_score~summerschool+parental_lincome+female+parental_schooling,cluster="school_id",data=regdata)
# Estimate reduced form 
RF<-feols(test_score~letter+parental_lincome+female+parental_schooling,cluster="school_id",data=regdata)
# Estimate first stage 
FS<-feols(summerschool~letter+parental_lincome+female+parental_schooling,cluster="school_id",data=regdata)
# Estimate IV specification 
IV<-feols(test_score~parental_lincome+female+parental_schooling| # Outcome eq.
            0|                                                    # Fixed effects
            summerschool~letter                                   # First stage
          ,cluster="school_id"                                  # Cluster var
          ,data=regdata)
# Combine results
IVresults<-list("OLS"=OLS,"RF"=RF,"FS"=FS,"IV"=IV)
# Coefficients
cm <- c('fit_summerschool' = 'Summer School',
        'summerschool' = 'Summer School','parental_lincome' = 'Parental Income', 
        'letter' = 'Reminder letter', "female"="Female","parental_schooling"="Parental Schooling")
# Output Table
modelsummary(IVresults, stars = TRUE,statistic = 'std.error',  
             fmt= '%.4f',coef_map=cm, output =      )
