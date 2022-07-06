# install.packages('fastDummies')
library(readxl)
library(lmtest)
library(plm)
library(fastDummies)
library(car)

df <- read_excel("C:\\Ph.D\\solow.xlsx")
form <- lny ~ lnndg + lns

##### see the data #####
View(df)
View(df_d)

##### (1) ols model, with and without robust standard errors #####
Model_ols <- plm(form, data = df, model = "pooling", index = c("id","year"))
coeftest(Model_ols)
coeftest(Model_ols, vcov = vcovHC(Model_ols, type="HC1"))

##### (2) panel data fixed effects model, with and without robust standard errors #####
Model_fixed <- plm(form, data = df, model = "within")
coeftest(Model_fixed)
coeftest(Model_fixed, vcov = vcovHC(Model_fixed, type="HC1"))
# Display the fixed effects:
# fixef(Model_fixed)

##### ols model+country dummies v.s. fixed effects model #####
df_d_country <- dummy_cols(df, select_columns = 'country')
df_d_country <- df_d_country[,-1:-4]
Model_ols_country <- lm(lny ~ ., data = df_d_country)
Model_ols_country[["coefficients"]][2:3]
Model_fixed[["coefficients"]][1:2]
# coeftest(Model_ols_country)
# coeftest(Model_fixed)

##### Poolability test for check whether the individual dummies are statistically significant #####
# Testing for fixed effects, null: OLS better than fixed
pFtest(Model_fixed, Model_ols)

##### Test of the hyphosesis that c1=c2 for the pooled, fixed and random model #####
linearHypothesis(Model_ols, c("lns = lnndg"))
linearHypothesis(Model_fixed, c("lns = lnndg"))

##### The three models with heteroschedasticity consistent standard error #####
# type="HC1", equivalent to STATA
coeftest(Model_ols, vcov = vcovHC(Model_ols, type="HC1"))
coeftest(Model_fixed, vcov = vcovHC(Model_fixed, type="HC1"))


