# install package
install.packages("ivreg", dependencies = TRUE)

# read the dataset and the first 8th columns
data("SchoolingReturns", package = "ivreg")
summary(SchoolingReturns[, 1:8])

# OLS model: log(wage) = education + experience^2 + ethnicity + smsa + south
m_ols <- lm(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south,
            data = SchoolingReturns)
summary(m_ols)

# IV model: instrumental variables are nearcollege and age^2, ethnicity, smsa and south
library("ivreg")
m_iv <- ivreg(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south |
                nearcollege + poly(age, 2) + ethnicity + smsa + south,
              data = SchoolingReturns)

# Same as above, specified slightly more concisely:
# exogenous variables | the endogenous variables | the additional instrumental variables only
m_iv <- ivreg(log(wage) ~ ethnicity + smsa + south | education + poly(experience, 2) |
                nearcollege + poly(age, 2), data = SchoolingReturns)
summary(m_iv)

# Present OLS and IV
library("modelsummary")
m_list <- list(OLS = m_ols, IV = m_iv)
msummary(m_list)

# Present coefficients and associated standard errors by plot,
# omit the intercept and experience terms.
modelplot(m_list, coef_omit = "Intercept|experience")