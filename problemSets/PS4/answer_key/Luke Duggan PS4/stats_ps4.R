# Read in data.

library(eha)
library(survival)

data <- infants
attach(data)

head(data)

# Fit  Cox Proportional Hazards model.

model <- coxph(Surv(enter, exit, event) ~ age + sex)
summary(model)

# Likelihood ratio test.

drop1(model, test = "Chisq")

