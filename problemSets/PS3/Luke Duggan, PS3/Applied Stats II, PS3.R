#############################################
##### QUESTION ONE.
##############################################

# Part One.

# Read in data.

library(nnet)
library(MASS)

data <- read.csv("./gdpChange.csv")
attach(data)

# Create categorical variable.

for (i in 1:length(GDPWdiff)) {
  
  if (GDPWdiff[i] > 0) {
    data$GDPchange[i] = "increase"
  }
  
  if (GDPWdiff[i] < 0) {
    data$GDPchange[i] = "decrease"
  }
  
  if (GDPWdiff[i] == 0) {
    data$GDPchange[i] = "no_change"
  }
}

data$GDPchange <- factor(data$GDPchange, levels = c("no_change", "increase",
                                                    "decrease"))
# Fit multinomial model.

multinom_model1 <- multinom(data$GDPchange ~ REG + OIL)

summary(multinom_model1)

# Estimate cutoff points.

thing <- predict(multinom_model1, type="probs")

mean(thing[,2])
mean(thing[,3])

# Part Two.

# Fit model.

ordered_logit <- polr(data$GDPchange ~ REG + OIL, Hess=T)
summary(ordered_logit)

# Estimate cutoffs.

thing2 <- predict(ordered_logit, type="probs")

mean(thing[,2])
mean(thing[,3])

detach(data)

##########################################
##### QUESTION TWO.
##########################################

data2 <- read.csv("./MexicoMuniData.csv")
head(data2)
attach(data2)

# Run a Poisson regression.
model2 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 +
                PAN.governor.06, family = poisson)

summary(model2)

# Fitted value.

exp(model2$coef[1] + model2$coef[2]*1 + model2$coef[3]*0 + 
                model2$coef[4]*1)

