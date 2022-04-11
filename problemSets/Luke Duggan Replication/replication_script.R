### Read in data
data <- read.csv("./fair1978.csv")
head(data)

### Estimate a Tobit model.
library(AER)
model <- tobit(y ~ sex + age + years_married + children + how_religious +
                 education + occupation + rate_marriage, data = data,
               left = 0)
summary(model)

### Estimate a Heckman model.
library(sampleSelection)

# We have to define a binary outcome variable for the selection equation.

thing <- numeric(601)
for (i in 1:length(data$y)) {
  if (data$y[i] > 0) {
    thing[i] <- 1
  }
  if (data$y[i] == 0) {
    thing[i] <- 0
  }
}

# Then estimate the model.

model2 <- heckit(selection = thing ~ sex + age + years_married + children + how_religious +
                   education + occupation + rate_marriage,
                 outcome = y ~ sex + age + years_married + children + how_religious +
                   education + occupation + rate_marriage,
                 data = data)
summary(model2)
