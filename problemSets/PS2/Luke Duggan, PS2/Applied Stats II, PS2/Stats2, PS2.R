##################################
# APPLIED STATS II - PROBLEM SET 2
# Luke Duggan - 27/02/'21
##################################

### QUESTION ONE
# First we read in the data.
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

summary(climateSupport)

# Next, we fit an additive model. 

results <???glm(choice ~ countries + sanctions, data= climateSupport, 
              family=binomial(link="logit"))
summary(results)

countries_unordered <- factor(climateSupport$countries, ordered = FALSE )
sanctions_unordered <- factor(climateSupport$sanctions, ordered = FALSE )

results2 <???glm(choice ~ countries_unordered + sanctions_unordered, 
               data= climateSupport, family=binomial(link="logit"))
summary(results2)

# To test the global null hypothesis, we create a reduced model and perform
# a likelihood ratio test:

reduced <- glm(choice ~ 1, data= climateSupport, 
                         family=binomial(link="logit"))
summary(reduced)

anova(reduced, results2, test = "Chisq")

### QUESTION TWO:

# We alter the original model to include an interaction term: 

results3 <???glm(choice ~ countries_unordered + sanctions_unordered 
               + countries_unordered*sanctions_unordered, 
               data= climateSupport, family=binomial(link="logit"))
summary(results3)
