# Homework 1
library(readr)
library(ggplot2)
library(dplyr)
library(broom)
bmi_study <- read_csv("~/Desktop/Magisterka/Statistical modelling in biology/bmi_study.csv", 
                      +     na = "NA")

# Checking if data has missing values 
sum(is.na(bmi_study)) # there isn't any missing values

# Summary statistics
summary(bmi_study)

# Check data structure
str(bmi_study)

# The infection and complication varaibles which we are interested in are binary values
# this is good for as they are our outcome variables

#Check the proportion (distribution) of binary values (if they are rare or not)
table(bmi_study$infection)
prop.table(table(bmi_study$infection))  # Proportions

table(bmi_study$complication)
prop.table(table(bmi_study$complication)) 
# Both are rare - that states it will better to use Complementary Log-Log function as a link function

# Now, we will develope GLM Models for infections and complications.
# Since these are binary variables (so we predict if event occures or not)
# we will use for it binomial family, with link function stated above
glm_infection <- glm(infection ~ . - complication, data = bmi_study,
                     family = binomial(link = "cloglog"))
glm_complication <- glm(complication ~ . - infection, data = bmi_study,
                       family = binomial(link = "cloglog"))

summary(glm_infection) # important covariates: sex, smoking, hospstay
summary(glm_complication) # important covariates: operations: operationlung removed, operationtwo lobes removed

# now let's fit models with these covariates and BMI as BMI is important for us
glm_infection2 <- glm(infection ~ sex + smoking + hospstay + bmi, data = bmi_study, 
                      family = binomial(link = "cloglog"))
glm_comlication2 <- glm(complication ~ operation + bmi,
                        data = bmi_study, family = binomial(link = "cloglog"))

summary(glm_infection2)
summary(glm_comlication2)
# still BMI has no statistical importants on predicting infection and complication

# For the infection model, get the hazard ratios and confidence intervals
exp(cbind(HR = coef(glm_infection2), confint(glm_infection2)))

# Males have 2.44 times higher risk of infection than females.
# Smokers have a 5.16 times higher risk of infection than non-smokers.
# For each additional day of hospital stay, the risk of infection increases by 2%.
# Higher BMI is associated with a slightly lower risk of infection, but this effect is marginal (HR < 1), 
# p-value = 0.05302 and the CI is close to 1.

# For the complication model, get the hazard ratios and confidence intervals
exp(cbind(HR = coef(glm_comlication2), confint(glm_comlication2)))

# Lung removal surgery significantly increases the risk of complications (about 4.65 times higher risk).
# Other surgeries do not significantly increase the risk of complications,
# p-value = 0.608730 and CI is close to 1
# Removal of two lobes increases the risk of complications by 3.83 times.
# BMI is not significantly associated with complications, as the HR is close to 1 and
# p-value = 0.151212 and the CI is close to 1.

# Visualizing HRs with confidence intervals for the infection model
infection_summary <- tidy(glm_infection2, conf.int = TRUE, exponentiate = TRUE)

# Visualizing HRs with confidence intervals for the complication model
complication_summary <- tidy(glm_comlication2, conf.int = TRUE, exponentiate = TRUE)

# Plot HR for infection
ggplot(infection_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Hazard Ratios with 95% Confidence Intervals for Infection",
       x = "Predictors",
       y = "Hazard Ratio") +
  theme_minimal()

# Plot HR for complications
ggplot(complication_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Hazard Ratios with 95% Confidence Intervals for Complications",
       x = "Predictors",
       y = "Hazard Ratio") +
  theme_minimal()


# For future visualisations:
# Predict probabilities for infection model
bmi_study$infection_prob <- predict(glm_infection2, type = "response")


# Predict probabilities for complication model
bmi_study$complication_prob <- predict(glm_comlication2, type = "response")

bmi_study$lower_lungremoved <- 0.04342597
bmi_study$upper_lungremoved <- 0.3659666

bmi_study$lower_lobesremoved <- 1.78902849
bmi_study$upper_lobesremoved <- 7.2249939

bmi_study$lower_sexM <- 1.177509125
bmi_study$upper_sexM <- 5.5794539

bmi_study$lower_smoking <- 1.550911495
bmi_study$upper_smoking <- 32.0074817

bmi_study$lower_bmi_infection <- 0.852122152
bmi_study$upper_bmi_infection <-  0.9991341

bmi_study$lower_bmi_complications <- 0.93373765
bmi_study$upper_bmi_complications <- 1.0099919

bmi_study$lower_hospstay <- 1.007893107
bmi_study$upper_hospstay<- 1.0242511





