# Intermediate Econometrics HW2

## Load Tidyverse library
library(tidyverse)

## Put data into a data frame
df = read_csv("data/IEU_68.csv")

summary(df)

## Add log(wage) and define dummy all variables
df = df %>%
  mutate(lwage = log(Wages)) %>%
  mutate(age_sq = Age^2) %>%
  mutate(male = ifelse(df$Sex == 1, 1, 0)) %>%
  mutate(currently_married = ifelse(df$Married == 2, 1, 0)) %>%
  mutate(ftw = ifelse(df$Full_time == 1, 1, 0)) %>%
  mutate(urban = ifelse(df$Urban ==1, 1, 0)) %>%
  mutate(union = ifelse(df$Union_member ==1, 1, 0)) %>%
  mutate(lower_caste = ifelse(df$Social_Group == 9, 0 ,1))

## Check that all variables were added correctly
view(df)

## Q1: Stability Test -----------
model1 = df %>% 
  filter(Informal == 1) %>% 
  lm(formula = lwage ~ male + Age + Education + age_sq)

model2 = df %>% 
  filter(Informal == 0) %>% 
  lm(formula = lwage ~ male + Age + Education + age_sq)

model3 = df %>%
  lm(formula = lwage ~ male + Age + Education + age_sq + Informal)

model_1_and_2_combined = df %>%
  lm(formula = lwage ~ male + Age + Education + age_sq)

### Look at the coefficients
coefficients(model1)
coefficients(model2)
coefficients(model3)

### Look at the confidence intervals 

confint(model1, level = 0.95)
confint(model2, level = 0.95)
confint(model3, level = 0.95)


### Stability Test: Does the same model apply for formal and informal workers? ###

output_m1 = summary(model1)
output_m2 = summary(model2)
output_m1_and_m2_combined = summary(model_1_and_2_combined)

### Pull and store the RSE of each model
sse_m1 = sum(model1$residuals^2)
sse_m2 = sum(model2$residuals^2)
sse_m1_and_m2_combined = sum(model_1_and_2_combined$residuals^2)

### Pull and store the number of observations in each model
n_m1 = dim(df %>% filter(Informal ==1))[1]
n_m2 = dim(df %>% filter(Informal ==0))[1]
n = dim(df)[1]

### 4 regressors and one intercept
k = 5

F_stability = ((sse_m1_and_m2_combined - sse_m2 - sse_m1)/k)/((sse_m2 + sse_m1) / (n - 2 * k))

F_stability 

#### Reject the null hypothesis that the model is the same for informal and formal workers

## Q2: New Model/Interpretation -----------

q2_model = df %>%
  lm(formula = lwage ~ Age + age_sq + Education + Informal + currently_married + urban + ftw + union)

summary(q2_model)

### The following returns the coefficients for Age, age_sq, and Education
### So, all else equal, the marginal effects of a one-year increase in age on wage is 2.69% - (0.0256% * 2 * Age)

coef(q2_model)[2:4]

### The marginal effect of a one-year increase in general education, all else equal, is a 4.26% increase in wage

## Q3: Adding low caste to the model ----------

q3_model = df %>%
  lm(formula = lwage ~ Age + age_sq + Education + Informal + currently_married + urban + ftw + union + lower_caste)

### Check the coefficients of Age, age_sq, Education, Informal, and lower_caste
coef(q3_model)[c(2:5, 10)]

### Are the coefficients of Age and Education **exactly** equal in the two models?
coef(q2_model)[c(2,4)] == coef(q3_model)[c(2,4)]

### Now we need to test if they are different at the 5% level

### Pull the standard errors for Age and Education from model 3
q3_se = coef(summary(q3_model))[c(2:4),2]
q3_var = q3_se^2

### Do the same for model 2

q2_se = coef(summary(q2_model))[c(2:4),2]
q2_var = q2_se^2


t_stat_age = (coef(q2_model)[2] - coef(q3_model)[2])/(sqrt(q3_var[1] + q2_var[1]))

### Is the t-stat for age greater than 1.96?
t_stat_age > 1.96

t_stat_age_sq = (coef(q2_model)[3] - coef(q3_model)[3])/(sqrt(q3_var[2] + q2_var[2]))

t_stat_age_sq
### No, it is less than 1.96: Fail to reject the null that the
### coefficient for Age in Model 2 is equal to the coefficient for Age in Model 3


### Let's run the same test for Education

t_stat_educ = (coef(q2_model)[4] - coef(q3_model)[4])/(sqrt(q3_var[3] + q2_var[3]))

t_stat_educ > 1.96 

### The t stat for education is greater than 1.96, we reject the null hypothesis that the coefficients are for education in q2_model and q3_model are different


coef(q3_model)["Informal"]

### We interpret the coefficient of Informal in q3_model as: those who work in the informal sector, all else equal, earn wages 89 percent lower than those who work in the formal sector



## Q4: interaction terms ---------

q4_model = df %>%
  mutate(male_informal = male * Informal) %>%
  lm(formula = lwage ~ Age + age_sq + Education + Informal + currently_married + urban + ftw + union + lower_caste + male_informal)

summary(q4_model)

### females working in the informal sector receive 127 percent lower wages than females in the formal sector. Males working in the informal sector earn 45 percent more than females working in the informal sector, but 82 percent less than males working in the formal sector

coef(q4_model)[c("Informal", "male_informal")]

sum(coef(q4_model)[c("Informal", "male_informal")])


## Q5: Heteroskedasticity --------------

### Store the residuals from the q4_model and add them to a new dataframe (for convenience)
log_q4_sq_residuals = log(q4_model$residuals^2)

df2 = cbind(df, log_q4_sq_residuals) %>%
  mutate(educ_sq = Education^2)

q4_model = df2 %>%
  lm(formula = log_q4_sq_residuals ~ Age + Education + age_sq + educ_sq + I(Age * Education))

summary(q4_model)


### The null hypothesis is that all of the coefficients are equal to zero; the alternative is that they are not equal to 0.
### With an F-statistic of 128.7 (and corresponding p value of 2.2e-16)
### we reject the null hypothesis of homoskedasticity
### 



## Q6: FGLS -----------
res_q4_model <- residuals(q4_model) 
res_q4_model_sq <- res_q4_model^2

#Run auxiliary regression on the known skedastic function of the error variance
model_test <- df %>% 
  lm(formula = log(res_q4_model_sq) ~ Age + I(Age^2) + Education + I(Education^2) + I(Age*Education) + 1)

summary(model_test)
sigmahat <- fitted(model_test)
hhat <- exp(sigmahat^2)

#Reestimate model in Q4 using FGLS by weighting 1/hhat

q6_model <- df %>% 
  lm(formula = lwage ~ Age + I(Age^2) + Education + Informal + currently_married + urban + ftw + union + lower_caste + I(male * Informal), 
     weights = I(1/hhat))

summary(q6_model)

## Q7: Discussion

