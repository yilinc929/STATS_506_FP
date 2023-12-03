library(foreign) 

## Transform XPT files into csv files.
data=read.xport("/Users/cathy/Desktop/P_DR1TOT.XPT") 

write.csv(data, file ="/Users/cathy/Desktop/dietary.csv")

data=read.xport("/Users/cathy/Desktop/P_DIQ.XPT") 

write.csv(data, file ="/Users/cathy/Desktop/diabetes.csv")

data=read.xport("/Users/cathy/Desktop/P_BPQ.XPT") 

write.csv(data, file ="/Users/cathy/Desktop/blood_pressure&cholesterol.csv")

data=read.xport("/Users/cathy/Desktop/P_DEMO.XPT") 

write.csv(data, file ="/Users/cathy/Desktop/demographic.csv")


##Import the files.
bp<-read.csv("/Users/cathy/Desktop/blood_pressure&cholesterol.csv")
diabetes<-read.csv("/Users/cathy/Desktop/diabetes.csv")
diet<-read.csv("/Users/cathy/Desktop/dietary.csv")
demograph<-read.csv("/Users/cathy/Desktop/demographic.csv")

## Merge and Keep the needed column from each files.
library(tidyverse)
merged_data <- full_join(bp, diabetes, by = "SEQN") %>%
  full_join(diet, by = "SEQN") %>%
  full_join(demograph, by = "SEQN")
## Keep vitamin A, vitamin B1, vitamin B2, vitamin C, alcohol, sugar, saturated fat ,carbohydrates
## blood pressure(BPQ020), diabetes(DIQ010), hypertension(BPD035), high cholesterol(BPQ080), age, gender
## Note that the variables in second line should all be converted to categorical variables.
merged_subs <- merged_data[c("SEQN","DR1TVARA","DR1TVB1","DR1TVB2",
                             "DR1TVC","DR1TALCO","DR1TSUGR","DR1TSFAT","DR1TCARB"
                             ,"BPQ020","DIQ010","BPD035","BPQ080","RIAGENDR","RIDAGEYR"
                             ,"SDMVSTRA","SDMVPSU","WTDR2DPP")]
## Rename the column 
merged_subs <- merged_subs %>%
  rename(
    vitamin_a=DR1TVARA,
    vitamin_b1=DR1TVB1,
    vitamin_b2=DR1TVB2,
    vitamin_c=DR1TVC,
    alcohol=DR1TALCO,
    sugar=DR1TSUGR,
    saturated_fat=DR1TSFAT,
    carbonhydrates=DR1TCARB,
    blood_pressure=BPQ020,
    high_cholesterol=BPQ080,
    diabete=DIQ010,
    hypertension=BPD035,
    gender=RIAGENDR,
    age=RIDAGEYR,
    strata=SDMVSTRA,
    PSU=SDMVPSU,
    weight=WTDR2DPP
  )


##write.csv(merged_subs, file = "/Users/cathy/Desktop/merged_subs.csv", row.names = TRUE)

## Detect the number of rows that are NA.
colSums(is.na(merged_subs))

## Drop those rows that contian NA's. But before that, drop the column for hypertension since it contains too much NA.
merged_subs <- merged_subs[ , -which(names(merged_subs) == "hypertension")]
colSums(is.na(merged_subs))

merged_del<-na.omit(merged_subs)

## Convert blood pressure(BPQ020), diabetes(DIQ010), hypertension(BPD035), high cholesterol(BPQ080) to binary.

merged_del$blood_pressure_b<- ifelse(merged_del$blood_pressure == 1, 1, 0)
#merged_del$hypertension_b <- ifelse(is.na(merged_del$hypertension) | merged_del$hypertension %in% c(777, 999), 0, 1)
merged_del$high_cholesterol_b<- ifelse(merged_del$high_cholesterol == 1, 1, 0)
merged_del$diabete_b<- ifelse(merged_del$diabete %in% c(1,3), 1, 0)


# EDA
##
print(summary(merged_del))


## Univariate Analysis--Distribution
hist(merged_del$vitamin_a, main = "Histogram for Vitamin A")
hist(merged_del$vitamin_b1, main = "Histogram for Vitamin B1")
hist(merged_del$vitamin_b2, main = "Histogram for Vitamin B2")
hist(merged_del$vitamin_c, main = "Histogram for Vitamin C")
hist(merged_del$alcohol, main = "Histogram for Alcohol")
hist(merged_del$sugar, main = "Histogram for Sugar")
hist(merged_del$saturated_fat, main = "Histogram for Saturated Fat")
hist(merged_del$carbonhydrates, main = "Histogram for Carbonhydrates")

## Outlier Detection
boxplot(merged_del$vitamin_a, main = "Boxplot for Vitamin A")
boxplot(merged_del$vitamin_b1, main = "Boxplot for Vitamin B1")
boxplot(merged_del$vitamin_b2, main = "Boxplot for Vitamin B2")
boxplot(merged_del$vitamin_c, main = "Boxplot for Vitamin C")
boxplot(merged_del$alcohol, main = "Boxplot for Alcohol")
boxplot(merged_del$sugar, main = "Boxplot for Sugar")
boxplot(merged_del$saturated_fat, main = "Boxplot for Saturated Fat")
boxplot(merged_del$carbonhydrates, main = "Boxplot for Carbonhydrates")


## Standardize the predictors.
merged_del$vitamin_a_std <- scale(merged_del$vitamin_a)
merged_del$vitamin_b1_std <- scale(merged_del$vitamin_b1)
merged_del$vitamin_b2_std <- scale(merged_del$vitamin_b2)
merged_del$vitamin_c_std <- scale(merged_del$vitamin_c)
merged_del$alcohol_std <- scale(merged_del$alcohol)
merged_del$sugar_std <- scale(merged_del$sugar)
merged_del$saturated_fat_std <- scale(merged_del$saturated_fat)
merged_del$carbohydrates_std <- scale(merged_del$carbohydrates)




##account for the complex survey design in R using the survey package
library(survey)
nhanes_design <- svydesign(
  id = ~PSU,       # Clusters / Primary Sampling Units
  strata = ~strata, # Stratification
  weights = ~weight, # Sampling weights
  data = merged_del,
  nest = TRUE
)

##Logistic Regression cholesterol model/diabetes/blood pressure.
###Without considering gender and age.
cholesterol_model1 <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = nhanes_design, family = binomial)
summary(cholesterol_model1)

diabetes_model1 <- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = nhanes_design, family = binomial)
summary(diabetes_model1)

###Considering gender, age , and interaction.
### Along with model evaluation, showing that they performed poorly, and illustrate why.
cholesterol_model_s <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates + age + gender + age:gender, 
  design = nhanes_design, family = binomial)
summary(cholesterol_model_s)
# Assuming you have the predicted probabilities from your svyglm model
predicted_probabilities <- predict(cholesterol_model_s, type = "response")
threshold <- 0.5
predicted_classes <- ifelse(predicted_probabilities > threshold, 1, 0)

# Assuming you have the actual binary outcomes in your data
actual_classes <- nhanes_design$variables$diabete_b

# Create a confusion matrix manually
conf_matrix <- table(predicted_classes, actual_classes)

# Calculate precision (Positive Predictive Value)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calculate recall (True Positive Rate)
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))


blood_pressure_model_s <- svyglm(
  blood_pressure_b  ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates + age + gender + age:gender,
  design = nhanes_design,family = binomial)
summary(blood_pressure_model_s)
# Assuming you have the predicted probabilities from your svyglm model
predicted_probabilities <- predict(blood_pressure_model_s, type = "response")
threshold <- 0.5
predicted_classes <- ifelse(predicted_probabilities > threshold, 1, 0)

# Assuming you have the actual binary outcomes in your data
actual_classes <- nhanes_design$variables$diabete_b

# Create a confusion matrix manually
conf_matrix <- table(predicted_classes, actual_classes)

# Calculate precision (Positive Predictive Value)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calculate recall (True Positive Rate)
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))


diabetes_model_s <- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates + age + gender + age:gender, 
  design = nhanes_design, family = binomial)
summary(diabetes_model_s)

# Assuming you have the predicted probabilities from your svyglm model
predicted_probabilities <- predict(diabetes_model_s, type = "response")
threshold <- 0.5
predicted_classes <- ifelse(predicted_probabilities > threshold, 1, 0)

# Assuming you have the actual binary outcomes in your data
actual_classes <- nhanes_design$variables$diabete_b

# Create a confusion matrix manually
conf_matrix <- table(predicted_classes, actual_classes)

# Calculate precision (Positive Predictive Value)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calculate recall (True Positive Rate)
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

### Since they performed poorly, I used stepwise regression to select the best model foe each response variable.
### Stepwise Regression.
step(diabetes_model_s)

step(blood_pressure_model_s)

step(cholesterol_model_s)

diabetes_step <- svyglm(
  diabete_b ~  vitamin_b1_std  + alcohol_std 
  + sugar_std + age + gender + age:gender, 
  design = nhanes_design, family = binomial)

bp_step <-svyglm(blood_pressure_b ~ vitamin_a_std + vitamin_b1_std + 
                   vitamin_b2_std + alcohol_std + saturated_fat_std + age + 
                   gender + age:gender, design = nhanes_design, family = binomial)

choles_step <- svyglm(formula = high_cholesterol_b ~ vitamin_b1_std + vitamin_b2_std + 
                        vitamin_c_std + alcohol_std + saturated_fat_std + carbonhydrates + 
                        age + gender + age:gender, design = nhanes_design, family = binomial)

summary(diabetes_step)
summary(bp_step)
summary(choles_step)


##Subset for different age and gender then do analysis within strata.
# Subset for age group 18-30
subset_age_18_30 <- subset(merged_del, age >= 18 & age <= 30)
library(survey)
design1 <- svydesign(
  id = ~PSU,      
  strata = ~strata, 
  weights = ~weight, 
  data = subset_age_18_30,
  nest = TRUE
)

cholesterol_18_30 <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design1, family = binomial)
summary(cholesterol_18_30)

blood_pressure_18_30<- svyglm(
  blood_pressure_b  ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates,
  design = design1,family = binomial)
summary(blood_pressure_18_30)

diabetes_18_30 <- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates , 
  design = design1, family = binomial)
summary(diabetes_18_30)


# Subset for age group 30-50
subset_age_30_50 <- subset(merged_del, age > 30 & age <= 50)
design2 <- svydesign(
  id = ~PSU,      
  strata = ~strata, 
  weights = ~weight, 
  data = subset_age_30_50,
  nest = TRUE
)

cholesterol_30_50 <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design2, family = binomial)
summary(cholesterol_30_50)

blood_pressure_30_50<- svyglm(
  blood_pressure_b  ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates,
  design = design2,family = binomial)
summary(blood_pressure_30_50)

diabetes_30_50 <- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design2, family = binomial)
summary(diabetes_30_50)

# Subset for age group 50+
subset_age_50_plus <- subset(merged_del, age > 50)
design3 <- svydesign(
  id = ~PSU,      
  strata = ~strata, 
  weights = ~weight, 
  data = subset_age_50_plus,
  nest = TRUE
)

cholesterol_50_plus <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design3, family = binomial)
summary(cholesterol_50_plus)

blood_pressure_50_plus<- svyglm(
  blood_pressure_b  ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates,
  design = design3,family = binomial)
summary(blood_pressure_50_plus)

diabetes_50_plus<- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design3, family = binomial)
summary(diabetes_50_plus)

# Subset for males
subset_male <- subset(merged_del, gender == 1)
design4 <- svydesign(
  id = ~PSU,      
  strata = ~strata, 
  weights = ~weight, 
  data = subset_male,
  nest = TRUE
)

cholesterol_male <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design4, family = binomial)
summary(cholesterol_male)

blood_pressure_male<- svyglm(
  blood_pressure_b  ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates,
  design = design4,family = binomial)
summary(blood_pressure_male)

diabetes_male<- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design4, family = binomial)
summary(diabetes_male)

# Subset for females
subset_female <- subset(merged_del, gender == 2)
design5 <- svydesign(
  id = ~PSU,      
  strata = ~strata, 
  weights = ~weight, 
  data = subset_female,
  nest = TRUE
)

cholesterol_female <- svyglm(
  high_cholesterol_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design5, family = binomial)
summary(cholesterol_female)

blood_pressure_female<- svyglm(
  blood_pressure_b  ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates,
  design = design5,family = binomial)
summary(blood_pressure_female)

diabetes_female<- svyglm(
  diabete_b ~ vitamin_a_std + vitamin_b1_std + vitamin_b2_std + vitamin_c_std + alcohol_std 
  + sugar_std + saturated_fat_std + carbonhydrates, 
  design = design5, family = binomial)
summary(diabetes_female)