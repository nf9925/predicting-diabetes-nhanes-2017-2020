###############################################################################################
### (A) LOADING THE NECESSARY LIBRARIES ###
###############################################################################################

# Loading required libraries
library(haven)       # For reading XPT files
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualization
library(caret)       # For modeling
library(glmnet)      # For lasso and ridge regression
library(randomForest) # For tree-based models
library(xgboost)     # For ensemble models
library(pROC)        # For AUC calculation

###############################################################################################
### (B) DEFINING THE OUTCOME OF INTEREST ###
###############################################################################################

# Dependent variable: DIQ010 - Doctor told you have diabetes

# Loading the data
diabetes <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DIQ.xpt"))

# Filtering for SEQN and DIQ010, then keeping Yes and No responses only, and recoding DIQ010 as a binary variable
diabetes_filtered <- diabetes %>%
  select(SEQN, DIQ010) %>%                 # Keep only SEQN and DIQ010 columns
  filter(DIQ010 %in% c(1, 2)) %>%          # Keep Yes (1) and No (2) responses
  mutate(DIQ010_binary = ifelse(DIQ010 == 1, 1, 0))  # Recode: 1 = Yes, 0 = No

# Summary statistics for DIQ010_binary
diabetes_summary <- diabetes_filtered %>% 
  summarise(
    Total = n(),
    Yes = sum(DIQ010_binary == 1),
    No = sum(DIQ010_binary == 0),
    Proportion_Yes = mean(DIQ010_binary)
  )

print(diabetes_summary)

###############################################################################################
### (C) DEFINING THE PREDICTORS OF INTEREST ###
###############################################################################################

# Independent variable: (1) DEMOGRAPHIC DATA

# Loading the data
demographics <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DEMO.xpt")) 

#Filtering for the columns I am interested in 
demographics <- demographics %>% select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2)

# Filter the data for required variables and keeping only males to create another variable
demographics_male <- demographics %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2) %>%  # Select relevant columns
  filter(RIAGENDR == 1)                                    # Keep only males (RIAGENDR == 1)


# Visualization: Age distribution (Just to check if the data is loaded correctly)
library(ggplot2)
library(dplyr)

# For Age Distribution in general 
ggplot(demographics, aes(x = RIDAGEYR)) +
  geom_histogram(binwidth = 5, fill = "indianred", color = "white") +
  labs(
    title = "Age Distribution in the Pre-pandemic Demographics Data (1-150 years)",
    x = "Age (years)",
    y = "Frequency"
  ) +
  theme_classic()

# Plotting age distribution for males
ggplot(demographics_male, aes(x = RIDAGEYR)) +
  geom_histogram(binwidth = 5, fill = "skyblue3", color = "white") +
  labs(
    title = "Age Distribution for Males in the Pre-pandemic Demographics Data",
    x = "Age (years)",
    y = "Frequency"
  ) +
  theme_classic()

# ************************************************************ #
### Independent variable:  (2) QUESTIONNAIRE DATA

# 1. Income Data
income <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_INQ.xpt")) 

# Selecting relevant columns
income <- income %>% select(SEQN, INDFMMPI)

#*************************************** NOTES **************************************#
#INDFMMPI (continuous variable) is used as a subset for this project because I want to: 
# a. Document the intricate variations in poverty level ratios.
# b. Create models with continuous predictors, such as regression or correlations, for better results.
# c. Perform analyses that call for exact numerical input, such as standard deviations and average poverty levels.
#************************************************************************************#

# ************************************************************ #

# 2. Physical Activity Data
physical_activities <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_PAQ.xpt")) 

# Selecting relevant columns
physical_activities <- physical_activities %>% select(SEQN, PAQ655)

# ************************************************************ #

# 3. Smoking Data
smoking <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_SMQ.xpt"))

# Selecting relevant columns
smoking <- smoking %>% select(SEQN, SMQ020)

# ************************************************************ #
### Independent variable:  (3) EXAMINATION DATA

# 1. Body Mass Index (BMI) Data
bmi <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BMX.xpt"))

# Selecting relevant columns
bmi <- bmi %>% select(SEQN, BMXBMI)

# ************************************************************ #

# 2. Blood Pressure Data
blood_pressure <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BPXO.xpt"))

# Selecting relevant columns
blood_pressure <- blood_pressure %>% select(SEQN, BPXOSY3) #I want to keep it relevant and simple so just systolic blood pressure

# ************************************************************ #
### Independent variable:  (3) LABORATORY DATA

# 1. Total Cholesterol Data
total_chol <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_TCHOL.xpt"))

# Selecting relevant columns
total_chol <- total_chol %>% select(SEQN, LBXTC)

# ************************************************************ #

# 2. Fasting Glucose Data

fasting_glucose <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_GLU.xpt"))

# Selecting relevant columns
fasting_glucose <- fasting_glucose %>% select(SEQN, LBXGLU) 

#*************************************** NOTES **************************************#
#* I am choosing LBXGLU - Fasting Glucose (mg/dL) because a fasting plasma glucose (FPG) of 7·0 mmol/L or more has been classified as diabetes(Zhou et al., 2024)
#* Conversion factor: mg/dL = mmol/L * 18.02
#*                          = 7.0 * 18.02
#*                          = 126.14 mg/dL 

#* Diabetes is diagnosed when the fasting blood glucose level is 126 mg/dL (7 mmol/L) or above on two different tests(Riley, 2024).
#************************************************************************************#

# ************************************************************ #

# 3. High-Sensitivity C-Reactive Protein (hs-CRP) Data

hs_crp <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_HSCRP.xpt"))

# Selecting relevant columns
hs_crp <- hs_crp %>% select(SEQN, LBXHSCRP)

###############################################################################################
### (D) MERGING ALL THE FILTERED DATASETS BY SEQN ###
###############################################################################################

# Loading necessary libraries
library(dplyr)
library(purrr)  # For reduce function

# Merging all datasets by SEQN
merged_data <- list(
  diabetes_filtered,      # DIQ010 - Outcome variable
  demographics,           # Demographic data
  income,                 # Income (INDFMMPI)
  physical_activities,    # Physical Activity (PAQ655)
  smoking,                # Smoking (SMQ020)
  bmi,                    # Body Mass Index (BMXBMI)
  blood_pressure,         # Systolic Blood Pressure (BPXOSY3)
  total_chol,             # Total Cholesterol (LBXTC)
  fasting_glucose,        # Fasting Glucose (LBXGLU)
  hs_crp                  # High-Sensitivity CRP (LBXHSCRP)
) %>%
  reduce(full_join, by = "SEQN")  

# Checking the structure and summary of the merged data
str(merged_data)
summary(merged_data)

# Counting missing rows for each column and display as a table
missing_counts <- merged_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  t() %>%
  as.data.frame()

colnames(missing_counts) <- c("Missing_Count")
print("Missing Values Count:")
print(missing_counts)

# Calculating and display missing percentages
missing_percent <- colSums(is.na(merged_data)) / nrow(merged_data) * 100
missing_summary <- data.frame(Variable = names(missing_percent), Missing_Percent = missing_percent)
print("Missing Values Percent:")
print(missing_summary)

### As there are multiple levels of missing data for different kinds of variables, I am introducing an approach to handle these. ###

# Handling Low Missingness (Replace with Median for Continuous Variables)
merged_data$BMXBMI[is.na(merged_data$BMXBMI)] <- median(merged_data$BMXBMI, na.rm = TRUE)

# Handling Moderate Missingness (Median Imputation for Continuous Variables)
variables_to_impute <- c("INDFMMPI", "LBXTC", "LBXHSCRP")
for (var in variables_to_impute) {
  merged_data[[var]][is.na(merged_data[[var]])] <- median(merged_data[[var]], na.rm = TRUE)
}

# Handling Missing Categorical Data (DMDEDUC2)
merged_data$DMDEDUC2 <- as.factor(ifelse(is.na(merged_data$DMDEDUC2), "Missing", as.character(merged_data$DMDEDUC2)))

# Removing Rows with Missing Values in Key Predictors
merged_data_clean <- merged_data %>%
  filter(!is.na(PAQ655) & !is.na(SMQ020) &
           !is.na(BPXOSY3) & !is.na(LBXGLU))

# Removing rows with missing DIQ010_binary because: a.The percentage of missing data is negligible; and (b) it ensures your logistic regression model runs smoothly without errors.
merged_data_clean <- merged_data_clean %>% filter(!is.na(DIQ010_binary))

# Final Check: Remaining Rows and Missing Values
cat("Remaining rows after cleaning:", nrow(merged_data_clean), "\n")
cat("Missing Values Check:\n")
print(colSums(is.na(merged_data_clean)))

# Checking the frequency and percentages of the binary responses
table(merged_data_clean$DIQ010_binary)
prop.table(table(merged_data_clean$DIQ010_binary))

###############################################################################################
### (E) DIVIDING THE PROCESSSED DATA INTO 70% TRAINING AND 30% TESTING SET ###
###############################################################################################

# Setting a seed for reproducibility
set.seed(123)

# Splitting the data into 70% training and 30% testing
split_index <- createDataPartition(merged_data_clean$DIQ010_binary, p = 0.7, list = FALSE)

# Creating training and testing datasets
train_data <- merged_data_clean[split_index, ]  # 70% Training
test_data <- merged_data_clean[-split_index, ]  # 30% Testing

# Checking the dimensions of the splits
cat("Training Set Rows:", nrow(train_data), "\n")
cat("Testing Set Rows:", nrow(test_data), "\n")

# Checking class distribution in train and test sets
cat("\nClass Distribution in Training Set:\n")
print(prop.table(table(train_data$DIQ010_binary)))

cat("\nClass Distribution in Testing Set:\n")
print(prop.table(table(test_data$DIQ010_binary)))

###############################################################################################
### (F) BALANCING THE TRAIN_DATA WITH SMOTE ###
###############################################################################################

#*************************************** NOTES **************************************#
# There is an apparent class imbalance because the minority class (“Yes”) constitutes less than 10% of the total data. This imbalance can lead to models that bias predictions toward the majority class.
# Ultimately, class imbalance can:
# a. Skew performance metrics like accuracy, which may look high even if the model fails to predict the minority class.
# b. Lead to poor predictions for the minority class (low sensitivity/recall).

#Hence, I want to address this by balancing the classes by creating synthetic samples for the minority class with SMOTE. I believe this approach should enable the model to predict the minority class. 
#************************************************************************************#

# Loading required libraries
library(tidymodels)  # For pre-processing
library(themis)      # For SMOTE

# Converting DIQ010_binary to a factor
train_data$DIQ010_binary <- as.factor(train_data$DIQ010_binary)

# Creating a SMOTE recipe and handling categorical predictors
smote_recipe <- recipe(DIQ010_binary ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%  # Dummy encode categorical predictors
  step_smote(DIQ010_binary, over_ratio = 1)                  # Applying SMOTE


# Preparing the SMOTE dataset
set.seed(123)  # Reproducibility
smote_prep <- prep(smote_recipe, training = train_data)

# Extracting the balanced training dataset
train_data_balanced <- juice(smote_prep)

# Checking the class distribution in the balanced training dataset
cat("Balanced Training Class Distribution:\n")
print(prop.table(table(train_data_balanced$DIQ010_binary)))

###############################################################################################
### (G) EXPERIMENTING WITH MODELS AND METHODS ON TRAIN_DATA ###
###############################################################################################

# A. Logistic Regression 
# B. Shrinkage Methods
# C. Tree-Based Methods
# D. Ensemble Method

###############################################################################################
### {H (i)} USING THE TRAIN_DATA TO BUILD LOGISTIC REGRESSION MODELS ###
###############################################################################################

# 1. Log_Model_Male_Only

# Filtering the training data for males only
train_data_male <- train_data_balanced %>% filter(RIAGENDR == 1)
cat("Number of Males in Training Data:", nrow(train_data_male), "\n")

# Logistic Regression for Males Only
log_model_male_only <- glm(DIQ010_binary ~ RIDAGEYR + BMXBMI + BPXOSY3 + LBXGLU + LBXHSCRP, data = train_data_male, family = binomial)
#---------------------------------------------------------------------------------------------#

# 2. Biologically Relevant Model : Predictors that are biologically significant, e.g., fasting glucose (LBXGLU), blood pressure (BPXOSY3), hs-CRP (LBXHSCRP).
bio_model <- glm(DIQ010_binary ~ LBXGLU + BPXOSY3 + LBXHSCRP, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 3. BP_Fasting Glucose_Interaction Model : Includes an interaction term between blood pressure (BPXOSY3) and fasting glucose (LBXGLU).
interaction_bp_glu_model <- glm(DIQ010_binary ~ BPXOSY3 * LBXGLU, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 4. Demo_Lab_Model : Includes all demographic (RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2) and laboratory predictors (LBXGLU, LBXTC, LBXHSCRP).
str(train_data_balanced)
demo_lab_model <- glm(DIQ010_binary ~ RIDAGEYR + RIAGENDR + RIDRETH3 + DMDEDUC2_X2 + DMDEDUC2_X3 + DMDEDUC2_X4 + DMDEDUC2_X5 + DMDEDUC2_X7 + DMDEDUC2_X9 + DMDEDUC2_Missing + LBXGLU + LBXTC + LBXHSCRP, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 5. Lab_Exam_Model : Combines laboratory (LBXGLU, LBXTC, LBXHSCRP) and examination predictors (BMXBMI, BPXOSY3).
lab_exam_model <- glm(DIQ010_binary ~ LBXGLU + LBXTC + LBXHSCRP + BMXBMI + BPXOSY3, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 6. Demo_AGR_Model : Key demographic predictors: RIDAGEYR, RIAGENDR, RIDRETH3, excluding Education. 
demo_agr_model <- glm(DIQ010_binary ~ RIDAGEYR + RIAGENDR + RIDRETH3, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 7. Demo_Only_Model : Includes only demographic variables (RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2).
demo_only_model <- glm(DIQ010_binary ~ RIDAGEYR + RIAGENDR + RIDRETH3 + DMDEDUC2_X2 + DMDEDUC2_X3 + DMDEDUC2_X4 + DMDEDUC2_X5 + DMDEDUC2_X7 + DMDEDUC2_X9 + DMDEDUC2_Missing, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 8. Exam_Only_Model : Only examination variables.
exam_only_model <- glm(DIQ010_binary ~ BMXBMI + BPXOSY3, data = train_data_balanced, family = binomial)

#---------------------------------------------------------------------------------------------#

# 9. Interaction_Model: Includes multiple interaction terms among key predictors and those interactions might include combinations of demographic, exam, and lab variables.
interaction_model <- glm(DIQ010_binary ~ RIDAGEYR * BMXBMI + BPXOSY3 * LBXGLU + RIAGENDR * LBXHSCRP, data = train_data_balanced, family = binomial)

# Summarizing all models
summary(log_model_male_only)
summary(bio_model)
summary(interaction_bp_glu_model)
summary(demo_lab_model)
summary(lab_exam_model)
summary(demo_agr_model)
summary(demo_only_model)
summary(exam_only_model)
summary(interaction_model)

###############################################################################################
### {H (ii)} EVALUATING LOGISTIC REGRESSION MODELS ON TEST_DATA ###
###############################################################################################

# Loading required libraries
library(caret)
library(pROC)

# Defining a list of all models
models <- list(
  "Log_Model_Male_Only" = log_model_male_only,
  "Biologically Relevant Model" = bio_model,
  "BP_Glu Interaction Model" = interaction_bp_glu_model,
  "Demo_Lab_Model" = demo_lab_model,
  "Lab_Exam_Model" = lab_exam_model,
  "Demo_AGR_Model" = demo_agr_model,
  "Demo_Only_Model" = demo_only_model,
  "Exam_Only_Model" = exam_only_model,
  "Interaction_Model" = interaction_model
)

# Ensuring DMDEDUC2 in test_data is encoded like train_data_balanced
test_data <- test_data %>%
  mutate(
    DMDEDUC2_X2 = ifelse(DMDEDUC2 == 2, 1, 0),
    DMDEDUC2_X3 = ifelse(DMDEDUC2 == 3, 1, 0),
    DMDEDUC2_X4 = ifelse(DMDEDUC2 == 4, 1, 0),
    DMDEDUC2_X5 = ifelse(DMDEDUC2 == 5, 1, 0),
    DMDEDUC2_X7 = ifelse(DMDEDUC2 == 7, 1, 0),
    DMDEDUC2_X9 = ifelse(DMDEDUC2 == 9, 1, 0),
    DMDEDUC2_Missing = ifelse(is.na(DMDEDUC2), 1, 0)
  )

# Initializing an empty results dataframe
model_results <- data.frame(
  Model = character(),
  AIC = numeric(),
  Accuracy = numeric(),
  AUC_ROC = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

# Looping through each model to calculate performance metrics
for (name in names(models)) {
  model <- models[[name]]
  
  # AIC for the model
  aic_value <- AIC(model)
  
  # Predicting probabilities and classes on the test set
  test_probs <- predict(model, newdata = test_data, type = "response")
  test_preds <- ifelse(test_probs > 0.5, 1, 0)
  
  # Confusion Matrix
  cm <- confusionMatrix(factor(test_preds), factor(test_data$DIQ010_binary), positive = "1")
  
  # Calculating AUC-ROC
  roc_curve <- roc(test_data$DIQ010_binary, test_probs)
  auc_value <- auc(roc_curve)
  
  # Extracting metrics
  acc <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  
  # Appending results to dataframe
  model_results <- rbind(model_results, data.frame(
    Model = name,
    AIC = round(aic_value, 2),
    Accuracy = round(acc, 3),
    AUC_ROC = round(auc_value, 3),
    Sensitivity = round(sens, 3),
    Specificity = round(spec, 3)
  ))
}

# Sorting the model_results dataframe by AIC in ascending order
model_results <- model_results %>% arrange(AIC)

# Printing the summarized and sorted results
print("Performance Summary for All Logistic Regression Models (Sorted by AIC):")
print(model_results)

#---------------------------------------------------------------------------------------------#

# Generating ROC Curves for Logistic Regression Models

# Loading required libraries
library(pROC)
library(ggplot2)

# Initializing a list to store ROC data for each model
roc_data <- list()

# Colors for ROC curves
colors <- c("red", "orange", "yellow", "green", "blue", "purple", "cyan", "brown", "darkgreen")

# Calculating AIC for each model and prepare ROC curve data
aic_values <- c()  # Store AIC values
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- names(models)[i]
  
  # Predicting probabilities on test data
  test_probs <- predict(model, newdata = test_data, type = "response")
  
  # Generating ROC curve
  roc_curve <- roc(test_data$DIQ010_binary, test_probs)
  
  # Extracting TPR (sensitivity) and FPR (1 - specificity)
  roc_df <- data.frame(
    TPR = roc_curve$sensitivities,
    FPR = 1 - roc_curve$specificities,
    Model = model_name
  )
  
  # Appending AIC value and update model name with AIC
  aic_value <- round(AIC(model), 2)
  aic_values <- c(aic_values, aic_value)
  roc_df$Model <- paste0(model_name, " (AIC: ", aic_value, ")")
  
  # Storing in list
  roc_data[[i]] <- roc_df
}

# Combining all ROC data into a single dataframe
combined_roc_data <- do.call(rbind, roc_data)

# Updated unique model names with AIC for legend
model_names_with_aic <- paste0(names(models), " (AIC: ", aic_values, ")")

# Plotting ROC curves using ggplot2
ggplot(combined_roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1) +  # Use linewidth instead of size
  scale_color_manual(
    values = colors,
    breaks = model_names_with_aic,
    labels = model_names_with_aic
  ) +
  labs(
    title = "ROC Curves for Logistic Regression Models with AIC",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Models with AIC"
  ) +
  theme_classic() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

###############################################################################################
### {I (i)} USING THE TRAIN_DATA TO BUILD SHRINKAGE METHODS(LASSO AND RIDGE MODELS) ###
###############################################################################################

# Loading required libraries
library(glmnet)  # For LASSO and Ridge regression
library(caret)   # For data splitting and preprocessing

# Preparing the data for glmnet
# Converting the target variable to numeric (0 and 1)
train_y <- as.numeric(as.character(train_data_balanced$DIQ010_binary))

# Excluding the response variable and convert predictors to a matrix (required by glmnet)
train_x <- model.matrix(DIQ010_binary ~ . - 1, data = train_data_balanced)

#---------------------------------------------------------------------------------------------#
### 1. LASSO Regression ###
#---------------------------------------------------------------------------------------------#

set.seed(123)  # For reproducibility

# Fitting LASSO model (alpha = 1 for LASSO)
lasso_model <- cv.glmnet(train_x, train_y, family = "binomial", alpha = 1, nfolds = 10)

# Best lambda for LASSO
best_lambda_lasso <- lasso_model$lambda.min
cat("Best Lambda for LASSO:", best_lambda_lasso, "\n")

# Refitting the LASSO model using the best lambda
final_lasso_model <- glmnet(train_x, train_y, family = "binomial", alpha = 1, lambda = best_lambda_lasso)

# Coefficients from the LASSO model
cat("LASSO Coefficients:\n")
print(coef(final_lasso_model))

#---------------------------------------------------------------------------------------------#
### 2. Ridge Regression ###
#---------------------------------------------------------------------------------------------#
set.seed(123)  # For reproducibility

# Fitting Ridge model (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(train_x, train_y, family = "binomial", alpha = 0, nfolds = 10)

# Best lambda for Ridge
best_lambda_ridge <- ridge_model$lambda.min
cat("Best Lambda for Ridge:", best_lambda_ridge, "\n")

# Refitting the Ridge model using the best lambda
final_ridge_model <- glmnet(train_x, train_y, family = "binomial", alpha = 0, lambda = best_lambda_ridge)

# Coefficients from the Ridge model
cat("Ridge Coefficients:\n")
print(coef(final_ridge_model))

###############################################################################################
### {I (ii)} EVALUATING SHRINKAGE METHODS(LASSO AND RIDGE MODELS) ON TEST_DATA ###
###############################################################################################

# Preparing test data
test_y <- as.numeric(as.character(test_data$DIQ010_binary))
test_x <- model.matrix(DIQ010_binary ~ . - 1, data = test_data)

# Ensuring the columns match between train_x and test_x
missing_cols <- setdiff(colnames(train_x), colnames(test_x))
if (length(missing_cols) > 0) {
  test_x <- cbind(test_x, matrix(0, nrow = nrow(test_x), ncol = length(missing_cols)))
  colnames(test_x)[(ncol(test_x) - length(missing_cols) + 1):ncol(test_x)] <- missing_cols
}

# Reordering test_x columns to match train_x
test_x <- test_x[, colnames(train_x)]

# LASSO Predictions
lasso_probs <- predict(final_lasso_model, newx = test_x, type = "response")
lasso_preds <- ifelse(lasso_probs > 0.5, 1, 0)

# Ridge Predictions
ridge_probs <- predict(final_ridge_model, newx = test_x, type = "response")
ridge_preds <- ifelse(ridge_probs > 0.5, 1, 0)

# Converting the predicted probabilities from matrix to vector
lasso_probs_vector <- as.vector(lasso_probs)
ridge_probs_vector <- as.vector(ridge_probs)

# Calculating AUC for LASSO
lasso_auc <- auc(test_data$DIQ010_binary, lasso_probs_vector)
cat("\nLASSO AUC:", round(lasso_auc, 3), "\n")

# Calculating AUC for Ridge
ridge_auc <- auc(test_data$DIQ010_binary, ridge_probs_vector)
cat("Ridge AUC:", round(ridge_auc, 3), "\n")

#---------------------------------------------------------------------------------------------#

# Performance Metrics of LASSO and Ridge Models

# Loading necessary libraries
library(caret)
library(pROC)

# Initializing a dataframe to store performance metrics
shrinkage_results <- data.frame(
  Model = character(),
  AIC = numeric(),
  Accuracy = numeric(),
  AUC_ROC = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

# Function to calculate AIC for a glmnet model
calculate_aic <- function(model, x, y) {
  preds <- predict(model, newx = x, type = "link")  # Linear predictions
  loglik <- sum(y * preds - log(1 + exp(preds)))    # Log-likelihood
  k <- length(coef(model)[, 1]) - 1                # Number of non-zero coefficients
  aic <- -2 * loglik + 2 * k                       # AIC formula
  return(aic)
}

#---------------------------------------------------------------------------------------------#
### 1. LASSO Metrics

# Predictions
lasso_preds <- ifelse(lasso_probs_vector > 0.5, 1, 0)

# Confusion Matrix
cm_lasso <- confusionMatrix(factor(lasso_preds), factor(test_data$DIQ010_binary), positive = "1")

# Metrics
lasso_aic <- calculate_aic(final_lasso_model, test_x, test_y)
lasso_accuracy <- cm_lasso$overall["Accuracy"]
lasso_sensitivity <- cm_lasso$byClass["Sensitivity"]
lasso_specificity <- cm_lasso$byClass["Specificity"]

# Appending LASSO results
shrinkage_results <- rbind(shrinkage_results, data.frame(
  Model = "LASSO",
  AIC = round(lasso_aic, 2),
  Accuracy = round(lasso_accuracy, 3),
  AUC_ROC = round(lasso_auc, 3),
  Sensitivity = round(lasso_sensitivity, 3),
  Specificity = round(lasso_specificity, 3)
))

#---------------------------------------------------------------------------------------------#
### 2. Ridge Metrics

# Predictions
ridge_preds <- ifelse(ridge_probs_vector > 0.5, 1, 0)

# Confusion Matrix
cm_ridge <- confusionMatrix(factor(ridge_preds), factor(test_data$DIQ010_binary), positive = "1")

# Metrics
ridge_aic <- calculate_aic(final_ridge_model, test_x, test_y)
ridge_accuracy <- cm_ridge$overall["Accuracy"]
ridge_sensitivity <- cm_ridge$byClass["Sensitivity"]
ridge_specificity <- cm_ridge$byClass["Specificity"]

# Appending Ridge results
shrinkage_results <- rbind(shrinkage_results, data.frame(
  Model = "Ridge",
  AIC = round(ridge_aic, 2),
  Accuracy = round(ridge_accuracy, 3),
  AUC_ROC = round(ridge_auc, 3),
  Sensitivity = round(ridge_sensitivity, 3),
  Specificity = round(ridge_specificity, 3)
))

# Printing Final Results
print("Performance Metrics for Shrinkage Methods (LASSO and Ridge):")
print(shrinkage_results)

#---------------------------------------------------------------------------------------------#

# Generating ROC Curves for Shrinkage Methods: 

# Loading necessary libraries
library(ggplot2)
library(pROC)

# Preparing data for ROC Curves
lasso_roc <- roc(test_data$DIQ010_binary, lasso_probs_vector)
ridge_roc <- roc(test_data$DIQ010_binary, ridge_probs_vector)

# Creating a dataframe for plotting
roc_data <- data.frame(
  FPR = c(1 - lasso_roc$specificities, 1 - ridge_roc$specificities),
  TPR = c(lasso_roc$sensitivities, ridge_roc$sensitivities),
  Model = c(rep(paste0("LASSO (AIC: ", round(lasso_aic, 2), ")"), length(lasso_roc$specificities)),
            rep(paste0("Ridge (AIC: ", round(ridge_aic, 2), ")"), length(ridge_roc$specificities)))
)

# Converting the Model column to a factor with specific levels
roc_data$Model <- factor(roc_data$Model, levels = c(
  paste0("LASSO (AIC: ", round(lasso_aic, 2), ")"),
  paste0("Ridge (AIC: ", round(ridge_aic, 2), ")")
))

# Generating the ROC plot
ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      `LASSO (AIC: 40.37)` = "red",
      `Ridge (AIC: 99.03)` = "blue"
    )
  ) +
  labs(
    title = "ROC Curves for Shrinkage Methods (LASSO and Ridge)",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) +
  theme_classic() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

###############################################################################################
### {J (i)} USING THE TRAIN_DATA TO BUILD MODELS BASED ON TREE_BASED METHODS AND EVALUATING THOSE ON TEST_DATA###
###############################################################################################

#1.	Random Forest
#2.	Gradient Boosting (GBM)
#3.	XGBoost
#4.	Decision Tree

#---------------------------------------------------------------------------------------------#

# Loading necessary libraries
library(caret)          # For model training and evaluation
library(randomForest)   # For Random Forest
library(gbm)            # For GBM
library(xgboost)        # For XGBoost
library(rpart)          # For Decision Tree
library(rpart.plot)     # For visualizing Decision Tree

# Ensuring DIQ010_binary is a factor (required for classification)
train_data$DIQ010_binary <- as.factor(train_data$DIQ010_binary)
test_data$DIQ010_binary <- as.factor(test_data$DIQ010_binary)

# Preparing predictor matrix (X) and target variable (Y)
train_x <- train_data[, -which(names(train_data) == "DIQ010_binary")]
train_y <- train_data$DIQ010_binary
test_x <- test_data[, -which(names(test_data) == "DIQ010_binary")]
test_y <- test_data$DIQ010_binary

#--------------------------------------------------------------------------------#
### 1. Random Forest ###
set.seed(123) # For reproducibility
rf_model <- randomForest(DIQ010_binary ~ ., data = train_data, ntree = 500)
rf_preds <- predict(rf_model, test_data, type = "class")
rf_probs <- predict(rf_model, test_data, type = "prob")[, 2] # Probabilities

#--------------------------------------------------------------------------------#
### 2. Gradient Boosting (GBM) ###
set.seed(123)

# Converting DIQ010_binary to numeric (0 and 1)
train_data$DIQ010_binary <- as.numeric(as.character(train_data$DIQ010_binary))  # Ensure binary as 0/1
test_data$DIQ010_binary <- as.numeric(as.character(test_data$DIQ010_binary))

# Training the GBM model
gbm_model <- gbm(
  DIQ010_binary ~ ., 
  data = train_data,
  distribution = "bernoulli", 
  n.trees = 500,
  interaction.depth = 3, 
  shrinkage = 0.01, 
  cv.folds = 5, 
  verbose = FALSE
)

gbm_preds <- predict(gbm_model, test_data, n.trees = 500, type = "response")
gbm_class_preds <- ifelse(gbm_preds > 0.5, 1, 0)

#--------------------------------------------------------------------------------#
### 3. XGBoost ###

# Ensuring DIQ010_binary is numeric (0 and 1)
train_y <- as.numeric(as.character(train_data$DIQ010_binary))
test_y <- as.numeric(as.character(test_data$DIQ010_binary))

# Creating model matrices for train and test
train_x <- model.matrix(DIQ010_binary ~ . - 1, data = train_data)
test_x <- model.matrix(DIQ010_binary ~ . - 1, data = test_data)

# Aligning feature names between train_x and test_x
missing_cols <- setdiff(colnames(train_x), colnames(test_x))
extra_cols <- setdiff(colnames(test_x), colnames(train_x))

# Adding missing columns in test_x as 0
if (length(missing_cols) > 0) {
  test_x <- cbind(test_x, matrix(0, nrow = nrow(test_x), ncol = length(missing_cols)))
  colnames(test_x)[(ncol(test_x) - length(missing_cols) + 1):ncol(test_x)] <- missing_cols
}

# Removing any extra columns in test_x
test_x <- test_x[, colnames(train_x)]

# Converting to XGBoost DMatrix
train_matrix <- xgb.DMatrix(data = train_x, label = train_y)
test_matrix <- xgb.DMatrix(data = test_x, label = test_y)

# Training XGBoost model
set.seed(123)
xgb_model <- xgboost(
  data = train_matrix, 
  objective = "binary:logistic", 
  nrounds = 500, 
  max_depth = 3, 
  eta = 0.01, 
  verbose = 0
)

# Predictions
xgb_preds <- predict(xgb_model, test_matrix)
xgb_class_preds <- ifelse(xgb_preds > 0.5, 1, 0)

#--------------------------------------------------------------------------------#
### 4. Decision Tree ###
set.seed(123)
dt_model <- rpart(DIQ010_binary ~ ., data = train_data, method = "class")
dt_preds <- predict(dt_model, test_data, type = "class")
dt_probs <- predict(dt_model, test_data, type = "prob")[, 2]

#--------------------------------------------------------------------------------#
### Evaluation of All Models ###
library(pROC)

# Function to compute metrics
compute_metrics <- function(true_values, predicted_classes, predicted_probs) {
  cm <- confusionMatrix(factor(predicted_classes), factor(true_values), positive = "1")
  auc_value <- auc(roc(true_values, predicted_probs))
  return(list(
    Accuracy = round(cm$overall["Accuracy"], 3),
    Sensitivity = round(cm$byClass["Sensitivity"], 3),
    Specificity = round(cm$byClass["Specificity"], 3),
    AUC_ROC = round(auc_value, 3)
  ))
}

# Initializing an empty data frame to store results
tree_results <- data.frame(
  Model = character(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  AUC_ROC = numeric(),
  stringsAsFactors = FALSE
)

# Computing Metrics for Each Model
# 1. Random Forest
rf_metrics <- compute_metrics(test_data$DIQ010_binary, rf_preds, rf_probs)
tree_results <- rbind(tree_results, data.frame(
  Model = "Random Forest",
  Accuracy = rf_metrics$Accuracy,
  Sensitivity = rf_metrics$Sensitivity,
  Specificity = rf_metrics$Specificity,
  AUC_ROC = rf_metrics$AUC_ROC
))
#---------------------------------------------------------------------------------------------#

# 2. GBM
gbm_metrics <- compute_metrics(test_data$DIQ010_binary, gbm_class_preds, gbm_preds)
tree_results <- rbind(tree_results, data.frame(
  Model = "GBM",
  Accuracy = gbm_metrics$Accuracy,
  Sensitivity = gbm_metrics$Sensitivity,
  Specificity = gbm_metrics$Specificity,
  AUC_ROC = gbm_metrics$AUC_ROC
))
#---------------------------------------------------------------------------------------------#

# 3. XGBoost
xgb_metrics <- compute_metrics(test_data$DIQ010_binary, xgb_class_preds, xgb_preds)
tree_results <- rbind(tree_results, data.frame(
  Model = "XGBoost",
  Accuracy = xgb_metrics$Accuracy,
  Sensitivity = xgb_metrics$Sensitivity,
  Specificity = xgb_metrics$Specificity,
  AUC_ROC = xgb_metrics$AUC_ROC
))
#---------------------------------------------------------------------------------------------#

# 4. Decision Tree
dt_metrics <- compute_metrics(test_data$DIQ010_binary, dt_preds, dt_probs)
tree_results <- rbind(tree_results, data.frame(
  Model = "Decision Tree",
  Accuracy = dt_metrics$Accuracy,
  Sensitivity = dt_metrics$Sensitivity,
  Specificity = dt_metrics$Specificity,
  AUC_ROC = dt_metrics$AUC_ROC
))

# Displaying Results
cat("Performance Metrics for Tree-Based Models:\n")
print(tree_results)

# Plotting the decision tree

# Loading required libraries
library(rpart)       # For Decision Tree
library(rpart.plot)  # For plotting the tree

# Ensuring DIQ010_binary is a factor in the balanced data
train_data_balanced$DIQ010_binary <- as.factor(train_data_balanced$DIQ010_binary)

# Building the Decision Tree model using the balanced training data
set.seed(123)
dt_model_balanced <- rpart(DIQ010_binary ~ ., data = train_data_balanced, method = "class")

# Plotting the Decision Tree
rpart.plot(dt_model_balanced, 
           main = "Decision Tree Diagram (SMOTE-Balanced Data)", 
           type = 2,          
           extra = 104,        # Class probabilities at each node
           under = TRUE,       # Adds node numbers under boxes
           tweak = 1.2,        # Adjust text size
           box.palette = "Greens",  # Green color palette for clarity
           shadow.col = "gray",     # Adds shadow for depth
           branch.lty = 2       # Dashed branches
)
#---------------------------------------------------------------------------------------------#

# Generating ROC Curves for Tree-Based Methods

# Loading necessary libraries
library(ggplot2)
library(pROC)

# Computing ROC for each Tree-Based Model
rf_roc <- roc(test_data$DIQ010_binary, rf_probs)
gbm_roc <- roc(test_data$DIQ010_binary, gbm_preds)
xgb_roc <- roc(test_data$DIQ010_binary, xgb_preds)
dt_roc <- roc(test_data$DIQ010_binary, dt_probs)

# Safely computing AIC with small epsilon for probabilities
compute_aic_safe <- function(probs, true_values, n_params) {
  probs <- ifelse(probs == 0, 1e-6, ifelse(probs == 1, 1 - 1e-6, probs)) # Avoid log(0)
  loglik <- sum(true_values * log(probs) + (1 - true_values) * log(1 - probs))
  aic <- -2 * loglik + 2 * n_params
  return(round(aic, 2))
}

# Corrected AIC for each tree-based model
rf_aic <- compute_aic_safe(rf_probs, as.numeric(as.character(test_data$DIQ010_binary)), n_params = 50)
gbm_aic <- compute_aic_safe(gbm_preds, as.numeric(as.character(test_data$DIQ010_binary)), n_params = 30)
xgb_aic <- compute_aic_safe(xgb_preds, as.numeric(as.character(test_data$DIQ010_binary)), n_params = 40)
dt_aic <- compute_aic_safe(dt_probs, as.numeric(as.character(test_data$DIQ010_binary)), n_params = 10)

# Summarizing AIC values into a data frame
tree_aic_summary <- data.frame(
  Model = c("Random Forest", "GBM", "XGBoost", "Decision Tree"),
  AIC = c(rf_aic, gbm_aic, xgb_aic, dt_aic)
)

# Printing the corrected AIC summary table
cat("AIC Values for Tree-Based Methods (Adjusted):\n")
print(tree_aic_summary)

# Combining data for ROC Curves
roc_data <- data.frame(
  FPR = c(1 - rf_roc$specificities, 1 - gbm_roc$specificities, 
          1 - xgb_roc$specificities, 1 - dt_roc$specificities),
  TPR = c(rf_roc$sensitivities, gbm_roc$sensitivities, 
          xgb_roc$sensitivities, dt_roc$sensitivities),
  Model = c(rep(paste0("Random Forest (AIC: ", rf_aic, ")"), length(rf_roc$specificities)),
            rep(paste0("GBM (AIC: ", gbm_aic, ")"), length(gbm_roc$specificities)),
            rep(paste0("XGBoost (AIC: ", xgb_aic, ")"), length(xgb_roc$specificities)),
            rep(paste0("Decision Tree (AIC: ", dt_aic, ")"), length(dt_roc$specificities)))
)

# Correcting AIC values for legend
rf_name <- paste0("Random Forest (AIC: ", rf_aic, ")")
gbm_name <- paste0("GBM (AIC: ", gbm_aic, ")")
xgb_name <- paste0("XGBoost (AIC: ", xgb_aic, ")")
dt_name <- paste0("Decision Tree (AIC: ", dt_aic, ")")

# Ensuring consistency in model names in the data
roc_data$Model <- factor(roc_data$Model, levels = c(rf_name, gbm_name, xgb_name, dt_name))

# Generating ROC Curves with ggplot2
ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = setNames(c("red", "blue", "green", "purple"),
                      c(rf_name, gbm_name, xgb_name, dt_name))
  ) +
  labs(
    title = "ROC Curves for Tree-Based Methods (Random Forest, GBM, XGBoost, Decision Tree)",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) +
  theme_classic() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

###############################################################################################
### {K} USING THE TRAIN_DATA TO BUILD MODEL BASED ON ENSEMBLE METHOD AND EVALUATING ON TEST_DATA###
###############################################################################################

# Loading required libraries
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(rpart)
library(pROC)
library(glmnet)

# Generating Base Model Predictions for Train Data
set.seed(123)

# Random Forest Predictions
rf_train_preds <- predict(rf_model, train_data, type = "prob")[, 2]
rf_test_preds <- predict(rf_model, test_data, type = "prob")[, 2]

# GBM Predictions
gbm_train_preds <- predict(gbm_model, train_data, n.trees = 500, type = "response")
gbm_test_preds <- predict(gbm_model, test_data, n.trees = 500, type = "response")

# XGBoost Predictions
xgb_train_preds <- predict(xgb_model, train_matrix)
xgb_test_preds <- predict(xgb_model, test_matrix)

# Decision Tree Predictions
dt_train_preds <- predict(dt_model, train_data, type = "prob")[, 2]
dt_test_preds <- predict(dt_model, test_data, type = "prob")[, 2]

# Preparing Meta-Model Data
train_meta <- data.frame(
  RF = rf_train_preds,
  GBM = gbm_train_preds,
  XGB = xgb_train_preds,
  DT = dt_train_preds,
  Target = train_data$DIQ010_binary
)

test_meta <- data.frame(
  RF = rf_test_preds,
  GBM = gbm_test_preds,
  XGB = xgb_test_preds,
  DT = dt_test_preds,
  Target = test_data$DIQ010_binary
)

# Standardizing predictions for meta-model
train_meta_scaled <- as.data.frame(scale(train_meta[, -5]))  # Scale predictors only
train_meta_scaled$Target <- train_meta$Target

test_meta_scaled <- as.data.frame(scale(test_meta[, -5]))
test_meta_scaled$Target <- test_meta$Target

# Training Ridge Regression Meta-Model
set.seed(123)
x_train <- as.matrix(train_meta_scaled[, -5])
y_train <- as.numeric(as.character(train_meta_scaled$Target))

meta_ridge <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0)  # Ridge regression (alpha = 0)

# Predicting on Test Data
x_test <- as.matrix(test_meta_scaled[, -5])
meta_probs <- predict(meta_ridge, newx = x_test, s = "lambda.min", type = "response")
meta_preds <- ifelse(meta_probs > 0.5, 1, 0)

# Evaluating the Stacked Model
meta_cm <- confusionMatrix(factor(meta_preds), factor(test_meta_scaled$Target), positive = "1")
meta_auc <- auc(roc(test_meta_scaled$Target, as.vector(meta_probs)))

#---------------------------------------------------------------------------------------------#

# Performance metrics for Ensemble Method

# Converting meta_probs to a numeric vector
meta_probs_vector <- as.vector(meta_probs)

# Calculating Confusion Matrix
meta_cm <- confusionMatrix(factor(meta_preds), factor(test_meta_scaled$Target), positive = "1")

# Calculating AUC-ROC
meta_auc <- auc(roc(test_meta_scaled$Target, meta_probs_vector))

# Function to calculate AIC (pseudo-log-likelihood for logistic regression)
calculate_aic <- function(probs, true_values, n_params) {
  loglik <- sum(true_values * log(probs) + (1 - true_values) * log(1 - probs))
  aic <- -2 * loglik + 2 * n_params
  return(round(aic, 2))
}

# Calculating AIC for Ridge Meta-Model
n_params <- length(coef(meta_ridge, s = "lambda.min")) - 1  # Number of non-zero coefficients
meta_aic <- calculate_aic(meta_probs_vector, as.numeric(as.character(test_meta_scaled$Target)), n_params)

# Extracting Metrics
meta_accuracy <- round(meta_cm$overall["Accuracy"], 3)
meta_sensitivity <- round(meta_cm$byClass["Sensitivity"], 3)
meta_specificity <- round(meta_cm$byClass["Specificity"], 3)
meta_auc_rounded <- round(meta_auc, 3)

# Displaying Results
cat("Performance Metrics for Stacked Ensemble Model:\n")
cat("Accuracy     :", meta_accuracy, "\n")
cat("Sensitivity  :", meta_sensitivity, "\n")
cat("Specificity  :", meta_specificity, "\n")
cat("AUC-ROC      :", meta_auc_rounded, "\n")
cat("AIC          :", meta_aic, "\n")

#---------------------------------------------------------------------------------------------#

# Generating ROC Curve for Ensemble Method

# Loading required libraries
library(ggplot2)
library(pROC)

# Computing ROC for the Ensemble Method
ensemble_roc <- roc(test_meta_scaled$Target, meta_probs_vector)

# Creating a data frame for ROC Curve plotting
ensemble_roc_data <- data.frame(
  FPR = 1 - ensemble_roc$specificities,  # False Positive Rate
  TPR = ensemble_roc$sensitivities,      # True Positive Rate
  Model = paste0("Stacked Ensemble (AIC: ", meta_aic, ")")
)

# Plotting the ROC Curve with ggplot2
ggplot(ensemble_roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1) +  # Plot the ROC curve
  scale_color_manual(values = c("Stacked Ensemble (AIC: 11.26)" = "darkorange")) +
  labs(
    title = "ROC Curve for Stacked Ensemble Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) +
  theme_classic() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

###############################################################################################
### (L) OPTIONAL VISUALIZATIONS ###
###############################################################################################


# Loading required libraries
library(ggplot2)
library(RColorBrewer)

# Checking available color palettes
display.brewer.all()

# Selecting a color palette (e.g., "Spectral" or "Pastel1")
selected_palette <- brewer.pal(9, "Spectral")  # Change palette name if needed

# Creating a histogram for Total Cholesterol (LBXTC)
ggplot(test_data, aes(x = LBXTC)) +
  geom_histogram(binwidth = 10, fill = selected_palette[4], color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Cholesterol (LBXTC)",
       x = "Total Cholesterol (mg/dL)",
       y = "Frequency") +
  theme_classic()

# Creating a density plot for Total Cholesterol
ggplot(test_data, aes(x = LBXTC)) +
  geom_density(fill = selected_palette[6], color = selected_palette[4], alpha = 0.7) +
  labs(title = "Density Plot of Total Cholesterol (LBXTC)",
       x = "Total Cholesterol (mg/dL)",
       y = "Density") +
  theme_classic()

#############################################

# Loading required libraries
library(ggplot2)
library(colorspace)  # For diverge_hcl color palettes

# Creating a diverging color palette using diverge_hcl
diverge_colors <- diverge_hcl(7, h = c(240, 10), c = 100, l = c(30, 90))

# Histogram for Systolic Blood Pressure
ggplot(test_data, aes(x = BPXOSY3)) +
  geom_histogram(binwidth = 5, fill = diverge_colors[4], color = "black", alpha = 0.8) +
  labs(title = "Distribution of Systolic Blood Pressure (BPXOSY3)",
       x = "Systolic Blood Pressure (mmHg)",
       y = "Frequency") +
  theme_classic()

# Density plot for Systolic Blood Pressure
ggplot(test_data, aes(x = BPXOSY3)) +
  geom_density(fill = diverge_colors[6], color = diverge_colors[3], alpha = 0.8) +
  labs(title = "Density Plot of Systolic Blood Pressure (BPXOSY3)",
       x = "Systolic Blood Pressure (mmHg)",
       y = "Density") +
  theme_classic()

#############################################

# Loading required libraries
library(ggplot2)
library(RColorBrewer)  # For Pastel1 color palette

# Selecting colors from the Pastel1 palette
pastel_colors <- brewer.pal(3, "Pastel1")  # Three colors for the plots


# Gender distribution
ggplot(demographics, aes(x = factor(RIAGENDR))) +
  geom_bar(fill = "darkturquoise", color = "black") +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  theme_classic()

#############################################

# Race/Ethnicity distribution
ggplot(demographics, aes(x = factor(RIDRETH3))) +
  geom_bar(fill = "coral4", color = "white") +
  labs(title = "Race/Ethnicity Distribution",
       x = "Race/Ethnicity",
       y = "Count") +
  scale_x_discrete(labels = c("1" = "Mexican American", 
                              "2" = "Other Hispanic", 
                              "3" = "Non-Hispanic White", 
                              "4" = "Non-Hispanic Black", 
                              "6" = "Asian", 
                              "7" = "Other/Multi-Racial")) +
  theme_classic()

#############################################

# Education Level distribution
ggplot(demographics, aes(x = factor(DMDEDUC2))) +
  geom_bar(fill = "gold", color = "black") +
  labs(title = "Education Level Distribution",
       x = "Education Level",
       y = "Count") +
  scale_x_discrete(labels = c("1" = "Less than 9th Grade", 
                              "2" = "9-11th Grade", 
                              "3" = "High School Grad/GED", 
                              "4" = "Some College/AA degree", 
                              "5" = "College Graduate or above", 
                              "7" = "Refused", 
                              "9" = "Don't Know")) +
  theme_classic()

#############################################

# Income data visualization

# Checking for negative values
summary(income$INDFMMPI)
income %>% filter(INDFMMPI < 0)  # Identifying rows with negative values

# Replacing negative values with NA
income$INDFMMPI <- ifelse(income$INDFMMPI < 0, NA, income$INDFMMPI)

install.packages("ggplot2")
library(ggplot2)


install.packages("gridExtra") 
library(gridExtra)

# Individual plots
plot_income <- ggplot(income, aes(x = INDFMMPI)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Family Monthly Poverty Index",
       x = "Poverty Index",
       y = "Frequency") +
  theme_classic()

plot_activity <- ggplot(physical_activities, aes(x = factor(PAQ655))) +
  geom_bar(fill = "darksalmon", color = "black") +
  labs(title = "Days of vigorous recreational activities", x = "Days", y = "Count") +
  scale_x_discrete(labels = c("1" = "0 days", "2" = "1-2 days", "3" = "3-4 days",
                              "4" = "5-6 days", "5" = "Every day")) +
  theme_classic()

plot_smoking <- ggplot(smoking, aes(x = factor(SMQ020))) +
  geom_bar(fill = "darkslategrey", color = "black") +
  labs(title = "Smoked at least 100 cigarettes in life", x = "Response", y = "Count") +
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No", "7" = "Refused", "9" = "Don't Know")) +
  theme_classic()

# Combining all three plots
grid.arrange(plot_income, plot_activity, plot_smoking, ncol = 2)


#############################################

#Combined plots for demographics

# Loading required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Cleaning and relabeling demographic variables
demographics <- demographics %>%
  mutate(
    RIAGENDR = factor(RIAGENDR, labels = c("Male", "Female")),
    RIDRETH3 = factor(RIDRETH3, labels = c(
      "Mexican American", "Other Hispanic", "Non-Hispanic White",
      "Non-Hispanic Black", "Asian", "Other Race/Multi-Racial"
    )),
    DMDEDUC2 = factor(DMDEDUC2, labels = c(
      "Less than 9th Grade", "9-11th Grade", "High School Graduate/GED",
      "Some College/AA Degree", "College Graduate or above",
      "Refused", "Don't Know"
    ))
  )

# Plot 1: Age Distribution
plot_age <- ggplot(demographics, aes(x = RIDAGEYR)) +
  geom_histogram(binwidth = 5, fill = "lightgrey", color = "black") +
  labs(title = "Age Distribution", x = "Age (Years)", y = "Frequency") +
  theme_classic()

# Plot 2: Gender Distribution
plot_gender <- ggplot(demographics, aes(x = RIAGENDR)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_classic()

# Plot 3: Race/Ethnicity Distribution (Classic theme with rotated labels)
plot_race <- ggplot(demographics, aes(x = RIDRETH3)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Race/Ethnicity Distribution", x = "Race/Ethnicity", y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Education Level Distribution (Classic theme with rotated labels)
plot_edu <- ggplot(demographics, aes(x = DMDEDUC2)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Education Level Distribution", x = "Education Level", y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combining all four plots into a grid
grid.arrange(plot_age, plot_gender, plot_race, plot_edu, ncol = 2)

#############################################

# Loading required libraries
library(ggplot2)
library(dplyr)

# Cleaning BMI Data (Removes missing values)
bmi_clean <- bmi %>%
  filter(!is.na(BMXBMI))

# Cleaning Blood Pressure Data (Removes missing values)
bp_clean <- blood_pressure %>%
  filter(!is.na(BPXOSY3) & !is.na(BPXODI3))

# Histogram for BMI Distribution
plot_bmi <- ggplot(bmi_clean, aes(x = BMXBMI)) +
  geom_histogram(binwidth = 1, fill = "blueviolet", color = "black") +
  labs(title = "BMI Distribution", x = "Body Mass Index (BMI)", y = "Frequency") +
  theme_classic()

# Histogram for Systolic Blood Pressure
plot_sys_bp <- ggplot(bp_clean, aes(x = BPXOSY3)) +
  geom_histogram(binwidth = 2, fill = "goldenrod1", color = "black") +
  labs(title = "Systolic Blood Pressure Distribution", x = "Systolic BP (mmHg)", y = "Frequency") +
  theme_classic()

# Histogram for Diastolic Blood Pressure
plot_dia_bp <- ggplot(bp_clean, aes(x = BPXODI3)) +
  geom_histogram(binwidth = 2, fill = "cadetblue3", color = "black") +
  labs(title = "Diastolic Blood Pressure Distribution", x = "Diastolic BP (mmHg)", y = "Frequency") +
  theme_classic()

# Loading gridExtra for combining plots
library(gridExtra)

# Combining BMI, Systolic BP, and Diastolic BP plots
grid.arrange(plot_bmi, plot_sys_bp, plot_dia_bp, ncol = 2)

#############################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Cleaning Total Cholesterol Data
total_chol_clean <- total_chol %>%
  filter(!is.na(LBXTC))  # Removes rows with missing cholesterol values

# Cleaning Fasting Glucose Data
fasting_glucose_clean <- fasting_glucose %>%
  filter(!is.na(LBXGLU))  # Removes rows with missing fasting glucose values

# Cleaning hs-CRP Data
hs_crp_clean <- hs_crp %>%
  filter(!is.na(LBXHSCRP))  # Removes rows with missing hs-CRP values

plot_chol <- ggplot(total_chol_clean, aes(x = LBXTC)) +
  geom_histogram(binwidth = 10, fill = "chocolate", color = "black") +
  labs(title = "Total Cholesterol Distribution",
       x = "Total Cholesterol (mg/dL)",
       y = "Frequency") +
  theme_classic()

plot_glucose <- ggplot(fasting_glucose_clean, aes(x = LBXGLU)) +
  geom_histogram(binwidth = 0.5, fill = "gold1", color = "black") +
  labs(title = "Fasting Glucose Distribution",
       x = "Fasting Glucose (mg/dL)",
       y = "Frequency") +
  theme_classic()

plot_crp <- ggplot(hs_crp_clean, aes(x = LBXHSCRP)) +
  geom_histogram(binwidth = 0.2, fill = "green4", color = "black") +
  labs(title = "hs-CRP Distribution",
       x = "hs-CRP (mg/L)",
       y = "Frequency") +
  theme_classic()

# Combining all plots into a grid
grid.arrange(plot_chol, plot_glucose, plot_crp, ncol = 2)
