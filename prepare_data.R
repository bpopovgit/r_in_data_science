## =============================================================================
## Data Preparation
## =============================================================================


# I) Script Setup --------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(janitor)
library(jsonlite)
library(skimr)
library(car)  # For VIF
library(corrr)  # For correlation analysis

source("Functions/explore_utils.R")
source("Functions/transform_utils.R")


# II) Constants ----------------------------------------------------------------
PATH_RAW <- "Data/Raw"
PATH_INTERIM <- "Data/Interim"
PATH_PROCESSED <- "Data/Processed"

TRANSACTION_DATA <- "transactions_data"
USER_DATA <- "users_data"
CARDS_DATA <- "cards_data"

MCC <- "mcc_codes"
FRAUD_TARGET <- "train_fraud_labels"

SEED = 121


# III) Combined data -----------------------------------------------------------

raw_transactions <- read_csv(file.path(PATH_RAW, paste0(TRANSACTION_DATA, ".csv")))
raw_fraud_target <- read_csv(file.path(PATH_RAW, paste0(FRAUD_TARGET, ".csv")))
raw_mcc <- read_csv(file.path(PATH_RAW, paste0(MCC, ".csv")))

raw_user <- read_csv(file.path(PATH_RAW, paste0(USER_DATA, ".csv")))
raw_cards <- read_csv(file.path(PATH_RAW, paste0(CARDS_DATA, ".csv")))

# 3.1) Account data side -----
accounts <- raw_cards %>% 
  rename(card_id = id) %>% 
  left_join(
    raw_user %>% rename(client_id = id),
    by="client_id"
  )

# 3.2) Transaction side -----
transactions <- raw_transactions %>% 
  rename(transaction_id = id, transaction_date = date, mcc_code = mcc) %>% 
  left_join(raw_fraud_target, by = "transaction_id") %>% 
  left_join(raw_mcc, by = "mcc_code") %>% 
  # keep only transaction_id-s with available fraud_flag, to be split before modeling
  filter(!is.na(fraud_flag))    # from 13,305,915 to 8,914,963 records
  
# 3.3) Combined data ------
combined <- transactions %>% 
  left_join(accounts, by = c("client_id", "card_id")) %>% 
  # Sort data by transaction_date before splitting
  arrange(transaction_date)

rm(
  accounts, transactions, 
  raw_transactions, raw_cards, raw_user, raw_mcc, raw_fraud_target
  )

# IV) Initial data split: modeling and holdout data (80-20) --------------------
set.seed(SEED)  # For reproducibility

evaluation_split <- initial_split(
  combined, 
  prop = 0.8,          # 80% Modeling, 20% Holdout
  strata = fraud_flag  # Maintain fraud ratio
  )

modeling_data <- training(evaluation_split)   # Used for training & validation
holdout_set <- testing(evaluation_split)      # Held-out for final testing

# Check fraud ratio
table(modeling_data$fraud_flag) / nrow(modeling_data)  # No 0.998514997 , Yes 0.001485003 
table(holdout_set$fraud_flag) / nrow(holdout_set)      # No 0.998462697,  Yes 0.001537303

# Exports
saveRDS(combined, file = file.path(PATH_INTERIM, "combined_raw.rds"))
saveRDS(modeling_data, file = file.path(PATH_INTERIM, "modeling_raw.rds"))
saveRDS(holdout_set, file = file.path(PATH_INTERIM, "holdout_raw.rds"))

# V) Modeling split: train and test data (80-20) -------------------------------
set.seed(SEED)  # Ensure reproducibility

train_test_split <- initial_split(
  modeling_data, 
  prop = 0.8,          # 80% Train, 20% Test
  strata = fraud_flag  # Maintain fraud ratio
)

train_data <- training(train_test_split)
test_data <- testing(train_test_split)

# Check fraud ratio in train/test
table(train_data$fraud_flag) / nrow(train_data)  # No 0.998520044, Yes 0.001479956 
table(test_data$fraud_flag) / nrow(test_data)    # No 0.998494806, Yes 0.001505194

# Exports
saveRDS(train_data, file = file.path(PATH_INTERIM, "train_raw.rds"))
saveRDS(test_data, file = file.path(PATH_INTERIM, "test_raw.rds"))

# Split summary:
# train_data	Model training (80% of modeling_data, 5,705,576 rows)
# test_data	Model validation (20% of modeling_data, 1,426,394 rows)
# holdout_set	Final evaluation (not touched until end), 1,782,993 rows

# VI) Data audit ---------------------------------------------------------------
audit_train_raw <- audit_data(train_data, "train_raw", export = TRUE)
audit_test_raw <- audit_data(test_data, "test_raw", export = TRUE)
audit_holdout <- audit_data(holdout_set, "holdout_raw", export = TRUE)
rm(audit_train_raw, audit_test_raw, audit_holdout)

# VII) Data cleaning and transformations ---------------------------------------

# Load cleaned training data
train_data <- readRDS(file.path(PATH_INTERIM, "train_raw.rds"))

# Define variables for processing
drop_cols <- c(
  "card_number",      # Sensitive data, not needed for modeling
  "cvv",              # Sensitive data, not needed for modeling
  "address",          # Too many unique values, irrelevant
  "zip",              # High cardinality, may not generalize well for fraud detection
  "errors",           # 98% missing
  "card_on_dark_web"  # Constant value, no variance
)  # Drop these
id_cols <- c(
  "transaction_id",
  "card_id",
  "client_id",
  "merchant_id",
  "mcc_code"
)
date_cols_to_transform <- c(
  "expires", 
  "acct_open_date"
)  # Convert to date info
date_cols <- c(
  "transaction_date"
)
binary_cols <- c(
  "fraud_flag",
  "has_chip", 
  "gender"
)   # Convert to binary 0/1
amount_cols <- c(
  "amount", 
  "credit_limit", 
  "per_capita_income", 
  "yearly_income", 
  "total_debt"
)  # Convert $ to numeric
numeric_cols <- c(
  "num_cards_issued", 
  "credit_score", 
  "num_credit_cards",
  "amount", 
  "credit_limit", 
  "per_capita_income", 
  "yearly_income", 
  "total_debt"
)  # Scale these
categorical_cols <- c(
  "use_chip",
  "card_brand",
  "card_type"
)
segmentation_cols <- c(
  "merchant_state",
  "merchant_city",
  "mcc_label",
  "latitude",
  "longitude",
  "gender",
  "birth_year",
  "birth_month",
  "current_age"
)
drop_cleaned <- c(
  "expires",
  "acct_open_date"
)


# Initial cleaning and transformations
train_data_interim <- train_data %>%
  select(-all_of(drop_cols)) %>%   # Drop unnecessary columns
  convert_to_date_info(date_cols_to_transform) %>%  # Convert date fields
  convert_to_proper_amount(amount_cols) %>%  # Convert $ fields to numeric
  convert_to_clean_binary(binary_cols) %>%  # Convert binary categorical to 0/1
  flag_mcc_risk_categories() %>%  # Create high-risk MCC categories
  compute_years_until_retirement() %>%  # Years remaining until retirement
  compute_account_age() %>%  # Account age
  compute_time_until_expiry() %>%  # Months until card expiration 
  compute_years_since_pin_change() %>%   # Years since last pin change
  compute_credit_utilization() %>%  # Available credit used
  compute_dti_ratio() %>%  # Debt to income ratio
  compute_days_since_last_txn() %>%  # Recency feature
  dummify_columns(categorical_cols, TRUE) %>%  # Convert categorical to dummies
  select(-any_of(c(drop_cleaned, segmentation_cols)))  # Drop original transformed date columns


# Target and predictors
# Separate target variable before VIF & Correlation analysis
target <- "fraud_flag"
id_cols <- c(
  "transaction_id",
  "card_id",
  "client_id",
  "merchant_id",
  "mcc_code"
)
predictors <- c(
  "amount",
  "has_chip",
  "num_cards_issued",
  "num_credit_cards",
  "credit_limit",
  "years_since_pin_change",
  "retirement_age",
  "years_until_retirement",
  "account_age_years",
  "months_until_expiry",
  "per_capita_income",
  "yearly_income",
  "total_debt",
  "credit_score",
  "credit_utilization_ratio",
  "debt_to_income_ratio",
  "days_since_last_txn",
  "use_chip_online_transaction",
  "use_chip_swipe_transaction",
  "card_brand_discover",
  "card_brand_mastercard",
  "card_brand_visa",
  "card_type_debit",
  "card_type_debit_prepaid",
  "mcc_gambling",
  "mcc_cash_advance",
  "mcc_luxury_goods",
  "mcc_electronics",
  "mcc_travel",
  "mcc_crypto",
  "mcc_subscriptions"
)
train_cleaned <- train_data_interim %>% 
  select(all_of(c(id_cols, target, predictors)))  # Save fraud flag

saveRDS(train_cleaned, file = file.path(PATH_PROCESSED, "train_clean.rds"))
audit_train_clean <- audit_data(train_cleaned, "train_clean", export = TRUE)

# --------------
# Inspect correlations against target
correlation_matrix <- train_cleaned %>%
  cor(use = "pairwise.complete.obs") %>% 
  flatten_correlation_matrix()


# Discard highly correlated predictors
train_cleaned_further <- train_cleaned %>%
  drop_highly_correlated(predictors, threshold = 0.90)
#  --------------


# XGBoost pre-processing steps -------------------------------------------------
# - Process test and holdout for XGBoost
test_data <- readRDS(file.path(PATH_INTERIM, "test_raw.rds"))
holdout_set <- readRDS(file.path(PATH_INTERIM, "holdout_raw.rds"))


# - Apply transformations for XGBoost
test_data_xgb <- process_data_xgb(test_data)
holdout_data_xgb <- process_data_xgb(holdout_set)

# - Save transformed datasets for XGBoost
saveRDS(test_data_xgb, file.path(PATH_PROCESSED, "test_clean_xgb.rds"))
saveRDS(holdout_data_xgb, file.path(PATH_PROCESSED, "holdout_clean_xgb.rds"))


# Tidymodels pre-processing steps ----------------------------------------------
# Apply feature dropping to training data
drop_results <- drop_highly_correlated(train_cleaned, predictors, threshold = 0.90)

# Save cleaned training data
train_data_tidymodels <- drop_results$cleaned_data
saveRDS(train_data_tidymodels, file.path(PATH_PROCESSED, "train_clean_tidymodels.rds"))

# Save the list of dropped features
saveRDS(drop_results$dropped_features, file.path(PATH_PROCESSED, "tidymodels_dropped_features.rds"))


# Apply transformations for TidyModels
dropped_features <- readRDS(file.path(PATH_PROCESSED, "tidymodels_dropped_features.rds"))
test_data_tidymodels <- process_data_tidymodels(test_data)
holdout_data_tidymodels <- process_data_tidymodels(holdout_set)

# Save transformed datasets for TidyModels
saveRDS(test_data_tidymodels, file.path(PATH_PROCESSED, "test_clean_tidymodels.rds"))
saveRDS(holdout_data_tidymodels, file.path(PATH_PROCESSED, "holdout_clean_tidymodels.rds"))


# audit_train_interim <- audit_data(train_data, "train_interim", export = TRUE)

# Check correlation with fraud_flag
# correlations <- compute_target_correlation(train_data, target = "fraud_flag") # needs fix
# print(correlations)

# Identify multicollinearity issues (VIF > 10)
# vif_results <- check_multicollinearity(train_data, threshold = 10)  # needs fix
# print(vif_results)

# Drop multicollinear features if necessary
# train_data <- train_data %>% 
#   select(-all_of(vif_results$variable))

# Scale numeric data
# train_data <- scale_numeric(train_data, numeric_cols, method = "standard")
# 
# # Feature engineering
# # TODO: add steps
# 
# # Tidy models recipe
# fraud_recipe <- recipe(fraud_flag ~ ., data = train_data) %>%
#   step_rm(all_of(id_cols)) %>% 
#   step_mutate(across(all_of(binary_cols), ~ ifelse(. == "Yes", 1, 0))) %>% 
#   step_normalize(all_numeric_predictors()) %>% 
#   step_dummy(all_nominal_predictors(), -all_outcomes())  # One-hot encode categorical variables
# 
# fraud_prep <- fraud_recipe %>%
#   prep(training = train_data, retain = TRUE)
# 
# train_data_clean <- juice(fraud_prep)

