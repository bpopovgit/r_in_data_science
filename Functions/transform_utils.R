## =============================================================================
## Data Cleaning & Transformation Functions
## =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)


#' Convert Date-Like Strings to Date, Year, and Month
#'
#' This function converts columns containing date-like strings (formatted as "YYYY/MM") into proper date formats.
#' It extracts the full date (set to the first day of the month), the year, and the month as separate columns.
#'
#' @param df A data frame.
#' @param cols A character vector specifying the column names to be converted.
#' @return A data frame with additional columns:
#' \describe{
#'   \item{\code{<col>_date}}{Converted date in "YYYY-MM-DD" format (first of the month).}
#'   \item{\code{<col>_year}}{Extracted year as a numeric value.}
#'   \item{\code{<col>_month}}{Extracted month as a numeric value.}
#' }
#' @examples
#' df <- tibble(expiry = c("12/2022", "05/2021", "07/2019"))
#' convert_to_date_info(df, c("expiry"))
#' @export
convert_to_date_info <- function(df, cols) {
  # Treat string representation of YYYY/MM data
  output <- df %>% 
    mutate(
      across(
        all_of(cols), 
        ~lubridate::parse_date_time(., orders = "my"), .names = "{.col}_date")
      ,
      across(
        all_of(cols), 
        ~lubridate::year(lubridate::parse_date_time(., orders = "my")), .names = "{.col}_year")
      ,
      across(
        all_of(cols), 
        ~lubridate::month(lubridate::parse_date_time(., orders = "my")), .names = "{.col}_month")
    )
  
  return(output)
}


#' Convert Dollar Amount Strings to Numeric
#'
#' This function converts columns containing dollar amount strings (e.g., "$1,000") into numeric values.
#'
#' @param df A data frame.
#' @param cols A character vector specifying the column names to be converted.
#' @return A data frame where specified columns are converted to numeric.
#' @examples
#' df <- tibble(balance = c("$1,000", "$5,250", "$120.75"))
#' convert_to_proper_amount(df, c("balance"))
#' @export
convert_to_proper_amount <- function(df, cols){
  # Treat dollar amount columns with values like "$1000"
  output <- df %>% 
    mutate(
      across(all_of(cols), 
             ~ as.numeric(gsub("[$,]", "", .)), .names = "{.col}")
      )
  return(output)
}


#' Convert Binary Categorical Columns to 0/1
#'
#' This function standardizes binary categorical columns by converting values like "YES", "True", "Female", and "1" to `1`,
#' and values like "NO", "False", "Male", and "0" to `0`.
#'
#' @param df A data frame.
#' @param cols A character vector specifying the column names to be converted.
#' @return A data frame where specified columns are converted to numeric (0/1).
#' @examples
#' df <- tibble(has_chip = c("YES", "NO", "YES"), gender = c("Male", "Female", "Female"))
#' convert_to_clean_binary(df, c("has_chip", "gender"))
#' @export
convert_to_clean_binary <- function(df, cols){
  # Treat binary categorical columns - for uniform values
  output <- df %>% 
    mutate(across(
      all_of(cols), 
      ~ ifelse(. %in% c("YES", "Yes", "Female", "True", "T", "1"), 1, 0), 
      .names = "{.col}")
      )
  return(output)
}


#' Remove Unnecessary Columns
#'
#' This function drops specified columns that are not useful for fraud detection.
#'
#' @param df A data frame.
#' @param drop_cols A character vector specifying the column names to be removed.
#' @return A data frame without the specified columns.
#' @examples
#' df <- tibble(transaction_id = c(1,2,3), fraud_flag = c("Yes", "No", "No"))
#' remove_unwanted_columns(df, c("transaction_id"))
#' @export
remove_unwanted_columns <- function(df, drop_cols) {
  df %>%
    select(-all_of(drop_cols)) 
}

#' Convert Binary Categorical Columns to 0/1
#'
#' Standardizes binary categorical columns (e.g., "YES"/"NO", "Female"/"Male") by converting them into numeric format.
#'
#' @param df A data frame.
#' @param binary_cols A character vector specifying binary column names.
#' @return A data frame with specified columns converted to numeric (0/1).
#' @examples
#' df <- tibble(has_chip = c("YES", "NO", "YES"), gender = c("Male", "Female", "Female"))
#' convert_binary_features(df, c("has_chip", "gender"))
#' @export
convert_binary_features <- function(df, binary_cols) {
  df %>%
    mutate(across(all_of(binary_cols), 
                  ~ ifelse(. %in% c("YES", "Yes", "Female", "True", "T", "1"), 1, 0), 
                  .names = "{.col}_bin")) %>%
    select(-all_of(binary_cols))  
}

#' Dummify Categorical Variables
#'
#' This function uses `fastDummies::dummy_cols()` to create dummy variables
#' for selected categorical columns while keeping the original dataset structure.
#'
#' @param df A data frame.
#' @param cols A character vector of categorical column names to convert.
#' @param drop_first Logical; if TRUE, drops the first category for each column to avoid multicollinearity.
#' @return A data frame with dummy variables added.
#' @examples
#' df <- tibble(card_type = c("Debit", "Credit", "Prepaid", "Debit"))
#' dummify_columns(df, c("card_type"), drop_first = TRUE)
#' @export
dummify_columns <- function(df, cols, drop_first = FALSE) {
  df <- df %>% 
    mutate(across(
      all_of(cols), 
      ~ str_to_lower(.) %>%   # Convert to lowercase
        str_replace_all(" ", "_") %>%  # Replace spaces with underscores
        str_replace_all("[^a-z0-9_]", "") # Remove special characters
    )) %>%  # Convert values to snake_case
    fastDummies::dummy_cols(
      select_columns = cols, 
      remove_first_dummy = drop_first, 
      remove_selected_columns = TRUE
    )
  
  return(df)
}

#' Convert Dollar Amount Strings to Numeric
#'
#' This function converts columns containing currency values (e.g., "$1,000") into numeric format.
#'
#' @param df A data frame.
#' @param currency_cols A character vector specifying the column names to be converted.
#' @return A data frame where specified columns are converted to numeric.
#' @examples
#' df <- tibble(balance = c("$1,000", "$5,250", "$120.75"))
#' convert_currency_columns(df, c("balance"))
#' @export
convert_currency_columns <- function(df, currency_cols){
  df %>% 
    mutate(across(all_of(currency_cols), 
                  ~ as.numeric(gsub("[$,]", "", .)), .names = "{.col}_num")) %>%
    select(-all_of(currency_cols))
}

#' Convert Date Columns and Extract Features
#'
#' Converts date-like columns to proper date formats and extracts year, month, day, and weekday.
#'
#' @param df A data frame.
#' @param date_cols A character vector specifying the column names to be converted.
#' @return A data frame with extracted date components.
#' @examples
#' df <- tibble(transaction_date = c("2010-01-01", "2019-05-21"))
#' convert_dates(df, c("transaction_date"))
#' @export
convert_dates <- function(df, date_cols) {
  df %>%
    mutate(
      transaction_date = as_datetime(transaction_date),
      transaction_year = year(transaction_date),
      transaction_month = month(transaction_date),
      transaction_day = day(transaction_date),
      transaction_weekday = wday(transaction_date, label = TRUE),
      expires_year = as.numeric(sub("/.*", "", expires)),  
      expires_month = as.numeric(sub(".*/", "", expires)),  
      acct_open_year = as.numeric(sub("/.*", "", acct_open_date))
    ) %>%
    select(-all_of(date_cols))
}

#' Handle Missing Values
#'
#' This function fills missing categorical values with "Unknown" and imputes numeric missing values with the median.
#'
#' @param df A data frame.
#' @param categorical_cols A character vector of categorical columns.
#' @param numeric_cols A character vector of numeric columns.
#' @return A data frame with missing values handled.
#' @examples
#' df <- tibble(merchant_state = c("NY", NA, "CA"), zip = c(1001, NA, 3005))
#' impute_missing_values(df, c("merchant_state"), c("zip"))
#' @export
impute_missing_values <- function(df, categorical_cols, numeric_cols) {
  df %>%
    mutate(
      across(all_of(categorical_cols), ~ replace_na(., "Unknown")),
      across(all_of(numeric_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))
    )
}

#' Remove Near-Zero Variance Columns
#'
#' Removes columns with low variability (fewer unique values than the given threshold).
#'
#' @param df A data frame.
#' @param threshold Minimum number of unique values required.
#' @return A data frame with low-variance columns removed.
#' @examples
#' df <- tibble(feature1 = c(1,1,1,1), feature2 = c(1,2,3,4))
#' remove_near_zero_variance(df, threshold = 2)
#' @export
remove_near_zero_variance <- function(df, threshold = 1) {
  df %>%
    select(where(~ n_distinct(.) > threshold))
}

#' Normalize Numeric Data
#'
#' Normalizes specified numeric columns using mean and standard deviation.
#'
#' @param df A data frame.
#' @param numeric_cols A character vector specifying numeric columns.
#' @return A data frame with normalized numeric columns.
#' @examples
#' df <- tibble(score = c(10, 20, 30))
#' normalize_numeric(df, c("score"))
#' @export
normalize_numeric <- function(df, numeric_cols) {
  df %>%
    mutate(across(all_of(numeric_cols), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE), .names = "{.col}_scaled"))
}


#' Scale Numeric Columns
#'
#' This function scales numeric variables based on the specified method.
#' 
#' @param df A data frame.
#' @param cols A character vector specifying the numeric columns to scale.
#' @param method A string specifying the scaling method:
#'   - `"standard"`: Standardization (subtract mean, divide by standard deviation).
#'   - `"minmax"`: Min-Max Scaling (scale between 0 and 1).
#'   - `"robust"`: Robust Scaling (subtract median, divide by IQR).
#' @return A data frame with scaled numeric columns.
#' @examples
#' df <- tibble(amount = c(100, 200, 300, 400), score = c(50, 70, 90, 110))
#' scale_numeric(df, c("amount", "score"), method = "standard")
#' @export
scale_numeric <- function(df, cols, method = "standard") {
  if (!method %in% c("standard", "minmax", "robust")) {
    stop("Invalid method. Choose from 'standard', 'minmax', or 'robust'.")
  }
  
  output <- df %>% 
    mutate(
      across(all_of(cols), 
      ~ case_when(
        method == "standard" ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
        method == "minmax" ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
        method == "robust" ~ (. - median(., na.rm = TRUE)) / IQR(., na.rm = TRUE)
      ), 
      .names = "{.col}_scaled"))
  
  return(output)
}


#' Compute Correlation with Target Variable
#'
#' This function computes Spearman correlation for numeric features and fraud_flag.
#'
#' @param df A data frame.
#' @param target The name of the target variable (should be binary categorical or numeric 0/1).
#' @return A tibble with correlation values.
#' @export
compute_target_correlation <- function(df, target = "fraud_flag") {
  numeric_vars <- df %>% select(where(is.numeric)) %>% names()
  
  correlations <- df %>% 
    select(all_of(c(numeric_vars, target))) %>% 
    mutate(fraud_flag = as.numeric(factor(.[[target]])) - 1) %>% 
    cor(method = "spearman", use = "complete.obs") %>% 
    as_tibble(rownames = "variable") %>% 
    select(variable, fraud_flag) %>% 
    filter(variable != target) %>% 
    arrange(desc(abs(fraud_flag)))
  
  return(correlations)
}


#' Check Multicollinearity Using VIF
#'
#' This function calculates the Variance Inflation Factor (VIF) for numeric variables.
#' Features with high VIF (e.g., > 10) indicate multicollinearity.
#'
#' @param df A data frame.
#' @param threshold The cutoff for VIF (default is 10).
#' @return A tibble with VIF values.
#' @export
check_multicollinearity <- function(df, threshold = 10) {
  numeric_vars <- df %>% select(where(is.numeric)) %>% names()
  
  vif_values <- car::vif(lm(paste("fraud_flag ~", paste(numeric_vars, collapse = " + ")), data = df))
  
  vif_df <- tibble(variable = names(vif_values), vif = vif_values) %>% 
    arrange(desc(vif)) %>% 
    filter(vif > threshold)  # Only return problematic features
  
  return(vif_df)
}

#' Compute Years Until Retirement
#'
#' This function calculates the number of years remaining until retirement 
#' based on the `current_age` and `retirement_age` columns.
#'
#' @param df A data frame containing the `current_age` and `retirement_age` columns.
#' @return A data frame with a new column:
#' \describe{
#'   \item{\code{years_until_retirement}}{The difference between `retirement_age` and `current_age`, 
#'   representing the number of years remaining until retirement.}
#' }
#' @examples
#' df <- tibble(current_age = c(30, 50, 60), retirement_age = c(65, 67, 70))
#' compute_years_until_retirement(df)
#' @export
compute_years_until_retirement <- function(df) {
  df <- df %>%
    mutate(years_until_retirement = retirement_age - current_age)
  
  return(df)
}

#' Compute Account Age
#'
#' This function calculates the age of the account in years at the time of each transaction.
#'
#' @param df A data frame containing the `acct_open_date_year` and `transaction_date` columns.
#' @return A data frame with a new column:
#' \describe{
#'   \item{\code{account_age_years}}{The number of years the account has been active at the time of each transaction.}
#' }
#' @examples
#' df <- tibble(acct_open_date_year = c(2005, 2010, 2015), transaction_date = as.Date(c("2020-06-01", "2021-08-15", "2019-03-10")))
#' compute_account_age(df)
#' @export
compute_account_age <- function(df) {
  df <- df %>%
    mutate(account_age_years = lubridate::year(transaction_date) - acct_open_date_year)
  
  return(df)
}

#' Compute Time Until Card Expiry
#'
#' This function calculates the number of months remaining until a card expires at the time of each transaction.
#'
#' @param df A data frame containing the `expires_date` and `transaction_date` columns.
#' @return A data frame with a new column:
#' \describe{
#'   \item{\code{months_until_expiry}}{The number of months remaining until the card expires at the time of each transaction.}
#' }
#' @examples
#' df <- tibble(
#'   expires_date = as.Date(c("2025-06-01", "2023-12-01", "2024-09-01")),
#'   transaction_date = as.Date(c("2022-06-01", "2023-08-15", "2024-03-10"))
#' )
#' compute_time_until_expiry(df)
#' @export
compute_time_until_expiry <- function(df) {
  df <- df %>%
    mutate(months_until_expiry = lubridate::interval(transaction_date, expires_date) / months(1))
  
  return(df)
}


#' Compute Debt-to-Income (DTI) Ratio
#'
#' This function calculates the Debt-to-Income ratio as (total_debt / yearly_income).
#'
#' @param df A data frame with `total_debt` and `yearly_income` as numeric columns.
#' @return A data frame with a new `debt_to_income_ratio` column.
#' @export
compute_dti_ratio <- function(df) {
  df <- df %>%
    mutate(
      debt_to_income_ratio = ifelse(
        yearly_income > 0, 
        total_debt / yearly_income, 
        NA  # Avoid division by zero
      )
    )
  return(df)
}

#' Compute Credit Utilization Ratio
#'
#' Measures how much of the available credit is used.
#'
#' @param df A data frame with `credit_limit` and `total_debt` as numeric columns.
#' @return A data frame with a new `credit_utilization_ratio` column.
#' @export
compute_credit_utilization <- function(df) {
  df <- df %>%
    mutate(
      credit_utilization_ratio = ifelse(
        credit_limit > 0, 
        (credit_limit - total_debt) / credit_limit, 
        0
      )
    )
  return(df)
}


#' Flag High-Risk MCC Categories
#'
#' This function creates binary flags for specific high-risk Merchant Category Codes (MCCs), 
#' categorizing them into different spending types such as gambling, cash advances, luxury goods, etc.
#'
#' @param df A data frame containing transaction data.
#' @param mcc_col The column name containing MCC codes (default: "mcc_code").
#' @return A data frame with additional binary columns indicating high-risk MCC categories:
#' \describe{
#'   \item{\code{mcc_gambling}}{Flag for gambling transactions (e.g., casinos, betting).}
#'   \item{\code{mcc_cash_advance}}{Flag for cash advances and wire transfers.}
#'   \item{\code{mcc_luxury_goods}}{Flag for high-end retail purchases.}
#'   \item{\code{mcc_electronics}}{Flag for electronics and computer stores.}
#'   \item{\code{mcc_travel}}{Flag for airline, hotel, and travel agencies.}
#'   \item{\code{mcc_crypto}}{Flag for cryptocurrency and digital wallet transactions.}
#'   \item{\code{mcc_subscriptions}}{Flag for recurring subscription-based services.}
#' }
#' @examples
#' df <- tibble(mcc_code = c(7995, 6011, 5944, 5732, 4511, 4900, 5968))
#' df <- flag_mcc_risk_categories(df)
#' @export
flag_mcc_risk_categories <- function(df, mcc_col = "mcc_code") {
  
  df <- df %>%
    mutate(
      # Gambling MCCs: Casinos, Betting, Lottery, etc.
      mcc_gambling = ifelse(
        .data[[mcc_col]] %in% c(
          7995,  # Betting (Casino, Lottery, Online Gambling)
          7800,  # Government Lottery
          7801,  # Online Gaming/Casino
          7802,  # Internet Gambling
          7803   # Horse Racing Betting
        ), 1, 0),
      
      # Cash Advance MCCs: ATM, Wire Transfers, Check Cashing
      mcc_cash_advance = ifelse(
        .data[[mcc_col]] %in% c(
          6010,  # Financial Institutions – Manual Cash Disbursement
          6011,  # ATM Cash Withdrawals
          6012,  # Merchandise and Services (Financial Institutions)
          4829,  # Money Transfers (Western Union, etc.)
          6051   # Digital Wallet Cash Load
        ), 1, 0),
      
      # Luxury Goods MCCs: High-End Retail, Jewelry, Department Stores
      mcc_luxury_goods = ifelse(
        .data[[mcc_col]] %in% c(
          5944,  # Jewelry, Watches, and Precious Stones
          5999,  # Miscellaneous & Specialty Retail Stores
          5309   # Duty-Free Stores (Luxury Shopping)
        ), 1, 0),
      
      # Electronics MCCs: Consumer Electronics and Computer Stores
      mcc_electronics = ifelse(
        .data[[mcc_col]] %in% c(
          5732,  # Electronics Stores
          5734,  # Computer Software Stores
          5045   # Computers, Peripherals, and Office Equipment
        ), 1, 0),
      
      # Travel MCCs: Airlines, Hotels, and Travel Agencies
      mcc_travel = ifelse(
        .data[[mcc_col]] %in% c(
          4511,  # Airlines, Air Carriers
          4722,  # Travel Agencies and Tour Operators
          7011   # Hotels, Motels, and Resorts
        ), 1, 0),
      
      # Cryptocurrency & Digital Wallet MCCs
      mcc_crypto = ifelse(
        .data[[mcc_col]] %in% c(
          4900,  # Utilities (Also used for Crypto Transactions)
          6050,  # Prepaid Card Loads (Often used in Crypto)
          6051   # Digital Wallet Transactions (Linked to Crypto)
        ), 1, 0),
      
      # Subscriptions MCCs: Recurring Payments for Online Services
      mcc_subscriptions = ifelse(
        .data[[mcc_col]] %in% c(
          5968,  # Subscription Services (e.g., Streaming Platforms)
          7273   # Dating & Adult Content Subscriptions
        ), 1, 0)
    )
  
  return(df)
}


#' Compute Days Since Last Transaction
#'
#' This function calculates the number of days since the last recorded transaction 
#' for each card, ensuring that each transaction only uses past data.
#'
#' @param df A data frame containing transaction data.
#' @return A data frame with an additional column:
#' \describe{
#'   \item{\code{days_since_last_txn}}{Number of days since the last transaction.}
#' }
#' @examples
#' df <- tibble(card_id = c(1,1,2,2), transaction_date = as.Date(c("2023-01-01", "2023-01-05", "2023-02-01", "2023-02-10")))
#' df <- compute_days_since_last_txn(df)
#' @export
compute_days_since_last_txn <- function(df) {
  # Ensures each transaction only uses past data.
  df <- df %>%
    group_by(card_id) %>%
    arrange(transaction_date) %>%
    mutate(
      days_since_last_txn = as.numeric(
        difftime(
          transaction_date, 
          lag(transaction_date, default = first(transaction_date)), 
          units = "days"))
    ) %>%
    ungroup()
  
  return(df)
}

#' Compute Years Since Last PIN Change
#'
#' This function calculates the number of years since the PIN was last changed 
#' based on the transaction date. A longer duration since the last change 
#' could indicate a higher risk of fraud.
#'
#' @param df A data frame containing `transaction_date` and `year_pin_last_changed`.
#' @return A data frame with an additional column:
#' \describe{
#'   \item{\code{years_since_pin_change}}{The number of years between the transaction date and the last PIN change.}
#' }
#' @examples
#' df <- tibble(transaction_date = as.Date(c("2023-01-01", "2021-05-10")),
#'              year_pin_last_changed = c(2018, 2015))
#' compute_years_since_pin_change(df)
#' @export
compute_years_since_pin_change <- function(df) {
  df <- df %>%
    mutate(
      years_since_pin_change = lubridate::year(transaction_date) - year_pin_last_changed
    )
  return(df)
}

#' Compute Transaction Count in a Rolling Window
#'
#' This function calculates the number of transactions per card within a given 
#' rolling time window to track transaction frequency.
#'
#' @param df A data frame containing transaction data.
#' @param window_days Integer indicating the time window in days (default: 7).
#' @return A data frame with an additional column:
#' \describe{
#'   \item{\code{txn_count_last_X_days}}{Number of transactions within the last X days.}
#' }
#' @examples
#' df <- tibble(card_id = c(1,1,2,2), transaction_date = as.Date(c("2023-01-01", "2023-01-05", "2023-02-01", "2023-02-10")))
#' df <- compute_txn_count_window(df, window_days = 7)
#' @export
compute_txn_count_window <- function(df, window_days = 7) {
  # Uses rolling window to prevent future data from influencing earlier transactions.
  df <- df %>%
    group_by(card_id) %>%
    arrange(transaction_date) %>%
    mutate(txn_count_last_X_days = map_dbl(row_number(), ~ sum(transaction_date[.x] - transaction_date <= window_days, na.rm = TRUE))) %>%
    ungroup()
  
  return(df)
}

#' Compute Unique Merchants in a Rolling Window
#'
#' This function calculates the number of unique merchants a cardholder 
#' has transacted with over a given rolling time window.
#'
#' @param df A data frame containing transaction data.
#' @param window_days Integer indicating the time window in days (default: 30).
#' @return A data frame with an additional column:
#' \describe{
#'   \item{\code{unique_merchants_last_X_days}}{Number of distinct merchants within the last X days.}
#' }
#' @examples
#' df <- tibble(card_id = c(1,1,2,2), transaction_date = as.Date(c("2023-01-01", "2023-01-05", "2023-02-01", "2023-02-10")), merchant_id = c(100, 101, 102, 102))
#' df <- compute_unique_merchants(df, window_days = 30)
#' @export
compute_unique_merchants <- function(df, window_days = 30) {
  # Fraudsters often use many merchants; this tracks that over time.
  df <- df %>%
    group_by(card_id) %>%
    arrange(transaction_date) %>%
    mutate(unique_merchants_last_X_days = map_dbl(
      row_number(), 
      ~ n_distinct(merchant_id[transaction_date[.x] - transaction_date <= window_days]))
    ) %>%
    ungroup()
  
  return(df)
}

#' Compute Spending Behavior in a Rolling Window
#'
#' This function calculates the average and standard deviation of transaction 
#' amounts within a given rolling time window to capture spending patterns.
#'
#' @param df A data frame containing transaction data.
#' @param window_days Integer indicating the time window in days (default: 7).
#' @return A data frame with two additional columns:
#' \describe{
#'   \item{\code{avg_amount_last_X_days}}{Average transaction amount over the last X days.}
#'   \item{\code{std_amount_last_X_days}}{Standard deviation of transaction amounts over the last X days.}
#' }
#' @examples
#' df <- tibble(card_id = c(1,1,2,2), transaction_date = as.Date(c("2023-01-01", "2023-01-05", "2023-02-01", "2023-02-10")), amount = c(100, 150, 200, 250))
#' df <- compute_spending_behavior(df, window_days = 7)
#' @export
compute_spending_behavior <- function(df, window_days = 7) {
  df <- df %>%
    group_by(card_id) %>%
    arrange(transaction_date) %>%
    mutate(
      avg_amount_last_X_days = map_dbl(row_number(), ~ mean(amount[transaction_date[.x] - transaction_date <= window_days], na.rm = TRUE)),
      std_amount_last_X_days = map_dbl(row_number(), ~ sd(amount[transaction_date[.x] - transaction_date <= window_days], na.rm = TRUE))
    ) %>%
    ungroup()
  
  return(df)
}

#' Compute Variance Inflation Factor (VIF)
#'
#' This function calculates VIF scores for numerical predictors to detect multicollinearity.
#'
#' @param df A data frame containing only predictor variables.
#' @param exclude_vars A character vector of columns to exclude (e.g., target, ID columns).
#' @return A tibble with variable names and their VIF scores.
#' @examples
#' vif_results <- compute_vif(train_data_features, exclude_vars = c("fraud_flag", "transaction_id", "card_id"))
#' @export
compute_vif <- function(df, exclude_vars = NULL) {
  # Select only numeric predictors, excluding specified variables
  df_numeric <- df %>%
    select(where(is.numeric), -any_of(exclude_vars))
  
  # Fit a linear model
  formula <- as.formula(paste(" ~ ", paste(names(df_numeric), collapse = " + ")))  # Regression formula
  vif_values <- car::vif(lm(formula, data = df_numeric))  # Compute VIF
  
  # Convert to tibble for easy inspection
  vif_df <- tibble(
    variable = names(vif_values),
    vif_score = as.numeric(vif_values)
  ) %>%
  arrange(desc(vif_score))  # Sort by highest VIF
  
  return(vif_df)
}



#' Flatten Correlation Matrix
#'
#' This function converts a correlation matrix into a long-format data frame,
#' making it easier to inspect variable relationships.
#'
#' @param cor_matrix A correlation matrix.
#' @return A tibble with variable pairs and their correlation values.
#' @examples
#' cor_matrix <- compute_correlation_matrix(train_data_features, id_cols = c("transaction_id", "card_id"))
#' flatten_correlation_matrix(cor_matrix)
#' @export
flatten_correlation_matrix <- function(cor_matrix) {
  cor_df <- as_tibble(cor_matrix, rownames = "variable1") %>%
    pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") %>%
    filter(variable1 != variable2) %>%  # Remove self-correlations (correlation = 1)
    arrange(desc(abs(correlation)))  # Sort by absolute correlation strength
  
  return(cor_df)
}


# Function to drop highly correlated features
drop_highly_correlated <- function(df, predictors, threshold = 0.97) {
  
  # Select only numeric predictors
  num_predictors <- df %>% select(all_of(predictors)) %>% select(where(is.numeric))
  
  # Compute correlation matrix (handling missing values)
  cor_matrix <- Hmisc::rcorr(as.matrix(num_predictors))$r
  
  # Convert correlation matrix to long format
  cor_df <- cor_matrix %>%
    as_tibble(rownames = "variable1") %>%
    pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") %>%
    filter(variable1 != variable2, abs(correlation) > threshold) %>%
    arrange(desc(abs(correlation)))
  
  # Identify variables to drop (keep only one from each highly correlated pair)
  to_drop <- cor_df %>%
    group_by(variable1) %>%
    slice(1) %>%
    pull(variable2) %>%
    unique()
  
  message("Dropping highly correlated features: ", paste(to_drop, collapse = ", "))
  
  return(list(cleaned_data = df %>% select(-all_of(to_drop)), dropped_features = to_drop))
}


process_data_xgb <- function(data) {
  data %>%
    select(-all_of(drop_cols)) %>%
    convert_to_date_info(date_cols_to_transform) %>%
    convert_to_proper_amount(amount_cols) %>%
    convert_to_clean_binary(binary_cols) %>%
    flag_mcc_risk_categories() %>%
    compute_years_until_retirement() %>%
    compute_account_age() %>%
    compute_time_until_expiry() %>%
    compute_years_since_pin_change() %>%
    compute_credit_utilization() %>%
    compute_dti_ratio() %>%
    compute_days_since_last_txn() %>%
    dummify_columns(categorical_cols, TRUE) %>%
    select(-any_of(c(drop_cleaned, segmentation_cols)))  # Keep all features
}


# Function to apply transformations AND remove highly correlated features
process_data_tidymodels <- function(data) {
  data %>%
    select(-all_of(drop_cols)) %>%
    convert_to_date_info(date_cols_to_transform) %>%
    convert_to_proper_amount(amount_cols) %>%
    convert_to_clean_binary(binary_cols) %>%
    flag_mcc_risk_categories() %>%
    compute_years_until_retirement() %>%
    compute_account_age() %>%
    compute_time_until_expiry() %>%
    compute_years_since_pin_change() %>%
    compute_credit_utilization() %>%
    compute_dti_ratio() %>%
    compute_days_since_last_txn() %>%
    dummify_columns(categorical_cols, TRUE) %>%
    select(-any_of(c(drop_cleaned, segmentation_cols))) %>%
    select(-all_of(dropped_features))  # Drop the same features as in training
}

