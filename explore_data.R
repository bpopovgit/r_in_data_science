## =============================================================================
## Data Exploration
## =============================================================================


# I) Script Setup --------------------------------------------------------------
library(tidyverse)
library(janitor)
library(jsonlite)
library(skimr)

source("Functions/explore_utils.R")
source("Functions/transform_utils.R")


# II) Constants ----------------------------------------------------------------
PATH_RAW <- "Data/Raw"
PATH_INTERIM <- "Data/Interim"

TRANSACTION_DATA <- "transactions_data"
USER_DATA <- "users_data"
CARDS_DATA <- "cards_data"

MCC <- "mcc_codes"
FRAUD_TARGET <- "train_fraud_labels"


# III. Import raw data ---------------------------------------------------------


# 3.1) Merchant category codes ------
mcc_raw <- jsonlite::fromJSON(
    file.path(PATH_RAW, paste0(MCC, ".json"))
  ) %>% 
  as_tibble()

mcc_long <- mcc_raw %>% 
  pivot_longer(
    everything(), 
    names_to = "mcc_code",
    values_to = "mcc_label"
  ) %>% 
  mutate(mcc_code = as.numeric(mcc_code))

write_csv(mcc_long, file.path(PATH_RAW, paste0(MCC, ".csv"))) 
rm(mcc_raw)

# 3.2) Fraud target ------
target_labels_raw <- jsonlite::fromJSON(
    file.path(PATH_RAW, paste0(FRAUD_TARGET, ".json")),
    simplifyVector = FALSE     
  ) %>% 
  as_tibble() 

target_labels_clean <- target_labels_raw %>% 
  .$target %>%
  # Convert to tibble with proper structure
  enframe(name = "transaction_id", value = "fraud_flag") %>%
  # Convert value to character (removing list structure)
  mutate(fraud_flag = unlist(fraud_flag)) %>% 
  mutate(transaction_id = as.numeric(transaction_id))


write_csv(target_labels_clean, file.path(PATH_RAW, paste0(FRAUD_TARGET, ".csv"))) 
rm(target_labels_raw)

# 3.3) Transactions data ------
transactions_raw <- read_csv(file.path(PATH_RAW, paste0(TRANSACTION_DATA, ".csv")))


# 3.4) User data ------
user_raw <- read_csv(file.path(PATH_RAW, paste0(USER_DATA, ".csv")))


# 3.5) Card data ------
cards_raw <- read_csv(file.path(PATH_RAW, paste0(CARDS_DATA, ".csv")))



# IV. Explore data -------------------------------------------------------------

# Run data audit over raw data to identify basic transformations required
audit_transactions <- audit_data(transactions_raw, "transactions_raw", export = TRUE)
audit_cards <- audit_data(cards_raw, "cards_raw", export = TRUE)
audit_user <- audit_data(user_raw, "user_raw", export = TRUE)
audit_target <- audit_data(target_labels_clean, "target_labels", export = TRUE)

rm(audit_transactions, audit_cards, audit_user, audit_target)


# V. Transform data: basic -----------------------------------------------------

# 4.1) Combine transactions, target, and mcc ------
transactions_interim <- transactions_raw %>% 
  rename(transaction_id = id, transaction_date = date, mcc_code = mcc) %>% 
  # clean types
  convert_to_proper_amount(., c("amount")) %>% 
  # add target variable and mcc details
  left_join(target_labels_clean, by = "transaction_id") %>% 
  left_join(mcc_long, by = "mcc_code") %>% 
  # keep only transaction_id-s with available fraud_flag, to be split before modeling
  filter(!is.na(fraud_flag))  # from 13,305,915 to 8,914,963 records

write_csv(transactions_interim, file.path(PATH_INTERIM, "transactions_interim.csv"))
audit_transactions <- audit_data(transactions_interim, "transactions_interim", export = TRUE)
rm(audit_transactions)

# 4.2) Interim cards data ------
cards_interim <- cards_raw %>% 
  rename(card_id = id) %>% 
  # basic clean-up
  convert_to_proper_amount(
    ., c("credit_limit")
  ) %>% 
  convert_to_date_info(
    ., c("expires", "acct_open_date")
  ) %>% 
  convert_to_clean_binary(
    ., c("has_chip", "card_on_dark_web")
  )
audit_cards <- audit_data(cards_interim, "cards_interim", export = TRUE)
write_csv(cards_interim, file.path(PATH_INTERIM, "cards_interim.csv"))
rm(audit_cards)

# 4.3) Interim user data ------
user_interim <- user_raw %>% 
  rename(client_id = id) %>% 
  # basic clean-up
  convert_to_proper_amount(
    ., c("per_capita_income", "yearly_income", "total_debt")
  ) %>% 
  convert_to_clean_binary(
    ., c("gender")
  )
audit_user <- audit_data(user_interim, "user_interim", export = TRUE)
write_csv(user_interim, file.path(PATH_INTERIM, "user_interim.csv"))
rm(audit_user)


# 4.4) Combine card and user data into account data ------
account_interim <- cards_interim %>% 
  left_join(user_interim, by = "client_id")
  
write_csv(account_interim, file.path(PATH_INTERIM, "account_interim.csv"))
audit_account <- audit_data(account_interim, "account_interim", export = TRUE)
rm(audit_account)
