# =============================================================================
# 0) Setup
# =============================================================================
library(tidyverse)
library(tidymodels)
library(xgboost)
library(yardstick)
library(themis)
library(ranger)
library(glmnet)
library(plotly)
library(ggplot2)
library(precrec)
library(vip)
library(pROC)
library(dplyr)


# Set seed for reproducibility
SEED <- 121
set.seed(SEED)

# Define file paths
PATH_PROCESSED <- "Data/Processed"


# =============================================================================
# 1) Load Preprocessed Data
# =============================================================================
# Load XGBoost datasets (Keeps all features)
train_xgb <- readRDS(file.path(PATH_PROCESSED, "train_clean.rds"))
test_xgb <- readRDS(file.path(PATH_PROCESSED, "test_clean_xgb.rds"))
holdout_xgb <- readRDS(file.path(PATH_PROCESSED, "holdout_clean_xgb.rds"))

# Load TidyModels datasets (Drops highly correlated features)
train_tidymodels <- readRDS(file.path(PATH_PROCESSED, "train_clean_tidymodels.rds"))
test_tidymodels <- readRDS(file.path(PATH_PROCESSED, "test_clean_tidymodels.rds"))
holdout_tidymodels <- readRDS(file.path(PATH_PROCESSED, "holdout_clean_tidymodels.rds"))

# Define target variable and ID columns
target_var <- "fraud_flag"
id_cols <- c("transaction_id", "card_id", "client_id", "merchant_id", "mcc_code")


# Ensure test & holdout sets only contain the same features as training set
common_columns <- intersect(names(train_xgb), names(test_xgb))

test_xgb <- test_xgb %>%
  select(all_of(common_columns))

holdout_xgb <- holdout_xgb %>%
  select(all_of(common_columns))


# Ensure test & holdout sets only contain the same features as training set
#common_columns <- intersect(names(train_tidymodels), names(test_tidymodels))

#test_tidymodels <- test_tidymodels %>%
  #select(all_of(common_columns))

#holdout_xgb <- holdout_xgb %>%
  #select(all_of(common_columns))

# === TidyModels datasets ===
common_columns_tidymodels <- intersect(names(train_tidymodels), names(test_tidymodels))

test_tidymodels <- test_tidymodels %>%
  select(all_of(common_columns_tidymodels))

holdout_tidymodels <- holdout_tidymodels %>%
  select(all_of(common_columns_tidymodels))


# === XGBoost datasets ===
common_columns_xgb <- intersect(names(train_xgb), names(test_xgb))

test_xgb <- test_xgb %>%
  select(all_of(common_columns_xgb))

holdout_xgb <- holdout_xgb %>%
  select(all_of(common_columns_xgb))


# =============================================================================
# 2) Train Models
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1) Train XGBoost Model (Exclude ID Columns)
# -----------------------------------------------------------------------------
scale_pos_weight_value <- nrow(train_xgb[train_xgb$fraud_flag == 0, ]) / 
  nrow(train_xgb[train_xgb$fraud_flag == 1, ])

dtrain <- xgb.DMatrix(
  data = as.matrix(select(train_xgb, -c(all_of(id_cols), fraud_flag))),
  label = train_xgb[[target_var]]
)

dtest <- xgb.DMatrix(
  data = as.matrix(select(test_xgb, -c(all_of(id_cols), fraud_flag))),
  label = test_xgb[[target_var]]
)

dholdout <- xgb.DMatrix(
  data = as.matrix(select(holdout_xgb, -c(all_of(id_cols), fraud_flag))),
  label = holdout_xgb[[target_var]]
)

# Define XGBoost parameters with cross-validation
params <- list(
  objective = "binary:logistic", 
  eval_metric = "auc",
  eta = 0.02,                     # Higher learning rate for faster training
  max_depth = 5,                  # Smaller trees for speed
  subsample = 0.6,                 # Reduce subsample for speed
  colsample_bytree = 0.6,          # Reduce feature sampling for efficiency
  scale_pos_weight = scale_pos_weight_value  # Adjust for class imbalance
)

# Perform Cross-Validation
cv_results <- xgb.cv(
  params = params, 
  data = dtrain, 
  nfold = 5,                         # Reduce folds for faster CV
  nrounds = 200,                      # Lower number of rounds
  stratified = TRUE,                   # Stratify by fraud_flag
  early_stopping_rounds = 50,          # Stop early if no improvement
  verbose = 1
)
saveRDS(cv_results, file.path(PATH_PROCESSED, "xgb_cv.rds"))

# Extract best number of rounds from cross-validation
best_nrounds <- cv_results$best_iteration

# Train Final XGBoost Model using Best Rounds
xgb_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = best_nrounds,
  verbose = 1
)

# Save XGBoost Model
saveRDS(xgb_model, file.path(PATH_PROCESSED, "xgb_model.rds"))

# =============================================================================
# 2.2) Train Logistic Regression & Random Forest Models (Optimized with Cross-Validation)
# =============================================================================

train_tidymodels <- train_tidymodels %>%
  mutate(fraud_flag = as.factor(fraud_flag))

test_tidymodels <- test_tidymodels %>%
  mutate(fraud_flag = as.factor(fraud_flag))

holdout_tidymodels <- holdout_tidymodels %>%
  mutate(fraud_flag = as.factor(fraud_flag))

# -----------------------------------------------------------------------------
# 1) Define Cross-Validation with Fewer Folds (v = 3)
# -----------------------------------------------------------------------------
cv_folds <- vfold_cv(train_tidymodels, v = 3, strata = fraud_flag)  # Reduced from v=5 to v=3

# -----------------------------------------------------------------------------
# 2) Define Recipe with Downsampling Instead of SMOTE
# -----------------------------------------------------------------------------
fraud_recipe <- recipe(fraud_flag ~ ., data = train_tidymodels) %>%
  step_rm(all_of(id_cols)) %>%  # Remove ID columns
  step_zv(all_numeric_predictors()) %>%  # Remove zero-variance columns
  step_corr(all_numeric_predictors(), threshold = 0.7) %>%  # Remove highly correlated features
  step_downsample(fraud_flag) %>%  # Faster than SMOTE
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes())

# -----------------------------------------------------------------------------
# 3) Train Logistic Regression Model with Cross-Validation
# -----------------------------------------------------------------------------
log_reg_model <- logistic_reg(penalty = 0.001, mixture = 0.5) %>%  
  set_engine("glmnet") %>%
  set_mode("classification") 

log_reg_wf <- workflow() %>%
  add_model(log_reg_model) %>%
  add_recipe(fraud_recipe)

log_reg_results <- fit_resamples(
  log_reg_wf,
  resamples = cv_folds,
  metrics = metric_set(roc_auc, accuracy, precision, recall, f_meas),
  control = control_resamples(save_pred = TRUE)
)

saveRDS(log_reg_results, file.path(PATH_PROCESSED, "cv_log_reg_results.rds"))

# -----------------------------------------------------------------------------
# 4) Train Random Forest Model with Cross-Validation
# -----------------------------------------------------------------------------
rf_model <- rand_forest(
  trees = 50,        # Reduced from 500 to 50 trees for speed
  mtry = tune(),     # Tune `mtry`, but keep it simple
  min_n = 10        # Minimum observations per leaf
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_wf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(fraud_recipe)

# Optimize `mtry` with a very small grid search (Faster than default tuning)
rf_tune_results <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid = tibble(mtry = c(3, 5, 7)),  # Instead of full search, test 3 values
  metrics = metric_set(roc_auc),
  control = control_grid(save_pred = TRUE)
)

best_params <- select_best(rf_tune_results, metric = "roc_auc")

rf_final_model <- finalize_model(rf_model, best_params)


rf_final_wf <- workflow() %>%
  add_model(rf_final_model) %>%
  add_recipe(fraud_recipe)

rf_results <- fit_resamples(
  rf_final_wf,
  resamples = cv_folds,
  metrics = metric_set(roc_auc, accuracy, precision, recall, f_meas),
  control = control_resamples(save_pred = TRUE)
)

saveRDS(rf_results, file.path(PATH_PROCESSED, "cv_rf_results.rds"))

# -----------------------------------------------------------------------------
# 5) Extract Performance Metrics Across Folds
# -----------------------------------------------------------------------------
log_reg_metrics <- collect_metrics(log_reg_results) %>% mutate(model = "Logistic Regression")
rf_metrics <- collect_metrics(rf_results) %>% mutate(model = "Random Forest")

# Combine CV results for comparison
cv_comparison <- bind_rows(log_reg_metrics, rf_metrics)

# Print Cross-Validation Results
print(cv_comparison)

# Save Cross-Validation Results
write_csv(cv_comparison, file.path(PATH_PROCESSED, "cv_results_tidymodels.csv"))

# -----------------------------------------------------------------------------
# 6) Train Final Models on Full Dataset
# -----------------------------------------------------------------------------
log_reg_fit <- fit(log_reg_wf, data = train_tidymodels)
rf_fit <- fit(rf_final_wf, data = train_tidymodels)

# Save Final Models
saveRDS(log_reg_fit, file.path(PATH_PROCESSED, "log_reg_model.rds"))
saveRDS(rf_fit, file.path(PATH_PROCESSED, "rf_model.rds"))

# =============================================================================
# 3) Evaluate and Compare All Models (XGBoost, Logistic Regression, Random Forest)
# =============================================================================

cv_results = readRDS(file.path(PATH_PROCESSED, "xgb_cv.rds"))

xgb_model = readRDS(file.path(PATH_PROCESSED, "xgb_model.rds"))

log_reg_fit = readRDS(file.path(PATH_PROCESSED, "log_reg_model.rds"))

rf_fit = readRDS(file.path(PATH_PROCESSED, "rf_model.rds"))

evaluate_all_models <- function(xgb_model, log_reg_fit, rf_fit, 
                                test_xgb, holdout_xgb, 
                                test_tidymodels, holdout_tidymodels,
                                threshold = 0.5) {
  
  id_cols <- c("transaction_id", "card_id", "client_id", "merchant_id", "mcc_code")
  
  metrics_all <- list()
  test_preds_list <- list()
  holdout_preds_list <- list()
  
  models <- list(
    "XGBoost" = list(model = xgb_model, test_data = test_xgb, holdout_data = holdout_xgb, type = "xgboost"),
    "Logistic Regression" = list(model = log_reg_fit, test_data = test_tidymodels, holdout_data = holdout_tidymodels, type = "tidymodels"),
    "Random Forest" = list(model = rf_fit, test_data = test_tidymodels, holdout_data = holdout_tidymodels, type = "tidymodels")
  )
  
  for (model_name in names(models)) {
    model_info <- models[[model_name]]
    model_fit <- model_info$model
    test_data <- model_info$test_data
    holdout_data <- model_info$holdout_data
    model_type <- model_info$type
    
    # ==== Predictions ====
    if (model_type == "xgboost") {
      dtest <- xgb.DMatrix(data = as.matrix(select(test_data, -all_of(c(id_cols, "fraud_flag")))))
      dholdout <- xgb.DMatrix(data = as.matrix(select(holdout_data, -all_of(c(id_cols, "fraud_flag")))))
      
      test_preds <- predict(model_fit, dtest)
      holdout_preds <- predict(model_fit, dholdout)
      
      test_df <- test_data %>%
        select(all_of(id_cols), fraud_flag) %>%
        mutate(.pred_1 = test_preds,
               .pred_class = as.factor(if_else(.pred_1 >= threshold, "1", "0")))
      
      holdout_df <- holdout_data %>%
        select(all_of(id_cols), fraud_flag) %>%
        mutate(.pred_1 = holdout_preds,
               .pred_class = as.factor(if_else(.pred_1 >= threshold, "1", "0")))
      
    } else {
      test_df <- predict(model_fit, new_data = test_data, type = "prob") %>%
        bind_cols(predict(model_fit, new_data = test_data, type = "class") %>% rename(.pred_class = .pred_class)) %>%
        bind_cols(test_data %>% select(all_of(id_cols), fraud_flag))
      
      holdout_df <- predict(model_fit, new_data = holdout_data, type = "prob") %>%
        bind_cols(predict(model_fit, new_data = holdout_data, type = "class") %>% rename(.pred_class = .pred_class)) %>%
        bind_cols(holdout_data %>% select(all_of(id_cols), fraud_flag))
    }
    
    test_df <- test_df %>%
      mutate(fraud_flag = as.factor(fraud_flag),
             .pred_class = as.factor(.pred_class))
    
    holdout_df <- holdout_df %>%
      mutate(fraud_flag = as.factor(fraud_flag),
             .pred_class = as.factor(.pred_class))
    
    # ==== Evaluation ====
    test_metrics <- tibble(
      model = model_name,
      dataset = "Test Set",
      metric = c("ROC_AUC", "PR_AUC", "F1_Score", "Precision", "Recall"),
      value = c(
        roc_auc(test_df, truth = fraud_flag, .pred_1)$.estimate,
        pr_auc(test_df, truth = fraud_flag, .pred_1)$.estimate,
        f_meas(test_df, truth = fraud_flag, estimate = .pred_class)$.estimate,
        precision(test_df, truth = fraud_flag, estimate = .pred_class)$.estimate,
        recall(test_df, truth = fraud_flag, estimate = .pred_class)$.estimate
      )
    )
    
    holdout_metrics <- tibble(
      model = model_name,
      dataset = "Holdout Set",
      metric = c("ROC_AUC", "PR_AUC", "F1_Score", "Precision", "Recall"),
      value = c(
        roc_auc(holdout_df, truth = fraud_flag, .pred_1)$.estimate,
        pr_auc(holdout_df, truth = fraud_flag, .pred_1)$.estimate,
        f_meas(holdout_df, truth = fraud_flag, estimate = .pred_class)$.estimate,
        precision(holdout_df, truth = fraud_flag, estimate = .pred_class)$.estimate,
        recall(holdout_df, truth = fraud_flag, estimate = .pred_class)$.estimate
      )
    )
    
    metrics_all[[model_name]] <- bind_rows(test_metrics, holdout_metrics)
    test_preds_list[[model_name]] <- test_df
    holdout_preds_list[[model_name]] <- holdout_df
  }
  
  return(list(
    metrics = bind_rows(metrics_all),
    test_predictions = test_preds_list,
    holdout_predictions = holdout_preds_list
  ))
}







# =============================================================================
# 4) Run Evaluation and Save Results
# =============================================================================
eval_results <- evaluate_all_models(
  xgb_model, log_reg_fit, rf_fit,
  test_xgb, holdout_xgb,
  test_tidymodels, holdout_tidymodels,
  threshold = 0.8
)

# Nicely formatted metrics
metrics_df <- eval_results$metrics
print(metrics_df)

# Extract test predictions per model
xgb_preds <- eval_results$test_predictions$XGBoost
log_preds <- eval_results$test_predictions$`Logistic Regression`
rf_preds  <- eval_results$test_predictions$`Random Forest`

# Extract holdout predictions
xgb_holdout <- eval_results$holdout_predictions$XGBoost
log_holdout <- eval_results$holdout_predictions$`Logistic Regression`
rf_holdout  <- eval_results$holdout_predictions$`Random Forest`



# Save to CSV
write_csv(metrics_df, file.path(PATH_PROCESSED, "model_evaluation_results.csv"))


# Additional Evaluation

# ROC Curve (with plotly)
plot_roc_curve <- function(pred_df, model_name = "Model") {
  library(pROC)
  library(plotly)
  
  # Ensure fraud_flag is factor
  pred_df <- pred_df %>% mutate(fraud_flag = as.factor(fraud_flag))
  
  roc_obj <- pROC::roc(response = pred_df$fraud_flag, predictor = pred_df$.pred_1, levels = c("0", "1"))
  df <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )
  
  plot_ly(df, x = ~fpr, y = ~tpr, type = 'scatter', mode = 'lines', name = 'ROC') %>%
    add_trace(x = c(0, 1), y = c(0, 1), mode = 'lines', line = list(dash = 'dash'), name = 'Random') %>%
    layout(title = paste("ROC Curve -", model_name),
           xaxis = list(title = "False Positive Rate"),
           yaxis = list(title = "True Positive Rate"))
}


# Precision-Recall Curve
plot_pr_curve <- function(pred_df, model_name = "Model") {
  library(precrec)
  library(plotly)
  library(dplyr)
  library(ggplot2) # for autoplot
  
  # Ensure fraud_flag is binary numeric
  pred_df <- pred_df %>%
    mutate(fraud_num = as.integer(fraud_flag == "1"))
  
  # Evaluate precision-recall using precrec
  eval <- evalmod(scores = pred_df$.pred_1, labels = pred_df$fraud_num)
  
  # Extract PR curve data using autoplot and filter
  pr_data <- as_tibble(ggplot2::ggplot_build(autoplot(eval))$data[[1]])
  
  # Rename for clarity
  pr_data <- pr_data %>%
    rename(precision = y, recall = x)
  
  # Plot PR curve using Plotly
  plot_ly(pr_data, x = ~recall, y = ~precision, type = 'scatter', mode = 'lines', name = 'PR Curve') %>%
    layout(
      title = paste("Precision-Recall Curve -", model_name),
      xaxis = list(title = "Recall"),
      yaxis = list(title = "Precision")
    )
}





# Confusion Matrix Heatmap (with ggplot2)
plot_confusion_matrix <- function(pred_df, model_name = "Model") {
  library(yardstick)
  library(ggplot2)
  
  # Ensure factors
  pred_df <- pred_df %>%
    mutate(
      fraud_flag = as.factor(fraud_flag),
      .pred_class = as.factor(.pred_class)
    )
  
  cm <- conf_mat(pred_df, truth = fraud_flag, estimate = .pred_class)
  df <- as.data.frame(cm$table)
  colnames(df) <- c("Truth", "Prediction", "Freq")  # Correct naming
  
  ggplot(df, aes(x = Truth, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), size = 6, color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    labs(title = paste("Confusion Matrix -", model_name),
         x = "Actual", y = "Predicted")
}



# Calibration Curve
plot_calibration_curve <- function(pred_df, model_name = "Model") {
  library(dplyr)
  library(plotly)
  
  # Make sure fraud_flag is 0/1 for calculations
  pred_df <- pred_df %>%
    mutate(
      fraud_flag = as.factor(fraud_flag),
      fraud_num = as.numeric(fraud_flag) - 1,
      bin = ntile(.pred_1, 10)
    )
  
  calib_df <- pred_df %>%
    group_by(bin) %>%
    summarise(mean_pred = mean(.pred_1), obs_rate = mean(fraud_num)) %>%
    ungroup()
  
  plot_ly(calib_df, x = ~mean_pred, y = ~obs_rate, type = 'scatter', mode = 'lines+markers') %>%
    add_trace(x = c(0, 1), y = c(0, 1), mode = 'lines', name = 'Perfect Calibration',
              line = list(dash = 'dash')) %>%
    layout(title = paste("Calibration Curve -", model_name),
           xaxis = list(title = "Predicted Probability"),
           yaxis = list(title = "Observed Fraud Rate"))
}


# Feature Importance (for xgboost or ranger
plot_feature_importance <- function(model, top_n = 20, model_name = "Model") {
  library(vip)
  vip(model, num_features = top_n) +
    ggtitle(paste("Top", top_n, "Feature Importances -", model_name))
}


# Learning Curve (for XGBoost CV)
plot_learning_curve <- function(cv_result, model_name = "XGBoost") {
  library(ggplot2)
  
  df <- cv_result$evaluation_log
  
  ggplot(df, aes(x = iter)) +
    geom_line(aes(y = train_auc_mean, color = "Train AUC")) +
    geom_line(aes(y = test_auc_mean, color = "Validation AUC")) +
    labs(title = paste("Learning Curve -", model_name),
         y = "AUC", x = "Boosting Rounds") +
    theme_minimal() +
    scale_color_manual(values = c("Train AUC" = "steelblue", "Validation AUC" = "darkorange"))
}

plot_roc_curve(xgb_preds, "XGBoost (Test)")
plot_pr_curve(rf_preds, "Random Forest (Test)")
plot_confusion_matrix(log_preds, "Logistic Regression (Test)")
plot_calibration_curve(rf_preds, "Random Forest (Test)")
plot_learning_curve(cv_results, "XGBoost")
plot_feature_importance(xgb_model, model_name = "XGBoost")
# =============================================================================
# 5) Generate All Evaluation Plots for All Models
# =============================================================================

# Test predictions per model
test_pred_list <- list(
  "XGBoost" = xgb_preds,
  "Logistic Regression" = log_preds,
  "Random Forest" = rf_preds
)

# Generate ROC and PR curves, confusion matrix, and calibration curve
for (model_name in names(test_pred_list)) {
  pred_df <- test_pred_list[[model_name]]
  
  print(plot_roc_curve(pred_df, paste(model_name, "(Test)")))
  print(plot_pr_curve(pred_df, paste(model_name, "(Test)")))
  print(plot_confusion_matrix(pred_df, paste(model_name, "(Test)")))
  print(plot_calibration_curve(pred_df, paste(model_name, "(Test)")))
}

# Learning Curve and Feature Importance for XGBoost
print(plot_learning_curve(cv_results, "XGBoost"))
print(plot_feature_importance(xgb_model, model_name = "XGBoost"))


# Test predictions per model
holdout_pred_list <- list(
  "XGBoost" = xgb_holdout,
  "Logistic Regression" = log_holdout,
  "Random Forest" = rf_holdout
)

# Generate ROC and PR curves, confusion matrix, and calibration curve
for (model_name in names(holdout_pred_list)) {
  pred_df <- holdout_pred_list[[model_name]]
  
  print(plot_roc_curve(pred_df, paste(model_name, "(Holdout)")))
  print(plot_pr_curve(pred_df, paste(model_name, "(Holdout)")))
  print(plot_confusion_matrix(pred_df, paste(model_name, "(Holdout)")))
  print(plot_calibration_curve(pred_df, paste(model_name, "(Holdout)")))
}

# Learning Curve and Feature Importance for XGBoost
print(plot_learning_curve(cv_results, "XGBoost"))
print(plot_feature_importance(xgb_model, model_name = "XGBoost"))

plot_rf_feature_importance <- function(rf_fit, top_n = 20, model_name = "Random Forest") {
  library(vip)
  rf_final <- extract_fit_parsnip(rf_fit)$fit
  
  vip::vip(rf_final, num_features = top_n, geom = "col") +
    ggtitle(paste("Top", top_n, "Feature Importances -", model_name)) +
    theme_minimal()
}



plot_logistic_coefficients <- function(log_reg_fit, top_n = 20, model_name = "Logistic Regression") {
  library(broom)
  library(plotly)
  
  # Extract coefficients and tidy
  tidy_coeffs <- tidy(log_reg_fit) %>%
    filter(term != "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    slice(1:top_n)
  
  # Plot using plotly
  plot_ly(
    tidy_coeffs,
    x = ~reorder(term, estimate),
    y = ~estimate,
    type = "bar",
    marker = list(color = ~ifelse(estimate > 0, 'tomato', 'steelblue'))
  ) %>%
    layout(
      title = paste("Top", top_n, "Coefficients -", model_name),
      xaxis = list(title = "Feature"),
      yaxis = list(title = "Coefficient"),
      margin = list(b = 120)
    )
}

# =============================================================================
# 6) Final Feature Importance Plots
# =============================================================================

print(plot_feature_importance(xgb_model, model_name = "XGBoost"))
print(plot_rf_feature_importance(rf_fit, model_name = "Random Forest"))
print(plot_logistic_coefficients(log_reg_fit, model_name = "Logistic Regression"))



