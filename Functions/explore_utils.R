library(tidyverse)
library(openxlsx)
library(lubridate)


#' Get Basic Column Information
#'
#' This function computes basic statistics for each column in a data frame, including column type,
#' number of unique values, missing values, and percentage of missing values.
#'
#' @param df A data frame.
#' @return A tibble with the following columns:
#' \describe{
#'   \item{column_name}{Column name.}
#'   \item{type}{Data type of the column.}
#'   \item{n_unique}{Number of unique values.}
#'   \item{n_missing}{Number of missing values.}
#'   \item{pct_missing}{Percentage of missing values.}
#' }
#' @export
get_basic_info <- function(df) {
  tibble(
    column_name = names(df),
    type = map_chr(df, ~class(.)[1]),
    n_unique = map_dbl(df, ~n_distinct(., na.rm = TRUE)),
    n_missing = map_dbl(df, ~sum(is.na(.))),
    pct_missing = round(map_dbl(df, ~sum(is.na(.)) / nrow(df) * 100), 2)
  )
}


#' Get Column Statistics
#'
#' Computes descriptive statistics for numeric, categorical, and datetime columns.
#'
#' @param df A data frame.
#' @return A tibble containing statistical summaries for each column.
#' @export
get_column_stats <- function(df) {
  all_stats <- map_dfr(names(df), function(col) {
    col_data <- df[[col]]
    col_type <- class(col_data)[1]
    
    # Process numeric columns
    if (is.numeric(col_data)) {
      tibble(
        column_name = col,
        type = "numeric",
        n_unique = as.character(n_distinct(col_data, na.rm = TRUE)),
        n_missing = as.character(sum(is.na(col_data))),
        pct_missing = as.character(round(sum(is.na(col_data)) / nrow(df) * 100, 2)),
        mean = as.character(mean(col_data, na.rm = TRUE)),
        sd = as.character(sd(col_data, na.rm = TRUE)),
        min = as.character(min(col_data, na.rm = TRUE)),
        max = as.character(max(col_data, na.rm = TRUE)),
        median = as.character(median(col_data, na.rm = TRUE))
      )
      
      # Process character/categorical columns
    } else if (is.character(col_data) || is.factor(col_data)) {
      tibble(
        column_name = col,
        type = "character",
        n_unique = as.character(n_distinct(col_data, na.rm = TRUE)),
        n_missing = as.character(sum(is.na(col_data))),
        pct_missing = as.character(round(sum(is.na(col_data)) / nrow(df) * 100, 2)),
        min_str = as.character(min(nchar(col_data), na.rm = TRUE)),
        max_str = as.character(max(nchar(col_data), na.rm = TRUE))
      )
      
      # Process datetime columns
    } else if (lubridate::is.POSIXt(col_data)) {
      tibble(
        column_name = col,
        type = "datetime",
        n_unique = as.character(n_distinct(col_data, na.rm = TRUE)),
        n_missing = as.character(sum(is.na(col_data))),
        pct_missing = as.character(round(sum(is.na(col_data)) / nrow(df) * 100, 2)),
        min = as.character(min(col_data, na.rm = TRUE)),
        max = as.character(max(col_data, na.rm = TRUE))
      )
      
    } else {
      tibble(column_name = col, type = "unknown")
    }
  })
  
  return(all_stats)
}



#' Get Numeric Column Distribution
#'
#' Computes bin distributions for numeric columns.
#'
#' @param df A data frame.
#' @param col The column name (as a string).
#' @return A tibble containing binned counts and cumulative percentages.
#' @export
get_numeric_distribution <- function(df, col) {
  col_data <- df[[col]]
  
  if (all(is.na(col_data)) || length(unique(col_data)) == 1) {
    return(tibble(message = paste(col, "has only one unique value or all NA")))
  }
  
  min_val <- min(col_data, na.rm = TRUE)
  max_val <- max(col_data, na.rm = TRUE)
  
  if (min_val == max_val) {
    return(tibble(message = paste(col, "has a single unique value:", min_val)))
  }
  
  breaks <- seq(min_val, max_val, length.out = 11)  # Ensure 10 bins
  
  df %>%
    filter(!is.na(!!sym(col))) %>%
    mutate(
      bin = cut(!!sym(col), 
                breaks = breaks,
                include.lowest = TRUE,
                dig.lab = 4)
    ) %>%
    count(bin) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 2),
      cumulative_percentage = round(cumsum(percentage), 2)
    )
}


#' Get Datetime Column Distribution
#'
#' Computes distribution counts by year and month for datetime columns.
#'
#' @param df A data frame.
#' @param col The column name (as a string).
#' @return A tibble containing year-month counts and cumulative percentages.
#' @export
get_datetime_distribution <- function(df, col) {
  df %>%
    filter(!is.na(!!sym(col))) %>%
    mutate(
      year_month = floor_date(!!sym(col), "month")
    ) %>%
    count(year_month) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 2),
      cumulative_percentage = round(cumsum(percentage), 2)
    )
}


#' Get Categorical Column Distribution
#'
#' Computes frequency counts and cumulative percentages for categorical columns.
#'
#' @param df A data frame.
#' @param col The column name (as a string).
#' @return A tibble containing frequency counts and cumulative percentages.
#' @export
get_categorical_distribution <- function(df, col) {
  df %>%
    filter(!is.na(!!sym(col))) %>%
    count(!!sym(col), name = "frequency") %>%
    arrange(desc(frequency)) %>%
    mutate(
      percentage = round(frequency / sum(frequency) * 100, 2),
      cumulative_percentage = round(cumsum(percentage), 2)
    )
}


#' Perform Data Audit
#'
#' Conducts a full audit of a data frame, computing column statistics and distributions.
#' Optionally exports the results to an Excel file.
#'
#' @param df A data frame.
#' @param name A string, the name of the audit file (if exported).
#' @param export Logical; if TRUE, an Excel report is created.
#' @return A list containing:
#' \describe{
#'   \item{main_summary}{A tibble with column statistics.}
#' }
#' @examples
#' # Example data
#' df <- tibble(
#'   id = 1:100,
#'   value = rnorm(100),
#'   category = sample(letters[1:5], 100, replace = TRUE),
#'   date = seq(as.POSIXct("2023-01-01"), by = "day", length.out = 100)
#' )
#'
#' # Run audit without exporting
#' results <- audit_data(df, "test_data", export = FALSE)
#'
#' # Run audit and export results to an Excel file
#' audit_data(df, "test_data", export = TRUE)
#' @export
audit_data <- function(df, name, export = FALSE) {
  # Ensure all tables have 'column_name' for merging
  main_summary <- get_column_stats(df)
  
  if (export) {
    wb <- createWorkbook()
    
    # Write main summary
    addWorksheet(wb, "main_summary")
    writeData(wb, "main_summary", main_summary)
    
    # Create distribution sheets for each column
    for (col in names(df)) {
      col_type <- case_when(
        is.numeric(df[[col]]) ~ "numeric",
        lubridate::is.POSIXt(df[[col]]) ~ "datetime",
        is.character(df[[col]]) ~ "character",
        TRUE ~ "other"
      )
      
      distribution_data <- switch(
        col_type,
        "numeric" = get_numeric_distribution(df, col),
        "datetime" = get_datetime_distribution(df, col),
        "character" = get_categorical_distribution(df, col),
        tibble()
      )
      
      # Add the sheet only if there is data to export
      if (nrow(distribution_data) > 0) {
        sheet_name <- substr(col, 1, 31)  # Ensure sheet name does not exceed Excel limit
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, distribution_data)
      }
    }
    
    # Create directory and save file
    dir.create("Data/Audit", recursive = TRUE, showWarnings = FALSE)
    saveWorkbook(wb, file.path("Data/Audit", paste0("data_audit_", name, ".xlsx")), overwrite = TRUE)
  }
  
  # Return results
  list(
    main_summary = main_summary
  )
}
