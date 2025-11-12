################################################################################
##I had AI document this so who knows what it could have messed up
################################################################################
## Packages
library(data.table)
library(psych)
library(tidyverse)
library(readxl)
library(skimr)
library(zoo)
library(sf)
library(tigris)
library(openxlsx)
library(grDevices)
library(arrow)
library(tictoc)
library(lubridate)
library(glue)
library(labelled)
library(gt)
library(tidygeocoder)
library(zipcodeR)
library(fastDummies)
library(ggnewscale)
library(stringr)
library(osrm)
library(gmapsdistance)
library(Hmisc)
library(ggbreak)
library(progress)
library(rlang)
library(googleway)
library(tinytable)
library(reticulate)
library(excel.link)
library(odbc)
library(DBI)
library(tidytext)
library(scales)
library(tools)
library(usethis)
library(devtools)

################################################################################
##Functions
################################################################################
#' Get the count of unique values
#'
#' Calculates the length of the vector returned by `unique()`.
#'
#' @param x A vector.
#' @return A single numeric value representing the count of unique elements.
lunique <- function(x){
  return(length(unique(x)))
}


#' Create a "table of tables"
#'
#' Reports the frequencies of frequencies. For example, if a vector has
#' three 1s, two 5s, and two 6s, this function will report that
#' two values (5 and 6) appear 2 times, and one value (1) appears 3 times.
#'
#' @param x A vector.
#' @return A `table` object showing the frequency of frequencies.
table2 <- function(x){
  table(table(x))
}


#' Create a table of NA vs. non-NA values
#'
#' @param x A vector.
#' @return A `table` object with counts for `TRUE` (is NA) and `FALSE` (is not NA).
nable <- function(x){
  table(is.na(x))
}


#' Print NA counts and percentages for a dataframe
#'
#' @param x A data.frame or tibble.
#' @return NULL. This function prints its results to the console.
#' @details The function prints the `nable()` count and then divides this
#'   by the total number of rows in the dataframe.
nable_pct <- function(x){
  print(table(is.na(x)))
  print(table(is.na(x)) / nrow(x))
}


#' Mutate multiple columns to Date objects
#'
#' A wrapper for `mutate(across(c(...), as.Date))` to quickly convert
#' one or more columns to the Date class.
#'
#' @param df A data.frame.
#' @param ... One or more unquoted column names to be converted to Date.
#' @return A data.frame with the specified columns mutated to Date.
mudate <- function(df, ...) {
  df %>%
    mutate(across(c(...), as.Date))
}


#' Filter with a condition but keep rows that are NA
#'
#' This function mimics `dplyr::filter()` but includes an `is.na()` check,
#' ensuring that rows where the condition evaluates to `NA` are kept
#' instead of being dropped.
#'
#' @param df A data.frame.
#' @param condition A logical expression to filter by (passed to `filter()`).
#' @return A filtered data.frame that includes rows matching the condition
#'   AND rows where the condition was `NA`.
filter_w_na <- function(df, condition) {
  # The {{ }} (curly-curly) syntax is needed to correctly handle
  # the column name passed to the function
  out <- df %>%
    filter({{ condition }} | is.na({{ condition }}))

  return(out)
}


#' List all functions in an environment
#'
#' @param env The environment to search in. Defaults to the Global Environment.
#' @return A character vector of function names.
list_all_functions <- function(env = .GlobalEnv) {
  # Get the names of all objects in the specified environment.
  all_object_names <- ls(envir = env)

  # Use sapply to apply is.function to each object, creating a logical vector.
  # get() retrieves the object itself based on its name string.
  is_function_logical_vector <- sapply(
    all_object_names,
    function(obj_name) is.function(get(obj_name, envir = env))
  )

  # Subset the original list of names using the logical vector.
  function_names <- all_object_names[is_function_logical_vector]

  return(function_names)
}


#' Calculate summary statistics for a Date vector
#'
#' Returns the min, max, mean, and median dates from a vector,
#' ignoring NA values.
#'
#' @param date_vector A vector of class `Date`.
#' @return A list containing `min_date`, `max_date`, `mean_date`, and
#'   `median_date`. Returns `NULL` if the vector is not of class `Date`
#'   or contains no valid dates.
date_mean <- function(date_vector) {
  # Validate that the input is a Date vector
  if (!inherits(date_vector, "Date")) {
    warning("Input vector is not of class 'Date'. Please convert it first.")
    return(NULL)
  }

  # Remove NAs for calculations
  clean_dates <- date_vector[!is.na(date_vector)]

  # Return NULL if no valid dates are left after removing NAs
  if (length(clean_dates) == 0) {
    warning("Date vector contains no non-NA values.")
    return(NULL)
  }

  # Calculate statistics
  # The 'range()' function returns a vector with the min and max
  date_range <- range(clean_dates)

  summary_stats <- list(
    min_date = date_range[1],
    max_date = date_range[2],
    mean_date = mean(clean_dates),
    median_date = median(clean_dates)
  )

  return(summary_stats)
}


#' Remove all objects from an environment *except* specified objects and functions
#'
#' This function is a "safe" version of `rm(list = ls())`. It finds all
#' objects in the specified environment, removes the objects you list to "keep"
#' and all functions, and then deletes what is left.
#'
#' @param ... Unquoted names of objects (dataframes, vectors, etc.) to keep.
#' @param env The environment to clean. Defaults to the Global Environment.
#' @return NULL. This function modifies the environment as a side effect.
#' @importFrom magrittr %>%
#' @importFrom rlang enquos quo_name
#' @importFrom purrr map_chr
clean_but_keep <- function(..., env = .GlobalEnv) {

  # 1. Capture the unquoted names you want to keep
  keep_vars <- rlang::enquos(...) %>%
    purrr::map_chr(rlang::quo_name)

  # 2. Get ALL objects in the environment
  all_objects <- ls(envir = env)

  # 3. Get all functions using your SAFER, existing function
  # This avoids the buggy sapply() call completely
  all_functions <- list_all_functions(env = env)

  # 4. Define the final list of objects to keep
  objects_to_keep <- c(keep_vars, all_functions)

  # 5. Determine which objects to remove
  objects_to_remove <- setdiff(all_objects, objects_to_keep)

  # 6. Remove the specified objects
  if (length(objects_to_remove) > 0) {
    rm(list = objects_to_remove, envir = env)
  }
}

#' Safely remove a vector of objects from the environment
#'
#' Takes a character vector of object names and removes only those that
#' currently exist in the Global Environment, without throwing an error
#' for objects that do not exist.
#'
#' @param objects_to_remove A character vector of object names to delete.
#' @return NULL. This function modifies the environment as a side effect.
rm_any_of <- function(objects_to_remove){
  # Find which of these objects actually exist in your current environment
  objects_that_exist <- ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv) %in% objects_to_remove]

  # Remove only the objects that were found
  # The 'if' condition is a safeguard to prevent an error if none of them exist
  if (length(objects_that_exist) > 0) {
    rm(list = objects_that_exist, envir = .GlobalEnv)
  }
}


#' Perform a rowwise mutate with a progress bar
#'
#' This function wraps `dplyr::mutate()` and forces `rowwise()` execution,
#' inserting a `progress::progress_bar$tick()` into the *first*
#' expression. This is useful for long-running rowwise operations
#' like API calls or complex string calculations.
#'
#' @param .data A data.frame.
#' @param ... One or more named expressions to be passed to `mutate()`.
#' @return A data.frame with the new/modified columns, ungrouped from the
#'   `rowwise()` operation.
#' @details The progress bar tick is only added to the first expression.
#'   All expressions are evaluated in order by `mutate()`.
mutate_bar <- function(.data, ...) {
  # --- 1. Initialize the Progress Bar ---
  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent | ETA: :eta",
    total = nrow(.data),
    clear = FALSE,
    width = 60
  )

  # --- 2. Capture User Expressions and the Correct Environment ---
  expressions <- enquos(...)
  calling_env <- current_env()

  # --- 3. Inject the Progress Bar Tick into the FIRST Expression Only ---
  # Check if there are any expressions to avoid errors
  if (length(expressions) > 0) {
    # Get the user's code from the first expression
    first_expr_code <- quo_get_expr(expressions[[1]])

    # Rebuild the first expression inside curly braces {} with the tick
    new_code_block <- expr({
      pb$tick()
      !!first_expr_code
    })

    # Create a new quosure for the first expression, tied to our environment
    expressions[[1]] <- new_quosure(new_code_block, env = calling_env)
  }

  # --- 4. Apply the Modified Expressions within mutate ---
  result <- .data %>%
    rowwise() %>%
    mutate(!!!expressions) %>%
    ungroup()

  return(result)
}

# --- NOTE: Removed 3 conflicting definitions of select_out ---
# --- Kept the final, safest version from our last conversation ---


#' Safely deselects columns from a dataframe.
#'
#' This function safely removes columns specified in `...`. It accepts
#' unquoted column names (e.g., `col_a`) or quoted column names
#' (e.g., `"col_b"`). It will *not* throw an error if a specified
#' column does not exist.
#'
#' @param .data The dataframe to modify.
#' @param ... One or more column names to remove, specified as bare
#'   names (e.g., `col_a`) or strings (e.g., `"col_b"`).
#' @return A data.frame with the specified columns removed.
select_out <- function(.data, ...) {
  # 1. Capture all arguments in ... as expressions (quosures)
  cols_to_remove_quos <- rlang::enquos(...)

  # 2. Convert every captured argument into a simple string.
  #    - col_a becomes "col_a"
  #    - "col_b" becomes "col_b"
  cols_to_remove_strings <- sapply(cols_to_remove_quos, rlang::as_name)

  # 3. Use any_of() which requires a character vector.
  #    This will remove all columns that exist and
  #    silently ignore any that are missing.
  .data %>%
    select(-any_of(cols_to_remove_strings))
}



#' Replace values with NA based on a condition for a single column
#'
#' Dynamically selects a column and replaces its values with `NA`
#' (of the correct type) where a given condition is `TRUE`.
#'
#' @param .data A data.frame.
#' @param var The unquoted name of the column to modify.
#' @param condition A logical expression. Where this is `TRUE`,
#'   the value in `var` will be set to `NA`.
#' @return A data.frame with the specified column modified.
na_replace <- function(.data, var, condition) {
  # Capture the condition expression provided by the user
  condition_quo <- enquo(condition)

  # Capture the unquoted column name
  var_quo <- enquo(var)

  # Check if the target column is numeric to choose the right NA type
  is_col_numeric <- .data %>%
    summarise(is_numeric = is.numeric({{ var_quo }})) %>%
    pull(is_numeric)

  na_type <- if (is_col_numeric) NA_real_ else NA_character_

  # Use mutate with the tidyeval operators
  .data %>%
    mutate(
      # {{var_quo}} tells mutate which column to modify.
      # The `:=` operator is needed when the column name is dynamic.
      "{{var_quo}}" := if_else(!!condition_quo, na_type, {{var_quo}})
    )
}

#' Conditionally replace values with NA for multiple columns.
#'
#' @param .data The dataframe to modify.
#' @param ... A series of named arguments in the format `column_name = condition`.
#' @return A dataframe with values replaced by NA where conditions are met.
#'
na_if_true <- function(.data, ...) {
  # Capture all the `var = condition` pairs provided by the user
  expressions <- enquos(...)

  # Get the names of the columns that will be modified
  vars_to_mutate <- names(expressions)

  # Start with the original dataframe
  data_mutated <- .data

  # Loop through each expression one by one
  for (i in seq_along(expressions)) {
    # Get the current column name (as a symbol) and its corresponding condition
    var_quo <- sym(vars_to_mutate[i])
    condition_quo <- expressions[[i]]

    # Check if the column is numeric to choose the correct NA type
    is_col_numeric <- data_mutated %>%
      summarise(is_numeric = is.numeric({{ var_quo }})) %>%
      pull(is_numeric)

    na_type <- if (is_col_numeric) NA_real_ else NA_character_

    # Apply the mutation for the current variable, updating the dataframe
    data_mutated <- data_mutated %>%
      mutate(
        "{{var_quo}}" := if_else(!!condition_quo, na_type, {{ var_quo }})
      )
  }

  # Return the final, fully modified dataframe
  return(data_mutated)
}


#' Find the best (longest) substring match from an approved list
#'
#' Compares a single name (`tc_name`) against a list of approved names.
#' It returns a perfect match if one exists. If not, it finds all
#' approved names that are substrings of `tc_name` and returns the
#' longest one.
#'
#' @param tc_name A single string to be checked.
#' @param approved_list A character vector of "approved" names to match against.
#' @return The longest matching string from `approved_list`, or the
#'   original `tc_name` if no match is found.
find_best_match <- function(tc_name, approved_list) {
  # 1. Check for a perfect match first. If it exists, we're done.
  if (tc_name %in% approved_list) {
    return(tc_name)
  }

  # 2. If no perfect match, find all approved names that are substrings of the tc_name.
  # We use fixed() to ensure names with special characters are treated as plain text.
  substring_matches <- approved_list[str_detect(tc_name, fixed(approved_list))]

  # 3. Decide what to do with the matches.
  if (length(substring_matches) == 0) {
    # If no approved name is found inside the tc_name, return the original tc_name.
    return(tc_name)
  } else {
    # If one or more matches are found, return the LONGEST one.
    # This is the most specific match (e.g., "KIDS LAND" is better than "KIDS").
    best_match <- substring_matches[which.max(nchar(substring_matches))]
    return(best_match)
  }
}


#' Scalar helper for Longest Common Substring (LCS)
#'
#' Internal function to calculate the LCS for two strings, with an
#' option to remove a list of "ignore strings" before comparison.
#'
#' @param str1 First string.
#' @param str2 Second string.
#' @param ignore_strings A character vector of phrases to remove from
#'   both strings (case-insensitive) before comparison.
#' @return The numeric length of the longest common substring.
#' @noRd
.lcs_scalar <- function(str1, str2, ignore_strings = NULL) {
  # Pre-process strings to remove ignored words before comparison
  if (!is.null(ignore_strings) && length(ignore_strings) > 0) {
    # Sort by length descending to remove longer phrases first (e.g., "LEARNING CENTER" before "CENTER")
    ignore_strings <- ignore_strings[order(nchar(ignore_strings), decreasing = TRUE)]

    # Loop through and remove each ignored string literally
    for (term in ignore_strings) {
      if (!is.na(str1)) {
        str1 <- stringr::str_replace_all(str1, stringr::fixed(term), "")
      }
      if (!is.na(str2)) {
        str2 <- stringr::str_replace_all(str2, stringr::fixed(term), "")
      }
    }
    # Clean up any extra spaces that result from the removal
    if (!is.na(str1)) str1 <- stringr::str_squish(str1)
    if (!is.na(str2)) str2 <- stringr::str_squish(str2)
  }

  # Return 0 if either string is missing (NA) or empty after cleaning.
  if (is.na(str1) || is.na(str2) || nchar(str1) == 0 || nchar(str2) == 0) {
    return(0)
  }

  s1_chars <- strsplit(str1, "")[[1]]
  s2_chars <- strsplit(str2, "")[[1]]
  lcs_matrix <- matrix(0, nrow = nchar(str1), ncol = nchar(str2))
  max_len <- 0

  for (i in 1:nchar(str1)) {
    for (j in 1:nchar(str2)) {
      if (s1_chars[i] == s2_chars[j]) {
        if (i == 1 || j == 1) {
          lcs_matrix[i, j] <- 1
        } else {
          lcs_matrix[i, j] <- lcs_matrix[i - 1, j - 1] + 1
        }
        if (lcs_matrix[i, j] > max_len) {
          max_len <- lcs_matrix[i, j]
        }
      } else {
        lcs_matrix[i, j] <- 0
      }
    }
  }
  return(max_len)
}


#' Vectorized Longest Common Substring (LCS)
#'
#' Calculates the length of the longest common substring for two
#' vectors of strings, element by element.
#'
#' @param str1_vec A character vector.
#' @param str2_vec A character vector, must be the same length as `str1_vec`.
#' @param ignore_strings A character vector of phrases to remove from
#'   both strings (case-insensitive) before comparison.
#' @return A numeric vector of LCS lengths.
count_consecutive_overlap <- function(str1_vec, str2_vec, ignore_strings = NULL) {
  mapply(.lcs_scalar, str1_vec, str2_vec, MoreArgs = list(ignore_strings = ignore_strings))
}


#' Scalar helper for Levenshtein (edit) distance
#'
#' Internal function to calculate the edit distance for two strings, with an
#' option to remove a list of "ignore strings" before comparison.
#'
#' @param str1 First string.
#' @param str2 Second string.
#' @param ignore_strings A character vector of phrases to remove from
#'   both strings (case-insensitive) before comparison.
#' @return The numeric edit distance.
#' @noRd
.edit_dist_scalar <- function(str1, str2, ignore_strings = NULL) {
  # Pre-process strings to remove ignored words before comparison
  if (!is.null(ignore_strings) && length(ignore_strings) > 0) {
    # Sort by length descending to remove longer phrases first
    ignore_strings <- ignore_strings[order(nchar(ignore_strings), decreasing = TRUE)]

    # Loop through and remove each ignored string literally
    for (term in ignore_strings) {
      if (!is.na(str1)) {
        str1 <- stringr::str_replace_all(str1, stringr::fixed(term), "")
      }
      if (!is.na(str2)) {
        str2 <- stringr::str_replace_all(str2, stringr::fixed(term), "")
      }
    }
    # Clean up any extra spaces that result from the removal
    if (!is.na(str1)) str1 <- stringr::str_squish(str1)
    if (!is.na(str2)) str2 <- stringr::str_squish(str2)
  }

  # If both are NA (or become empty), the difference is 0.
  if ((is.na(str1) || nchar(str1) == 0) && (is.na(str2) || nchar(str2) == 0)) {
    return(0)
  }
  # If one is NA/empty, the difference is the full length of the other string.
  if (is.na(str1) || nchar(str1) == 0) {
    return(nchar(str2))
  }
  if (is.na(str2) || nchar(str2) == 0) {
    return(nchar(str1))
  }
  # If they are identical after cleaning, the difference is 0.
  if (str1 == str2) {
    return(0)
  }

  # Use the adist function from the base 'utils' package.
  return(adist(str1, str2)[1, 1])
}


#' Vectorized Levenshtein (edit) distance
#'
#' Calculates the edit distance for two vectors of strings,
#' element by element.
#'
#' @param str1_vec A character vector.
#' @param str2_vec A character vector, must be the same length as `str1_vec`.
#' @param ignore_strings A character vector of phrases to remove from
#'   both strings (case-insensitive) before comparison.
#' @return A numeric vector of edit distances.
calculate_edit_distance <- function(str1_vec, str2_vec, ignore_strings = NULL) {
  mapply(.edit_dist_scalar, str1_vec, str2_vec, MoreArgs = list(ignore_strings = ignore_strings))
}


#' Scalar helper for TF-IDF weighted similarity score
#'
#' Internal function. Calculates a similarity score for two strings based on
#' the sum of IDF weights for their common words.
#'
#' @param str1 First string.
#' @param str2 Second string.
#' @param tfidf_weights A data.frame with columns `word` and `idf`.
#' @return A numeric score.
#' @noRd
.tfidf_score_scalar <- function(str1, str2, tfidf_weights) {
  if (is.na(str1) || is.na(str2)) return(0)

  # Split names into words AND convert to lowercase to match the tfidf table
  words1 <- unlist(strsplit(tolower(str1), "\\s+"))
  words2 <- unlist(strsplit(tolower(str2), "\\s+"))

  # Find the common words
  common_words <- intersect(words1, words2)

  if (length(common_words) == 0) return(0)

  # Sum the IDF scores of the common words
  score <- tfidf_weights %>%
    filter(word %in% common_words) %>%
    summarise(total_score = sum(idf)) %>%
    pull(total_score)

  # Handle cases where the pull returns an empty numeric vector
  if (length(score) == 0) return(0)

  return(score)
}


#' Vectorized TF-IDF weighted similarity score
#'
#' Calculates a similarity score for two vectors of strings based on
#' the sum of IDF weights for their common words.
#'
#' @param str1_vec A character vector.
#' @param str2_vec A character vector, same length as `str1_vec`.
#' @param tfidf_weights A data.frame with columns `word` and `idf`.
#' @return A numeric vector of similarity scores.
calculate_tfidf_similarity <- function(str1_vec, str2_vec, tfidf_weights) {
  mapply(.tfidf_score_scalar, str1_vec, str2_vec, MoreArgs = list(tfidf_weights = tfidf_weights))
}

#' Creates a publication-quality histogram with sensible defaults.
#'
#' @param df The dataframe containing the data.
#' @param var_string The *string name* of the numeric variable to plot.
#' @param binwidth_val The width of the histogram bins.
#' @return A ggplot object representing the histogram.
lazy_hist <- function(df, var_string, binwidth_val = 30) {
  # 1. Calculate the mean for the vertical line
  mean_val <- mean(df[[var_string]], na.rm = TRUE)

  # 2. NEW: Define the range and breaks for the x-axis ticks
  # This ensures the ticks match the binwidth
  min_val <- floor(min(df[[var_string]], na.rm = TRUE) / binwidth_val) * binwidth_val
  max_val <- ceiling(max(df[[var_string]], na.rm = TRUE) / binwidth_val) * binwidth_val
  x_breaks <- seq(from = min_val, to = max_val, by = binwidth_val/2)


  # 3. Create the plot using ggplot2
  ggplot(df, aes(x = .data[[var_string]])) +

    # Add the histogram layer
    geom_histogram(
      binwidth = binwidth_val,
      fill = "#4e79a7",
      color = "white",
      alpha = 0.9
    ) +

    # NEW: Add text labels for the count inside each bar
    geom_text(
      stat = "bin",
      aes(label = after_stat(if_else(count > 0, as.character(count), ""))), # Use count as the label, hide if 0
      binwidth = binwidth_val,
      vjust = 1.5,     # Adjust vertical position to be inside the bar
      color = "white",     # Set text color
      size = 3.5           # Adjust text size
    ) +

    # Add a vertical line to show the mean value
    geom_vline(
      xintercept = mean_val,
      color = "#e15759",
      linetype = "dashed",
      linewidth = 1
    ) +

    # Use annotate() for a single text label for the mean
    annotate(
      "text",
      x = mean_val,
      y = Inf,
      label = paste("Mean =", round(mean_val, 1)),
      vjust = 1.5,
      hjust = -0.1,
      color = "#e15759",
      size = 4
    ) +

    # NEW: Add custom scale for the x-axis to align ticks with bins
    scale_x_continuous(breaks = x_breaks) +

    # Add informative, auto-generated labels and a title
    labs(
      title = paste("Distribution of", toTitleCase(var_string)),
      subtitle = "Frequency distribution with mean indicated by the dashed line",
      x = toTitleCase(var_string),
      y = "Frequency (Count)"
    ) +

    # Apply a clean, professional theme
    theme_minimal(base_size = 14) +

    # Further customize theme elements for a polished look
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f5", color = NA),
      panel.background = element_rect(fill = "#f5f5f5", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1) # Angle text if labels overlap
    )
}

#' Save multiple dataframes to separate sheets in a single Excel file.
#'
#' @param filename The path and name of the output .xlsx file (e.g., "output.xlsx").
#' @param sheet_names A character vector of names for the sheets.
#' @param data_list A list of the dataframe objects to write.
#'
#' @details The order of sheet_names must correspond to the order of dataframes in data_list.
#'          The length of sheet_names must equal the length of data_list.
#'          Using a list of dataframes (data_list) is more robust than using
#'          dataframe names (strings), as it works regardless of the environment.
#'
#' @return NULL. The function writes a file as a side effect and prints a message.
#'
save_to_excel_multisheet <- function(filename, sheet_names, data_list) {

  # --- Input Validation ---
  if (length(sheet_names) != length(data_list)) {
    stop("Error: The number of sheet names must equal the number of dataframes in the list.")
  }

  if (!is.list(data_list) || is.data.frame(data_list)) {
    stop("Error: 'data_list' must be a list of dataframes (e.g., list(df1, df2)).")
  }

  if (!all(sapply(data_list, is.data.frame))) {
    warning("Warning: Not all items in 'data_list' appear to be dataframes.")
  }

  # 1. Create a new Excel workbook
  wb <- createWorkbook()

  # 2. Loop through and add each sheet and its data
  for (i in 1:length(data_list)) {
    current_sheet_name <- sheet_names[i]
    current_data <- data_list[[i]]

    addWorksheet(wb, sheetName = current_sheet_name)
    writeData(wb, sheet = current_sheet_name, x = current_data)
  }

  # 3. Save the workbook
  # Using tryCatch for basic error handling during save
  tryCatch({
    saveWorkbook(wb, file = filename, overwrite = TRUE)
    cat(paste("Excel file '", filename, "' created successfully with ", length(data_list), " sheets.\n", sep=""))
  }, error = function(e) {
    cat(paste("Error saving file '", filename, "': ", e$message, "\n", sep=""))
  })
}
