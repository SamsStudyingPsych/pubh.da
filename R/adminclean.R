#' Get the count of unique values
#'
#' Calculates the length of the vector returned by `unique()`.
#'
#' @param x A vector.
#' @return A single numeric value representing the count of unique elements.
#' @export
#' @examples
#' \dontrun{
#' lunique(c(1, 1, 2, 3, 3)) # Returns 3
#' }
lunique <- function(x){
  return(length(unique(x)))
}


#' Create a "table of tables"
#'
#' Reports the frequencies of frequencies.
#'
#' @param x A vector.
#' @return A `table` object showing the frequency of frequencies.
#' @export
#' @examples
#' \dontrun{
#' vec <- c(1, 1, 1, 2, 2, 3, 3)
#' table2(vec)
#' # 2 values (2 and 3) appear 2 times
#' # 1 value (1) appears 3 times
#' }
table2 <- function(x){
  table(table(x))
}


#' Create a table of NA vs. non-NA values
#'
#' @param x A vector.
#' @return A `table` object with counts for `TRUE` (is NA) and `FALSE` (is not NA).
#' @export
#' @examples
#' \dontrun{
#' nable(c(1, 2, NA, 4, NA))
#' }
nable <- function(x){
  table(is.na(x))
}


#' Print NA counts and percentages for a dataframe
#'
#' @param x A data.frame or tibble.
#' @return NULL. This function prints its results to the console.
#' @export
#' @examples
#' \dontrun{
#' nable_pct(df)
#' }
nable_pct <- function(x){
  print(table(is.na(x)))
  print(table(is.na(x)) / nrow(x))
}


#' Mutate multiple columns to Date objects
#'
#' A wrapper for `mutate(across(c(...), as.Date))`.
#'
#' @param df A data.frame.
#' @param ... One or more unquoted column names to be converted to Date.
#' @return A data.frame with the specified columns mutated to Date.
#' @export
#' @importFrom dplyr mutate across
#' @examples
#' \dontrun{
#' df <- mudate(df, start_date, end_date)
#' }
mudate <- function(df, ...) {
  df %>%
    dplyr::mutate(dplyr::across(c(...), as.Date))
}


#' Filter with a condition but keep rows that are NA
#'
#' @param df A data.frame.
#' @param condition A logical expression to filter by (passed to `filter()`).
#' @return A filtered data.frame including rows matching the condition AND NA rows.
#' @export
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' # Keep rows where age > 18 OR age is NA
#' filter_w_na(df, age > 18)
#' }
filter_w_na <- function(df, condition) {
  out <- df %>%
    dplyr::filter({{ condition }} | is.na({{ condition }}))
  return(out)
}


#' List all functions in an environment
#'
#' @param env The environment to search in. Defaults to the Global Environment.
#' @return A character vector of function names.
#' @export
#' @examples
#' \dontrun{
#' list_all_functions()
#' }
list_all_functions <- function(env = .GlobalEnv) {
  objs <- ls(envir = env)
  if (length(objs) == 0) return(character(0))
  Filter(function(x) {
    is.function(tryCatch(get(x, envir = env), error = function(e) FALSE))
  }, objs)
}


#' Calculate summary statistics for a Date vector
#'
#' @param date_vector A vector of class `Date`.
#' @return A list containing min, max, mean, and median dates.
#' @export
#' @examples
#' \dontrun{
#' date_mean(df$appl_dte)
#' }
date_mean <- function(date_vector) {
  if (!inherits(date_vector, "Date")) {
    warning("Input vector is not of class 'Date'. Please convert it first.")
    return(NULL)
  }
  clean_dates <- date_vector[!is.na(date_vector)]
  if (length(clean_dates) == 0) {
    warning("Date vector contains no non-NA values.")
    return(NULL)
  }
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
#' @param ... Unquoted names of objects to keep.
#' @param env The environment to clean. Defaults to .GlobalEnv.
#' @return NULL. Side effect: removes objects.
#' @export
#' @importFrom rlang enquos quo_name
#' @importFrom purrr map_chr
#' @examples
#' \dontrun{
#' clean_but_keep(df, lookup_table)
#' }
clean_but_keep <- function(..., env = .GlobalEnv) {
  # 1. Early Exit: If the environment is already empty, do nothing.
  all_objects <- ls(envir = env)
  if (length(all_objects) == 0) {
    return(invisible(NULL))
  }
  
  # 2. Capture the unquoted names you want to keep
  keep_vars <- rlang::enquos(...) %>%
    purrr::map_chr(rlang::quo_name)
  
  # 3. Get all functions (using internal helper logic or the function itself)
  # We use the robust logic directly here to ensure stability within this function
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
#' @param objects_to_remove A character vector of object names to delete.
#' @return NULL.
#' @export
#' @examples
#' \dontrun{
#' rm_any_of(c("temp_df", "old_var"))
#' }
rm_any_of <- function(objects_to_remove){
  objects_that_exist <- ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv) %in% objects_to_remove]
  if (length(objects_that_exist) > 0) {
    rm(list = objects_that_exist, envir = .GlobalEnv)
  }
}


#' Perform a rowwise mutate with a progress bar
#'
#' @param .data A data.frame.
#' @param ... One or more named expressions to be passed to `mutate()`.
#' @return A data.frame with modified columns.
#' @export
#' @importFrom progress progress_bar
#' @importFrom rlang enquos current_env quo_get_expr expr new_quosure
#' @importFrom dplyr rowwise mutate ungroup
#' @examples
#' \dontrun{
#' df %>%
#'   mutate_bar(new_col = complex_calculation(col1))
#' }
mutate_bar <- function(.data, ...) {
  pb <- progress::progress_bar$new(
    format = "  Processing [:bar] :percent | ETA: :eta",
    total = nrow(.data), clear = FALSE, width = 60
  )
  expressions <- rlang::enquos(...)
  calling_env <- rlang::current_env()
  
  if (length(expressions) > 0) {
    first_expr_code <- rlang::quo_get_expr(expressions[[1]])
    new_code_block <- rlang::expr({
      pb$tick()
      !!first_expr_code
    })
    expressions[[1]] <- rlang::new_quosure(new_code_block, env = calling_env)
  }
  
  result <- .data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!!expressions) %>%
    dplyr::ungroup()
  return(result)
}


#' Safely deselects columns from a dataframe
#'
#' @param .data The dataframe to modify.
#' @param ... Column names to remove (bare or quoted).
#' @return A data.frame with columns removed.
#' @export
#' @importFrom rlang enquos as_name
#' @importFrom dplyr select any_of
#' @examples
#' \dontrun{
#' df %>% select_out(temp_id, "legacy_code")
#' }
select_out <- function(.data, ...) {
  cols_to_remove_quos <- rlang::enquos(...)
  cols_to_remove_strings <- sapply(cols_to_remove_quos, rlang::as_name)
  .data %>%
    dplyr::select(-dplyr::any_of(cols_to_remove_strings))
}


#' Replace values with NA based on a condition for a single column
#'
#' @param .data A data.frame.
#' @param var The unquoted name of the column to modify.
#' @param condition A logical expression.
#' @return A data.frame.
#' @export
#' @importFrom dplyr mutate if_else summarise pull
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' df %>% na_replace(age, age < 0)
#' }
na_replace <- function(.data, var, condition) {
  condition_quo <- rlang::enquo(condition)
  var_quo <- rlang::enquo(var)
  
  is_col_numeric <- .data %>%
    dplyr::summarise(is_numeric = is.numeric({{ var_quo }})) %>%
    dplyr::pull(is_numeric)
  
  na_type <- if (is_col_numeric) NA_real_ else NA_character_
  
  .data %>%
    dplyr::mutate("{{var_quo}}" := dplyr::if_else(!!condition_quo, na_type, {{var_quo}}))
}


#' Conditionally replace values with NA for multiple columns
#'
#' @param .data The dataframe to modify.
#' @param ... Named arguments: `column_name = condition`.
#' @return A dataframe.
#' @export
#' @importFrom rlang enquos sym
#' @importFrom dplyr mutate if_else summarise pull
#' @examples
#' \dontrun{
#' df %>% na_if_true(age = age < 0, income = income == 99999)
#' }
na_if_true <- function(.data, ...) {
  expressions <- rlang::enquos(...)
  vars_to_mutate <- names(expressions)
  data_mutated <- .data
  
  for (i in seq_along(expressions)) {
    var_quo <- rlang::sym(vars_to_mutate[i])
    condition_quo <- expressions[[i]]
    is_col_numeric <- data_mutated %>%
      dplyr::summarise(is_numeric = is.numeric({{ var_quo }})) %>%
      dplyr::pull(is_numeric)
    na_type <- if (is_col_numeric) NA_real_ else NA_character_
    
    data_mutated <- data_mutated %>%
      dplyr::mutate("{{var_quo}}" := dplyr::if_else(!!condition_quo, na_type, {{ var_quo }}))
  }
  return(data_mutated)
}


#' Find the best (longest) substring match from an approved list
#'
#' @param tc_name A single string to be checked.
#' @param approved_list A character vector of "approved" names.
#' @return The matched string or original name.
#' @export
#' @importFrom stringr str_detect fixed
#' @examples
#' \dontrun{
#' find_best_match("ABC Learning Center LLC", c("ABC Learning", "XYZ Corp"))
#' }
find_best_match <- function(tc_name, approved_list) {
  if (tc_name %in% approved_list) return(tc_name)
  substring_matches <- approved_list[stringr::str_detect(tc_name, stringr::fixed(approved_list))]
  if (length(substring_matches) == 0) {
    return(tc_name)
  } else {
    best_match <- substring_matches[which.max(nchar(substring_matches))]
    return(best_match)
  }
}


#' Vectorized Longest Common Substring (LCS)
#'
#' @param str1_vec A character vector.
#' @param str2_vec A character vector.
#' @param ignore_strings Vector of strings to remove before comparison.
#' @return A numeric vector of LCS lengths.
#' @export
#' @examples
#' \dontrun{
#' count_consecutive_overlap(df$name1, df$name2, ignore_strings = c("INC", "LLC"))
#' }
count_consecutive_overlap <- function(str1_vec, str2_vec, ignore_strings = NULL) {
  mapply(.lcs_scalar, str1_vec, str2_vec, MoreArgs = list(ignore_strings = ignore_strings))
}


#' Vectorized Levenshtein (edit) distance
#'
#' @param str1_vec A character vector.
#' @param str2_vec A character vector.
#' @param ignore_strings Vector of strings to remove before comparison.
#' @return A numeric vector of edit distances.
#' @export
#' @examples
#' \dontrun{
#' calculate_edit_distance(df$name1, df$name2)
#' }
calculate_edit_distance <- function(str1_vec, str2_vec, ignore_strings = NULL) {
  mapply(.edit_dist_scalar, str1_vec, str2_vec, MoreArgs = list(ignore_strings = ignore_strings))
}


#' Vectorized TF-IDF weighted similarity score
#'
#' @param str1_vec A character vector.
#' @param str2_vec A character vector.
#' @param tfidf_weights Optional weights dataframe.
#' @return A numeric vector of similarity scores.
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate distinct count select rename n_distinct row_number
#' @importFrom tidyr unnest
#' @examples
#' \dontrun{
#' calculate_tfidf_similarity(df$name1, df$name2)
#' }
calculate_tfidf_similarity <- function(str1_vec, str2_vec, tfidf_weights = NULL) {
  if (is.null(tfidf_weights)) {
    corpus_vector <- unique(c(str1_vec, str2_vec))
    corpus_vector <- corpus_vector[!is.na(corpus_vector)]
    all_words_df <- tibble::tibble(raw_text = corpus_vector) %>%
      dplyr::mutate(doc_id = dplyr::row_number()) %>%
      dplyr::mutate(word_list = strsplit(tolower(raw_text), "\\s+")) %>%
      tidyr::unnest(word_list) %>%
      dplyr::rename(word = word_list)
    
    tfidf_weights <- all_words_df %>%
      dplyr::distinct(doc_id, word) %>%
      dplyr::count(word, name = "n_docs") %>%
      dplyr::mutate(
        total_docs = dplyr::n_distinct(all_words_df$doc_id),
        idf = log(total_docs / n_docs)
      ) %>%
      dplyr::select(word, idf)
  }
  mapply(.tfidf_score_scalar, str1_vec, str2_vec, MoreArgs = list(tfidf_weights = tfidf_weights))
}


#' Creates a publication-quality histogram
#'
#' @param df The dataframe.
#' @param var_string The string name of the variable.
#' @param binwidth_val Bin width.
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_text geom_vline annotate scale_x_continuous labs theme_minimal theme element_text element_blank element_rect after_stat
#' @importFrom dplyr if_else
#' @importFrom tools toTitleCase
#' @examples
#' \dontrun{
#' lazy_hist(df, "age", binwidth_val = 5)
#' }
lazy_hist <- function(df, var_string, binwidth_val = 30) {
  mean_val <- mean(df[[var_string]], na.rm = TRUE)
  min_val <- floor(min(df[[var_string]], na.rm = TRUE) / binwidth_val) * binwidth_val
  max_val <- ceiling(max(df[[var_string]], na.rm = TRUE) / binwidth_val) * binwidth_val
  x_breaks <- seq(from = min_val, to = max_val, by = binwidth_val/2)
  
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_string]])) +
    ggplot2::geom_histogram(binwidth = binwidth_val, fill = "#4e79a7", color = "white", alpha = 0.9) +
    ggplot2::geom_text(
      stat = "bin",
      ggplot2::aes(label = ggplot2::after_stat(dplyr::if_else(count > 0, as.character(count), ""))),
      binwidth = binwidth_val, vjust = 1.5, color = "white", size = 3.5
    ) +
    ggplot2::geom_vline(xintercept = mean_val, color = "#e15759", linetype = "dashed", linewidth = 1) +
    ggplot2::annotate("text", x = mean_val, y = Inf, label = paste("Mean =", round(mean_val, 1)), vjust = 1.5, hjust = -0.1, color = "#e15759", size = 4) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::labs(
      title = paste("Distribution of", tools::toTitleCase(var_string)),
      subtitle = "Frequency distribution with mean indicated by the dashed line",
      x = tools::toTitleCase(var_string), y = "Frequency (Count)"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#f5f5f5", color = NA),
      panel.background = ggplot2::element_rect(fill = "#f5f5f5", color = NA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


#' Save multiple dataframes to separate sheets in a single Excel file
#'
#' @param filename Output filename (.xlsx).
#' @param sheet_names Vector of sheet names.
#' @param data_list List of dataframes.
#' @return NULL.
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @examples
#' \dontrun{
#' save_to_excel_multisheet("report.xlsx", c("Sheet1", "Sheet2"), list(df1, df2))
#' }
save_to_excel_multisheet <- function(filename, sheet_names, data_list) {
  if (length(sheet_names) != length(data_list)) stop("Error: The number of sheet names must equal the number of dataframes.")
  if (!is.list(data_list) || is.data.frame(data_list)) stop("Error: 'data_list' must be a list of dataframes.")
  wb <- openxlsx::createWorkbook()
  for (i in 1:length(data_list)) {
    openxlsx::addWorksheet(wb, sheetName = sheet_names[i])
    openxlsx::writeData(wb, sheet = sheet_names[i], x = data_list[[i]])
  }
  tryCatch({
    openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
    cat(paste("Excel file '", filename, "' created successfully.\n", sep=""))
  }, error = function(e) cat(paste("Error saving file '", filename, "': ", e$message, "\n", sep="")))
}


#' Find the Most Recently Modified File in a Folder
#'
#' @param folder_path Path to directory.
#' @return Full path to the most recent file.
#' @export
#' @examples
#' \dontrun{
#' latest_file <- mr_file("data/downloads")
#' }
mr_file <- function(folder_path) {
  all_paths <- list.files(folder_path, full.names = TRUE)
  if (length(all_paths) == 0) return(character(0))
  file_details <- file.info(all_paths)
  file_only_details <- file_details[!file_details$isdir, ]
  if (nrow(file_only_details) == 0) return(character(0))
  rownames(file_only_details)[which.max(file_only_details$mtime)]
}


#' Cross Check Missing Data
#'
#' @param df1 Primary dataframe.
#' @param df2 Reference dataframe.
#' @param check_var Column to check for NAs in df1.
#' @param id_var Common ID column.
#' @return Dataframe of matching records from df2.
#' @export
#' @importFrom dplyr filter pull
#' @examples
#' \dontrun{
#' cross_check_missing(df, addr_df, ADDR_LN1, PERS_ID)
#' }
cross_check_missing <- function(df1, df2, check_var, id_var) {
  missing_ids <- df1 %>%
    dplyr::filter(is.na({{ check_var }})) %>%
    dplyr::pull({{ id_var }}) %>%
    unique()
  result <- df2 %>%
    dplyr::filter({{ id_var }} %in% missing_ids)
  return(result)
}


#' Clean IDs (Remove Leading Zeros)
#'
#' @param x Vector of IDs.
#' @return Character vector.
#' @export
#' @importFrom stringr str_remove
#' @examples
#' \dontrun{
#' clean_ids(df$id_col)
#' }
clean_ids <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_remove("^0+") %>%
    trimws()
}


#' Descending Arrange
#'
#' @param data Dataframe.
#' @param var Variable to sort by.
#' @return Sorted dataframe.
#' @export
#' @importFrom dplyr arrange desc
#' @examples
#' \dontrun{
#' darange(df, appl_date)
#' }
darange <- function(data, var) {
  data %>%
    dplyr::arrange(dplyr::desc({{ var }}))
}


#' Safe Paste (NA Handling)
#'
#' @param ... Objects to paste.
#' @return Character string.
#' @export
#' @importFrom dplyr coalesce
#' @examples
#' \dontrun{
#' safe_paste(first_name, " ", last_name)
#' }
safe_paste <- function(...) {
  dots <- list(...)
  clean_dots <- lapply(dots, function(x) dplyr::coalesce(as.character(x), ""))
  do.call(paste0, clean_dots)
}


#' Lazy Line Plot (Updated Style)
#'
#' @param data Dataframe.
#' @param x_var X variable.
#' @param y_var Y variable.
#' @param title Plot title.
#' @param x_lab X label.
#' @param y_lab Y label.
#' @return ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs scale_y_continuous element_text theme
#' @importFrom scales comma
#' @examples
#' \dontrun{
#' lazy_line(df, week_num, total_kids)
#' }
lazy_line <- function(data, x_var, y_var, title = "Trend", x_lab = "X Axis", y_lab = "Y Axis") {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ x_var }}, y = {{ y_var }})) +
    ggplot2::geom_line(color = "red", linewidth = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Lazy Clean Names
#'
#' @param data Dataframe.
#' @return Dataframe with clean names.
#' @export
#' @importFrom janitor clean_names
#' @examples
#' \dontrun{
#' lazy_names(df)
#' }
lazy_names <- function(data) {
  janitor::clean_names(data)
}


#' Lazy Table (gt Style 6)
#'
#' @param data Dataframe.
#' @param title Table title.
#' @param rename_map Named vector for renaming columns.
#' @param center_cols Vector of columns to center.
#' @return gt table.
#' @export
#' @importFrom gt gt tab_header md cols_label opt_stylize cols_align
#' @examples
#' \dontrun{
#' lazy_table(df, rename_map = c(old_name = "New Name"))
#' }
lazy_table <- function(data,
                       title = "Table Overview",
                       rename_map = NULL,
                       center_cols = NULL) {
  tbl <- gt::gt(data) %>%
    gt::tab_header(title = gt::md(title)) %>%
    gt::opt_stylize(style = 6, color = "blue")
  if (!is.null(rename_map)) {
    tbl <- tbl %>%
      gt::cols_label(.list = rename_map)
  }
  if (!is.null(center_cols)) {
    tbl <- tbl %>%
      gt::cols_align(align = "center", columns = center_cols)
  }
  return(tbl)
}

# --- Internal Helper Functions (Not Exported) ---

.lcs_scalar <- function(str1, str2, ignore_strings = NULL) {
  if (!is.null(ignore_strings) && length(ignore_strings) > 0) {
    ignore_strings <- ignore_strings[order(nchar(ignore_strings), decreasing = TRUE)]
    for (term in ignore_strings) {
      if (!is.na(str1)) str1 <- stringr::str_replace_all(str1, stringr::fixed(term), "")
      if (!is.na(str2)) str2 <- stringr::str_replace_all(str2, stringr::fixed(term), "")
    }
    if (!is.na(str1)) str1 <- stringr::str_squish(str1)
    if (!is.na(str2)) str2 <- stringr::str_squish(str2)
  }
  if (is.na(str1) || is.na(str2) || nchar(str1) == 0 || nchar(str2) == 0) return(0)
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
        if (lcs_matrix[i, j] > max_len) max_len <- lcs_matrix[i, j]
      } else {
        lcs_matrix[i, j] <- 0
      }
    }
  }
  return(max_len)
}

.edit_dist_scalar <- function(str1, str2, ignore_strings = NULL) {
  if (!is.null(ignore_strings) && length(ignore_strings) > 0) {
    ignore_strings <- ignore_strings[order(nchar(ignore_strings), decreasing = TRUE)]
    for (term in ignore_strings) {
      if (!is.na(str1)) str1 <- stringr::str_replace_all(str1, stringr::fixed(term), "")
      if (!is.na(str2)) str2 <- stringr::str_replace_all(str2, stringr::fixed(term), "")
    }
    if (!is.na(str1)) str1 <- stringr::str_squish(str1)
    if (!is.na(str2)) str2 <- stringr::str_squish(str2)
  }
  if ((is.na(str1) || nchar(str1) == 0) && (is.na(str2) || nchar(str2) == 0)) return(0)
  if (is.na(str1) || nchar(str1) == 0) return(nchar(str2))
  if (is.na(str2) || nchar(str2) == 0) return(nchar(str1))
  if (str1 == str2) return(0)
  return(adist(str1, str2)[1, 1])
}

.tfidf_score_scalar <- function(str1, str2, tfidf_weights) {
  if (is.na(str1) || is.na(str2)) return(0)
  words1 <- unlist(strsplit(tolower(str1), "\\s+"))
  words2 <- unlist(strsplit(tolower(str2), "\\s+"))
  common_words <- intersect(words1, words2)
  if (length(common_words) == 0) return(0)
  score <- tfidf_weights %>%
    dplyr::filter(word %in% common_words) %>%
    dplyr::summarise(total_score = sum(idf)) %>%
    dplyr::pull(total_score)
  if (length(score) == 0) return(0)
  return(score)

}


#' Collapse consecutive date ranges
#'
#' Groups rows by specified variables and merges consecutive date ranges.
#' Ranges are considered consecutive if the start date of a row is
#' exactly one day after the end date of the previous row in the group.
#'
#' @param df A data.frame or tibble.
#' @param group_vars A character vector of column names to group by.
#' @param start_var The name of the start date column (unquoted).
#' @param end_var The name of the end date column (unquoted).
#' @return A data.frame with consecutive date ranges merged.
#' @export
#' @importFrom dplyr group_by arrange mutate lag ungroup summarize
#' @importFrom rlang syms enquo
#' @examples
#' \dontrun{
#' # If row 1 ends 2023-01-01 and row 2 starts 2023-01-02, they will be merged.
#' collapse_dates(df, group_vars = c("id", "category"), start_date, end_date)
#' }
collapse_dates <- function(df, group_vars, start_var, end_var) {
  
  # Convert string inputs to symbols for tidy evaluation
  group_syms <- rlang::syms(group_vars)
  start_quo <- rlang::ensym(start_var)
  end_quo <- rlang::ensym(end_var)
  
  df %>%
    # 1. Group by the user's ID variables
    dplyr::group_by(!!!group_syms) %>%
    # 2. Arrange by start date to ensure correct order
    dplyr::arrange(!!start_quo, .by_group = TRUE) %>%
    dplyr::mutate(
      # 3. specific logic: if start <= previous end + 1, it's a continuation
      # (Use +1 to handle the "consecutive day" requirement)
      # We default the first row to being a "new group" (TRUE)
      is_new_group = is.na(dplyr::lag(!!end_quo)) | 
                     !!start_quo > (dplyr::lag(!!end_quo) + 86400),
      # 4. Create a unique ID for each continuous block
      group_id = cumsum(is_new_group)
    ) %>%
    # 5. Group by original vars AND the new block ID
    dplyr::group_by(!!!group_syms, group_id) %>%
    # 6. Collapse to min start and max end
    dplyr::mutate(
      new_start = min(!!start_quo),
      new_end = max(!!end_quo)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::unique() %>%
    # 7. cleanup
    dplyr::select(-group_id) %>%
    dplyr::rename(
      !!rlang::quo_name(start_quo) := new_start,
      !!rlang::quo_name(end_quo) := new_end
    )
}


