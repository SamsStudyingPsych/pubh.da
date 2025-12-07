admincleanr

A Toolkit for Administrative Data Cleaning and Record Linkage

admincleanr is an R package designed for data analysts working with "trench warfare" data—messy extracts from case management systems, government databases, and administrative backends.

While the tidyverse is excellent for general data manipulation, it doesn't always handle the specific pain points of administrative data: mismatched ID types, memory management for massive extracts, and "dirty" record linkage. This package bridges that gap.

Features

admincleanr focuses on four core areas:

Environment Management: Keep your R session from crashing when working with large datasets.

Dirty Record Linkage: Fuzzy matching and cross-checking for mismatched IDs or names.

Data Hygiene: Safer wrappers for standard cleaning tasks (handling NAs, dates, and strings).

Lazy Visualization: Quick, publication-ready plots for distribution checks.

Installation

You can install the development version from GitHub:

# install.packages("devtools")
devtools::install_github("yourusername/admincleanr")


Usage Highlights

1. Memory Management

Analysts often work with massive CSV dumps that clog up RAM. clean_but_keep() allows you to wipe your environment clean while preserving only the specific datasets and functions you need to continue your analysis.

# Clear everything from memory EXCEPT your main dataframe and your lookup table
clean_but_keep(df, lookup_table)


2. Diagnosing Join Failures

When a left_join returns fewer rows than expected, cross_check_missing() helps you instantly identify which records failed to match.

# Find all rows in df where the address is missing, 
# and pull those specific People IDs from the master address list for inspection.
missing_records <- cross_check_missing(
  df1 = df, 
  df2 = address_master, 
  check_var = ADDRESS_LINE_1, 
  id_var = PERSON_ID
)


3. Record Linkage & String Matching

Administrative data often contains slightly different names for the same entity (e.g., "ABC Learning Center" vs. "ABC Learning Ctr LLC").

# Find the best match for a name from an approved list
# Returns the longest substring match if a perfect match isn't found
match <- find_best_match("ABC Learning Center LLC", approved_providers)

# Calculate similarity scores for fuzzy matching
df <- df %>%
  mutate(
    lev_dist = calculate_edit_distance(name_col_1, name_col_2),
    overlap_score = count_consecutive_overlap(name_col_1, name_col_2)
  )


4. Safe Data Cleaning

Standard R functions can be risky with administrative data. admincleanr provides safer alternatives.

# clean_ids: Removes leading zeros and trims whitespace without corrupting IDs
df$id <- clean_ids(df$raw_id_col)

# safe_paste: Pastes strings together but treats NAs as empty strings "" 
# instead of the text "NA"
df$full_name <- safe_paste(first_name, " ", middle_name, " ", last_name)


5. Quick Visualization

Generate standardized, publication-ready histograms and trend lines with minimal code.

# Instantly see the distribution of a variable with mean lines and counts
lazy_hist(df, "age", binwidth_val = 5)

# Quick trend line with standard formatting
lazy_line(df, x_var = week_num, y_var = total_cases)


Contributing

This package was born out of the need to streamline public health and case management data analytics. Pull requests are welcome for any function that solves a repetitive administrative data task.

I am actively looking for feedback to make this toolkit more robust for the public health and administrative data community. If you have ideas for improvements, new features, or just want to discuss the "trench warfare" of data cleaning, please feel free to reach out to me directly at sam.a.barans@gmail.com.
