# --- Tidyverse: Core Manipulation ---
#' @importFrom dplyr mutate select filter group_by summarise ungroup arrange desc
#' @importFrom dplyr left_join inner_join right_join full_join semi_join anti_join
#' @importFrom dplyr rename distinct count pull n row_number lag lead
#' @importFrom dplyr if_else case_when coalesce na_if
#' @importFrom dplyr across where everything any_of all_of
#' @importFrom dplyr bind_rows bind_cols
#'
# --- Tidyverse: Reshaping ---
#' @importFrom tidyr pivot_longer pivot_wider separate unite
#' @importFrom tidyr unnest nest fill drop_na replace_na
#'
# --- Tidyverse: Strings & Regex ---
#' @importFrom stringr str_detect str_replace str_replace_all str_remove str_remove_all
#' @importFrom stringr str_extract str_extract_all str_sub str_pad
#' @importFrom stringr str_trim str_squish str_c str_length
#' @importFrom stringr str_to_lower str_to_upper str_to_title fixed
#' @importFrom glue glue
#'
# --- Tidyverse: Dates (Lubridate) ---
#' @importFrom lubridate year month day wday quarter semester
#' @importFrom lubridate ymd mdy dmy ymd_hms
#' @importFrom lubridate floor_date ceiling_date round_date
#' @importFrom lubridate today now
#' @importFrom lubridate interval %within%
#' @importFrom lubridate years months days weeks
#' @importFrom lubridate make_date make_datetime
#'
# --- Excel & IO ---
#' @importFrom readxl read_excel excel_sheets
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook write.xlsx
#' @importFrom janitor clean_names tabyl get_dupes remove_empty
#'
# --- Plotting (ggplot2) ---
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_line geom_bar geom_point geom_histogram geom_boxplot geom_text geom_label geom_vline geom_hline
#' @importFrom ggplot2 theme_minimal theme_classic theme_bw theme element_text element_blank element_rect
#' @importFrom ggplot2 labs ggtitle xlab ylab
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous scale_color_manual scale_fill_manual
#' @importFrom ggplot2 facet_wrap facet_grid
#' @importFrom scales comma percent dollar
#'
# --- Tables ---
#' @importFrom gt gt tab_header md cols_label opt_stylize cols_align tab_options
#'
# --- Advanced R Programming ---
#' @importFrom rlang enquos enquo quo_name current_env quo_get_expr expr new_quosure as_name sym syms eval_tidy
#' @importFrom purrr map map_df map_chr map_lgl map2 walk
#' @importFrom tibble tibble tribble
#'
# --- Base Stats & Utils ---
#' @importFrom stats median quantile sd var cor
#' @importFrom utils head tail str
#' @importFrom methods is
NULL
