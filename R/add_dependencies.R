# --- Core Data & Analysis ---
usethis::use_package("data.table")
usethis::use_package("psych")
usethis::use_package("tidyverse")
usethis::use_package("janitor")
usethis::use_package("zoo")
usethis::use_package("Hmisc")
usethis::use_package("skimr")
usethis::use_package("fastDummies")

# --- IO & Database ---
usethis::use_package("readxl")
usethis::use_package("openxlsx")
usethis::use_package("arrow")
usethis::use_package("excel.link")
usethis::use_package("odbc")
usethis::use_package("DBI")

# --- Geospatial & Maps ---
usethis::use_package("sf")
usethis::use_package("tigris")
usethis::use_package("tidygeocoder")
usethis::use_package("zipcodeR")
usethis::use_package("osrm")
usethis::use_package("gmapsdistance")
usethis::use_package("googleway")

# --- Visualization & Tables ---
usethis::use_package("gt")
usethis::use_package("tinytable")
usethis::use_package("ggnewscale")
usethis::use_package("ggbreak")
usethis::use_package("scales")

# --- Text & String Manipulation ---
usethis::use_package("glue")
usethis::use_package("stringr")
usethis::use_package("tidytext")
usethis::use_package("labelled")

# --- Utilities ---
usethis::use_package("tictoc")
usethis::use_package("lubridate")
usethis::use_package("progress")
usethis::use_package("rlang")
usethis::use_package("reticulate")

# --- Base R Packages ---
# These are standard in R, but good to be explicit if you use them heavily
usethis::use_package("grDevices")
usethis::use_package("tools")

# --- Development Tools (Suggests) ---
# These are only needed by YOU to build the package, not by the user to run it
usethis::use_package("usethis", type = "Suggests")
usethis::use_package("devtools", type = "Suggests")
