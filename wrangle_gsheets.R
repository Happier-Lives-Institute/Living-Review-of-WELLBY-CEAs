#~############################################################################~#
# Gsheets to csv ----
#~############################################################################~#

# Load dependencies
source("dependencies/dependencies.R")

living_review_url <- "https://docs.google.com/spreadsheets/d/1qcNT4QXurBW52OKBBy8b4ty6YqppkTD9Xu-4Kq0JbwE/"
url_comparisons   <- "https://docs.google.com/spreadsheets/d/1xMA664duSlM7CLhMOwFirBJw6k_c83cL-lcOXl06qPU/"

gsheets <- list(
  list(file = "living_review_table",      ss = living_review_url, sheet = "Living Review Table"),
  list(file = "botecs_from_whr",          ss = living_review_url, sheet = "BOTECs from the WHR"),
  list(file = "other",                    ss = living_review_url, sheet = "Other"),
  list(file = "charity_comparisons_table", ss = url_comparisons,  sheet = "Charity comparisons table")
)

out_dir <- "data/gsheets"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#~=======================================================~=
## Fetch each sheet ----
#~=======================================================~=

for (g in gsheets) {

  print(paste0("      Fetching ", g$sheet, "..."))

  dat <- read_sheet(ss = g$ss, sheet = g$sheet)

  # A column holding more than one type comes back as a list, which write_csv
  # cannot take. as.character keeps the stored number, unlike col_types = "c"
  # which would give the rounded text the sheet displays.
  dat[] <- lapply(dat, function(col) {
    if(!is.list(col)) return(col)
    sapply(col, function(x) if(length(x) == 0) NA_character_ else as.character(x)[1])
  })

  path <- file.path(out_dir, paste0(g$file, ".csv"))
  write_csv(dat, path)

  print(paste0("      Written ", path, " (", nrow(dat), " rows)"))
}

print("   Finished fetching gsheets.")
