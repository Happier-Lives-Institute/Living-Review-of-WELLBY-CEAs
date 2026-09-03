#~############################################################################~#
# Gsheets to csv ----
#~############################################################################~#

library(googlesheets4)

living_review_url <- "https://docs.google.com/spreadsheets/d/1qcNT4QXurBW52OKBBy8b4ty6YqppkTD9Xu-4Kq0JbwE/"

gsheets <- list(
  list(file = "living_review_table", sheet = "Living Review Table"),
  list(file = "botecs_from_whr",     sheet = "BOTECs from the WHR"),
  list(file = "other",               sheet = "Other")
)

out_dir <- "data/gsheets"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#~=======================================================~=
## Fetch each sheet ----
#~=======================================================~=

for (g in gsheets) {

  print(paste0("      Fetching ", g$sheet, "..."))

  dat <- read_sheet(ss = living_review_url, sheet = g$sheet)

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
