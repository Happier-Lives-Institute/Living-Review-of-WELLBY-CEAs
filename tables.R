#~############################################################################~#
# Summary table ----
#~############################################################################~#
# Sourced once per analysis version from the main.R loop. Reads
# `living_review_data` (already filtered for the current version) and
# `current_settings$version` from the loop environment, and writes a Word
# table into graphs/<version>/table.docx.

# Output directory (created in graphs.R; recreate defensively in case this is
# ever sourced on its own)
graph_dir <- file.path("graphs", current_settings$version)
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

# Flatten possible list-columns (duration and others arrive as list-columns
# because their type differs across the three source sheets) to a single
# displayable string per row.
flatten_col <- function(x) {
  map_chr(x, ~ if (length(.x) == 0 || all(is.na(.x))) {
    NA_character_
  } else {
    paste(as.character(.x), collapse = ", ")
  })
}

# Replace NA with an empty string for clean display
blank_na <- function(x) ifelse(is.na(x), "", x)

# Build the display frame: sorted most cost-effective first, columns and
# headers exactly as requested.
table_data <- living_review_data %>%
  arrange(desc(WBp1k)) %>%
  transmute(
    `Charity`                            = blank_na(charity),
    `What the charity does`              = blank_na(intervention),
    `Cost per WELLBY`                    = blank_na(paste0("$", round_c(CpWB, 0))),
    `WELLBYs created per $1,000 donated` = blank_na(as.character(round_c(WBp1k, 1))),
    `Duration of effect (years)`         = blank_na(flatten_col(duration)),
    `Country income`                     = blank_na(country_income),
    `Total sample`                       = blank_na(flatten_col(total_sample)),
    `Total studies`                      = blank_na(flatten_col(total_studies)),
    `Causal evidence`                    = blank_na(flatten_col(causal_evidence)),
    `Evidence relevance`                 = blank_na(flatten_col(evidence_relevance)),
    `Depth of analysis`                  = blank_na(depth_of_analysis),
    `Publication status`                 = blank_na(publication_status),
    `Evaluator`                          = blank_na(evaluator)
  )

# Build and style the flextable
ft <- flextable(table_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(align = "left", part = "all") %>%
  valign(valign = "top", part = "body") %>%
  set_table_properties(layout = "autofit")

# Save to Word
save_as_docx(ft, path = file.path(graph_dir, "table.docx"))
