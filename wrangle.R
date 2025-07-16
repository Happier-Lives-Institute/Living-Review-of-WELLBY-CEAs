#~############################################################################~#
# Preparations ----
#~############################################################################~#

# Load dependencies
source("dependencies/dependencies.R")

# Load the data
living_review_data <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1qcNT4QXurBW52OKBBy8b4ty6YqppkTD9Xu-4Kq0JbwE/",
  sheet = "Living Review Table"
)

# Clean the names
living_review_data <- living_review_data %>%
  rename(
    charity = `Charity`,
    CpWB = `Cost per WELLBY`,
    WBp1k = `WELLBYs created per $1,000 donated`,
    duration = `Duration of effect (years)`,
    country_income = `Country income`,
    total_sample = `Total sample`,
    total_studies = `Total studies`,
    causal_evidence = `Causal evidence`,
    evidence_relevance = `Evidence relevance`,
    depth_of_analysis = `Depth of analysis`,
    publication_status = `Publication status`,
    evaluator = `Evaluator`,
    intervention = `What the charity does`,
    dosage = `Dosage`
  )

# Take only useful columns
living_review_data <- living_review_data %>%
  select(
    charity, intervention, CpWB, WBp1k, country_income,
    depth_of_analysis, publication_status, evaluator
  )

# Fix variables with strings in numeric
living_review_data$CpWB <- map_dbl(living_review_data$CpWB, ~ as.numeric(.x))
living_review_data$WBp1k <- map_dbl(living_review_data$WBp1k, ~ as.numeric(.x))

# General wrangling of variables
living_review_data <- living_review_data %>%
  mutate(
    # And a simplified country variable
    country_income_simple = case_when(
      str_detect(country_income, "HIC") ~ "HICs", 
      .default = country_income
    ),
    # Clean up evaluators that will go under "Krekel and colleagues"
    evaluator = ifelse(
      str_detect(evaluator, "Krekel"), "Krekel and colleagues", evaluator
    ),
    # Make a numeric depth equivalent
    depth_of_analysis = case_when(
      depth_of_analysis == "Shallow" ~ 1,
      depth_of_analysis == "Medium" ~ 2,
      depth_of_analysis == "In-depth" ~ 4,
      TRUE ~ NA_real_
    )
  )

#~############################################################################~#
# Create different selections ----
#~############################################################################~#

# Filter out rows with NAs
living_review_data <- living_review_data %>%
  filter(!is.na(CpWB))

# # Remove BOTECs and unpublished
# living_review_data <- living_review_data %>%
#   filter(
#     !str_detect(publication_status, "BOTEC"),
#     !str_detect(publication_status, "Unpublished")
#   )
