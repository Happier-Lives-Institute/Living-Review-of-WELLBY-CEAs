
# Load the data
living_review_data <- read_csv("data/gsheets/living_review_table.csv", col_types = cols(.default = "c"))

# Get BOTEC/Unpublished living review data
living_review_data_botecs <- read_csv("data/gsheets/botecs_from_whr.csv", col_types = cols(.default = "c"))

# Other studies
living_review_data_other <- read_csv("data/gsheets/other.csv", col_types = cols(.default = "c"))

living_review_data <- bind_rows(living_review_data, living_review_data_botecs) %>%
  bind_rows(living_review_data_other)

# Fix variables with strings in numeric
# gsub because the cells are currency formatted, e.g. $4,838.71
living_review_data$`Cost per WELLBY` <- as.numeric(gsub("[$,]", "", living_review_data$`Cost per WELLBY`))
living_review_data$`WELLBYs created per $1,000 donated` <- as.numeric(gsub("[$,]", "", living_review_data$`WELLBYs created per $1,000 donated`))

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
    dosage = `Dosage`,
    recommendation = `HLI recommendation`
  )

# Take only useful columns
living_review_data <- living_review_data %>%
  select(
    charity, intervention, CpWB, WBp1k, duration, country_income,
    total_sample, total_studies, causal_evidence, evidence_relevance,
    depth_of_analysis, publication_status, evaluator, recommendation
  )

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
    # Make a numeric depth equivalent (used for the size aesthetic in plots;
    # the original text is preserved in depth_of_analysis for the tables)
    depth_of_analysis_num = case_when(
      depth_of_analysis == "Shallow" ~ 1,
      depth_of_analysis == "Medium" ~ 2,
      depth_of_analysis == "In-depth" ~ 4,
      TRUE ~ NA_real_
    )
  )

# Filter out rows with NAs
living_review_data <- living_review_data %>%
  filter(!is.na(CpWB))

# Always remove life boats
living_review_data <- living_review_data %>% filter(charity != "Royal National Lifeboat Institution")

# Save data because we are going to loop through it
living_review_data_temp <- living_review_data

#~############################################################################~#
# Charity comparisons table ----
#~############################################################################~#
# A separate curated sheet, used for the evaluated charities figure

charity_comparison_data <- read_csv(
  "data/gsheets/charity_comparisons_table.csv", col_types = cols(.default = "c")
)

charity_comparison_data <- charity_comparison_data %>%
  mutate(
    CpWB  = as.numeric(gsub("[$,]", "", CpWB)),
    WBp1k = as.numeric(gsub("[$,]", "", WBp1k)),
    # Make a numeric depth equivalent
    depth_of_analysis = case_when(
      depth_of_analysis == "Shallow" ~ 1,
      depth_of_analysis == "Medium" ~ 2,
      depth_of_analysis == "In-depth" ~ 4,
      TRUE ~ NA_real_
    ),
    # Create a charity label that has name and intervention
    charity_label = ifelse(
      !is.na(intervention),
      paste0(charity, "\n[", intervention, "]"),
      charity
    )
  )
