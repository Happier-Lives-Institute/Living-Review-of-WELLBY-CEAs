
living_review_url <- "https://docs.google.com/spreadsheets/d/1qcNT4QXurBW52OKBBy8b4ty6YqppkTD9Xu-4Kq0JbwE/"

# Load the data
living_review_data <- read_sheet(living_review_url, sheet = "Living Review Table")

# Get BOTEC/Unpublished living review data
living_review_data_botecs <- read_sheet(living_review_url, sheet = "BOTECs from the WHR")

# Other studies
living_review_data_other <- read_sheet(living_review_url, sheet = "Other")

# Fix variables with strings in numeric
living_review_data$`Cost per WELLBY` <- map_dbl(living_review_data$`Cost per WELLBY`, ~ as.numeric(.x))
living_review_data$`WELLBYs created per $1,000 donated` <- map_dbl(living_review_data$`WELLBYs created per $1,000 donated`, ~ as.numeric(.x))
living_review_data_other$`Duration of effect (years)` <- as.list(living_review_data_other$`Duration of effect (years)`)

living_review_data <- bind_rows(living_review_data, living_review_data_botecs) %>% 
  bind_rows(living_review_data_other)

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
    charity, intervention, CpWB, WBp1k, country_income,
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
    # Make a numeric depth equivalent
    depth_of_analysis = case_when(
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
