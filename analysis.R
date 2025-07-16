
# Get the data
source("wrangle.R")

# Number of CEAs
nrow(living_review_data)

# Number of CEAs per evaluator
living_review_data %>%
  group_by(evaluator) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
