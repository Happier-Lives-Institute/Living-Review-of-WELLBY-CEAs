# Load dependencies
source("dependencies/dependencies.R")

# Load the data and wrangle it
source("wrangle.R")

# Settings for analyses
my_settings <- data.frame(
  version = c("living_review", "all", "only_add_typical_acts"),
  max_x_CpWB = c(12500, 51000, 51000),
  breaks_x_CpWB = c(2500, 10000, 10000),
  richtext_x_CpWB = c(7050, 28500, 28500),
  richtext_y_CpWB = c(9.75, 13.25, 11.25),
  richtext_x_WBp1k = c(61.5, 61.5, 61.5),
  richtext_y_WBp1k = c(6, 9, 7),
  max_x_evaluators_CpWB = c(10000, 40000, 40000),
  breaks_x_evaluators_CpWB = c(2000, 10000, 10000),
  max_x_evaluators_WBp1k = c(120, 120, 120),
  breaks_x_evaluators_WBp1k = c(20, 20, 20),
  comparison_height = c(3*300, 4*300, 4*300)
)

# run all analyses
for (i in 1:nrow(my_settings)) {
  current_settings <- my_settings %>% filter(version == my_settings$version[i])
  
  if(current_settings$version == "living_review") {
    living_review_data <- living_review_data_temp %>% filter(
      publication_status == "Published"
    )
  }
  
  if(current_settings$version == "all") {
    living_review_data <- living_review_data_temp
  }
  
  if(current_settings$version == "only_add_typical_acts") {
    living_review_data <- living_review_data_temp %>% filter(
      publication_status == "Published" | charity %in% c(
        "Hypothetical homeless charity (1)",
        "Hypothetical homeless charity (2)",
        "Guide Dogs UK"
      )
    )
  }

  print(paste0("Running analyses for version: ", current_settings$version))
  source("graphs.R")
  source("tables.R")

  # Number of CEAs
  print(nrow(living_review_data))
  
  # Number of CEAs per evaluator
  living_review_data %>%
    group_by(evaluator) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>% print()
  
  # Number of CEAs per target country
  living_review_data %>%
    group_by(country_income_simple) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>% print()
  
}
