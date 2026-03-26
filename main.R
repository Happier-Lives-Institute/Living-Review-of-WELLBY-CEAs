# Load dependencies
source("dependencies/dependencies.R")

# Load the data and wrangle it
source("wrangle.R")

# Settings for analyses
my_settings <- data.frame(
  version = c("living_review", "all"),
  max_x_CpWB = c(12500, 51000),
  breaks_x_CpWB = c(2500, 10000),
  richtext_x_CpWB = c(7050, 28500),
  richtext_y_CpWB = c(9.75, 13.25),
  richtext_x_WBp1k = c(61.5, 61.5),
  richtext_y_WBp1k = c(6, 9),
  max_x_evaluators = c(5000, 15000),
  breaks_x_evaluators = c(1000, 2500),
  comparison_height = c(3*300, 4*300)
)

# run all analyses
for (i in 1:nrow(my_settings)) {
  current_settings <- my_settings %>% filter(version == my_settings$version[i])
  
  if(current_settings$version == "living_review") {
    living_review_data <- living_review_data_temp %>% filter(
      publication_status == "Published"
    )
  } else if(current_settings$version == "all") {
    living_review_data <- living_review_data_temp
  }

  print(paste0("Running analyses for version: ", current_settings$version))
  source("graphs.R")

  # Number of CEAs
  print(nrow(living_review_data))
  
  # Number of CEAs per evaluator
  living_review_data %>%
    group_by(evaluator) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>% print()
  
}
