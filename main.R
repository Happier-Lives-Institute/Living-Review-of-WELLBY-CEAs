# Load dependencies
source("dependencies/dependencies.R")

# Load the data and wrangle it
source("wrangle.R")

# Settings for analyses
my_settings <- data.frame(
  version = c("living_review", "all", "only_add_typical_acts", "other_graphs"),
  max_x_CpWB = c(12500, 51000, 51000, NA),
  breaks_x_CpWB = c(2500, 10000, 10000, NA),
  richtext_x_CpWB = c(7050, 28500, 28500, NA),
  richtext_y_CpWB = c(9.75, 13.25, 11.25, NA),
  richtext_x_WBp1k = c(61.5, 61.5, 61.5, NA),
  richtext_y_WBp1k = c(6, 9, 7, NA),
  max_x_evaluators_CpWB = c(10000, 40000, 40000, NA),
  breaks_x_evaluators_CpWB = c(2000, 10000, 10000, NA),
  max_x_evaluators_WBp1k = c(120, 120, 120, NA),
  breaks_x_evaluators_WBp1k = c(20, 20, 20, NA),
  comparison_height = c(3*300, 4*300, 4*300, NA)
)

# Evaluator colours
evaluator_colours <- c(
  "State of Life"             = "#F4C430"
  , "Krekel and colleagues"   = "#CD5C5C"
  , "Pro Bono Economics"      = "#73937e"
  , "Happier Lives Institute" = "#2361b7"
)

# Do we want svgs to have the same ratio
svg_ratio_setting <- T

# run all analyses
for (i in 1:nrow(my_settings)) {
  current_settings <- my_settings %>% filter(version == my_settings$version[i])
  
  # Output each analysis version into its own subfolder
  graph_dir <- file.path("graphs", current_settings$version)
  dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)
  
  print(paste0("Running analyses for version: ", current_settings$version))
  if(current_settings$version == "other_graphs") {
    
    living_review_data <- living_review_data_temp %>% filter(
      publication_status == "Published" | charity %in% c(
        "Hypothetical homeless charity (1)",
        "Hypothetical homeless charity (2)",
        "Guide Dogs UK"
      )
    )
    source("prepare_graph_data.R")
    source("other_graphs.R")
    
  } else {
    
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
    
    source("prepare_graph_data.R")
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
}