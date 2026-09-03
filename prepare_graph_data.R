#~############################################################################~#
# Prepare the graph data ----
#~############################################################################~#

# Create the charity labels
living_review_data <- living_review_data %>%
  mutate(
    income_label = ifelse(
      country_income_simple == "HICs",
      "<span style='color:#0072B2'>(HIC)</span>",
      "<span style='color:#E69F00'>(LMIC)</span>"
    ),
    charity_label = paste0(
      "<b>",charity,"</b>",
      " <span style='font-size:10pt'>[", intervention, "]</span> ", income_label
    ),
    charity_label_living = paste0(
      charity, " ", income_label,
      "<br><span style='font-size:10pt'>[", intervention, "]</span>"
    ),
    WBp1k_label = ifelse(
      WBp1k < 1, paste0(" ", round_c(WBp1k, 2)),
      paste0(" ", round_c(WBp1k, 1))
    )
  )

living_review_data_in_sample <- living_review_data %>% 
  filter(publication_status == "Published")

# The most/least cost-effective charity is always taken from the published sample
most_cost_effective_row <- living_review_data_in_sample %>%
  slice_max(WBp1k, n = 1, with_ties = FALSE)
least_cost_effective_row <- living_review_data_in_sample %>%
  slice_min(WBp1k, n = 1, with_ties = FALSE)

most_cost_effective_charity  <- most_cost_effective_row$charity
least_cost_effective_charity <- least_cost_effective_row$charity

# Labels for the two single-charity groups in the comparison plots.
label_most  <- paste0("Most cost-effective charity\nin sample (", most_cost_effective_charity, ")")
label_least <- paste0("Least cost-effective\ncharity in sample\n(", least_cost_effective_charity, ")")

# Create comparison data based on version
if(current_settings$version == "living_review") {

  data_comparison <- rbind(
    living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>%
      filter(rank < 6) %>% summarise(
        charity = "Top 5 charities",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>% filter(charity == most_cost_effective_charity) %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = label_most
      ),
    living_review_data %>% filter(charity == least_cost_effective_charity) %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = label_least
      ),
    living_review_data %>%
      filter(country_income_simple == "HICs") %>% summarise(
        charity = "Charities operating in HICs (UK)",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>%
      filter(country_income_simple == "LMICs") %>% summarise(
        charity = "Charities operating in LMICs",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      )
  ) %>% arrange(CpWB)

} else {

  data_comparison <- rbind(
    living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>%
      filter(rank < 6) %>% summarise(
        charity = "Top 5 charities",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>% filter(charity == most_cost_effective_charity) %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = label_most
      ),
    living_review_data %>% filter(charity == least_cost_effective_charity) %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = label_least
      ),
    living_review_data %>%
      filter(country_income_simple == "HICs" &
               publication_status != "BOTEC for WHR chapter"
      ) %>% summarise(
        charity = "Charities operating in HICs (UK)\nnot counting Guide Dogs UK\nand homelessness BOTECS",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>%
      filter(country_income_simple == "LMICs") %>% summarise(
        charity = "Charities operating in LMICs",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>%
      filter(publication_status == "BOTEC for WHR chapter") %>% summarise(
        charity = "BOTECs of Guide Dogs UK\nand homelessness\nfor the WHR chapter",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      )
  ) %>% arrange(CpWB)
}
