#~############################################################################~#
# Preparations ----
#~############################################################################~#

# Output each analysis version into its own subfolder
graph_dir <- file.path("graphs", current_settings$version)
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

# Evaluator colours
evaluator_colours <- c(
  "State of Life"             = "#F4C430"
  , "Krekel and colleagues"   = "#CD5C5C"
  , "Pro Bono Economics"      = "#73937e"
  , "Happier Lives Institute" = "#2361b7"
)

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

height_large_graphs <- max(75*nrow(living_review_data), 1750)

#~============================================================================~=
## Reusable plot elements ----
#~============================================================================~=

# LMIC/HIC explanatory annotation reused across the two large dot plots
income_annotation_label <- paste0(
  "<span style='color:#E69F00;font-size:9pt;'>(LMIC)</span> operates in low- or<br>",
  "middle-income countries<br><br>",
  "<span style='color:#0072B2;font-size:9pt'>(HIC)</span> operates in<br>high-income countries"
)

# Evaluator colour scale reused across every plot coloured by evaluator
evaluator_color_scale <- scale_color_manual(name = "Evaluator", values = evaluator_colours)

# Depth-of-analysis size scale reused across the three dot plots
depth_size_scale <- scale_size_continuous(
  name = "Depth of analysis",
  breaks = c(1, 2, 4),
  labels = c("Shallow", "Medium", "In-depth"),
  guide = "legend"
)

# Common legend styling for the dot plots (position/margin set per plot)
dotplot_legend_theme <- theme(
  text = element_text(family = "Avenir"),
  axis.text.y = ggtext::element_markdown(),
  legend.box.background = element_rect(fill = "transparent", color = "black"),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 11)
)

# Fill palettes for the grouped comparison bars (5 groups vs 6 with BOTECs)
comparison_fill_5 <- c(
  "Least cost-effective\ncharity in sample\n(Football Beyond Borders)" = "#3498DB",
  "Charities operating in HICs (UK)" = "#B39BC8",
  "Top 5 charities" = "#F5B041",
  "Most cost-effective charity\nin sample (Pure Earth)" = "#27AE60",
  "Charities operating in LMICs" = "#D98880"
)
comparison_fill_6 <- c(
  "Least cost-effective\ncharity in sample\n(Football Beyond Borders)" = "#3498DB",
  "Charities operating in HICs (UK)\nnot counting Guide Dogs UK\nand homelessness BOTECS" = "#B39BC8",
  "Top 5 charities" = "#F5B041",
  "Most cost-effective charity\nin sample (Pure Earth)" = "#27AE60",
  "Charities operating in LMICs" = "#D98880",
  "BOTECs of Guide Dogs UK\nand homelessness\nfor the WHR chapter" = "#F1948A"
)

# Helper: pull a variable for the charity matching a (fixed) name pattern
pull_charity <- function(pattern, var) {
  living_review_data %>% filter(grepl(pattern, charity, fixed = TRUE)) %>% pull({{ var }})
}

# Combine a CpWB and a WBp1k plot into one side-by-side figure labelled by metric
make_double <- function(p_cpwb, p_wbp1k) {
  p_cpwb + p_wbp1k +
    plot_layout(ncol = 2)
}

#~############################################################################~#
# WBp1k graph ----
#~############################################################################~#

p_WBp1k <- living_review_data %>%
  ggplot(aes(y = reorder(charity_label, WBp1k), x = WBp1k,
             color = evaluator,
             size = depth_of_analysis_num,
  )) +
  geom_text(
    aes(label = WBp1k_label),
    hjust = -0.4, color = "black", size = 4, alpha = 1
  ) +
  coord_cartesian(xlim = c(0,120)) +
  scale_x_continuous(
    breaks = seq(0, 120, 10),
    expand = expansion(mult = c(0.02,0.06))
  ) +
  geom_segment(
    aes(x = 0, xend = WBp1k, y = charity_label, yend = charity_label),
    linewidth = 1, show.legend = F
  ) +
  geom_point() +
  theme_hli_wbg() +
  scale_alpha(range = c(0.5, 1), guide = "none") +
  ylab("") +
  xlab("WELLBYs created per $1,000 donated") +
  evaluator_color_scale +
  depth_size_scale +
  annotate(
    "richtext",
    x = current_settings$richtext_x_WBp1k,
    y = current_settings$richtext_y_WBp1k,
    label = income_annotation_label,
    hjust = 0,
    family = "Avenir",
    fill = NA,
    label.color = NA
  ) +
  dotplot_legend_theme +
  theme(
    legend.position = c(0.50, 0.45),
    legend.box.margin = margin(3, 5, 85, 5)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)
  )

# Alt-text for the plot
fmt_wbp1k         <- function(x) ifelse(x < 1, round_c(x, 2), round_c(x, 1))
pure_earth_wbp1k  <- pull_charity("Pure Earth", WBp1k)
taimaka_wbp1k     <- pull_charity("Taimaka", WBp1k)
friendship_wbp1k  <- pull_charity("Friendship Bench", WBp1k)
strongminds_wbp1k <- pull_charity("StrongMinds", WBp1k)
fbb_wbp1k         <- pull_charity("Football Beyond Borders", WBp1k)
guide_dogs_wbp1k  <- pull_charity("Guide Dogs", WBp1k)

title_WBp1k <- paste0(
  "Dot plot of ", nrow(living_review_data), " interventions ranked by WELLBYs created per $1,000 donated. ",
  "LMIC interventions dominate the top, led by Pure Earth Ghana (", fmt_wbp1k(pure_earth_wbp1k), "), ",
  "Taimaka (", fmt_wbp1k(taimaka_wbp1k), "), ",
  "Friendship Bench (", fmt_wbp1k(friendship_wbp1k), "), and ",
  "StrongMinds (", fmt_wbp1k(strongminds_wbp1k), "). ",
  "HIC interventions cluster near zero, with Football Beyond Borders producing just ",
  fmt_wbp1k(fbb_wbp1k), " WELLBYs per $1,000 donated",
  if (current_settings$version == "all" && length(guide_dogs_wbp1k) > 0)
    paste0(" and Guide Dogs UK producing just ", fmt_wbp1k(guide_dogs_wbp1k), " WELLBYs per $1,000")
  else "",
  "."
)

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "WBp1k"),
  plot = p_WBp1k,
  width = 3000,
  height = height_large_graphs,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = title_WBp1k
)

#~############################################################################~#
# CpWB graph ----
#~############################################################################~#

p_CpWB <- living_review_data %>%
  ggplot(aes(y = reorder(charity_label, -CpWB), x = CpWB,
             color = evaluator,
             size = depth_of_analysis_num,
  )) +
  geom_text(
    aes(label = scales::dollar_format()(CpWB)),
    hjust = -0.3, color = "black", size = 4, alpha = 1
  ) +
  coord_cartesian(xlim = c(0,current_settings$max_x_CpWB)) +
  scale_x_continuous(
    labels = scales::dollar,
    breaks = seq(0, current_settings$max_x_CpWB, current_settings$breaks_x_CpWB),
    expand = expansion(mult = c(0.03,0.07))
  ) +
  geom_segment(
    aes(x = 0, xend = CpWB, y = charity_label, yend = charity_label),
    linewidth = 1, show.legend = FALSE
  ) +
  geom_point() +
  theme_hli_wbg() +
  scale_alpha(range = c(0.5, 1), guide = "none") +
  ylab("") +
  xlab("Cost per WELLBY ($)") +
  evaluator_color_scale +
  depth_size_scale +
  annotate(
    "richtext",
    x = current_settings$richtext_x_CpWB,
    y = current_settings$richtext_y_CpWB,
    label = income_annotation_label,
    hjust = 0,
    family = "Avenir",
    fill = NA,
    label.color = NA
  )+
  dotplot_legend_theme +
  theme(
    legend.position = c(0.55, 0.60),  # Move the legend inside the plot area
    legend.box.margin = margin(3, 5, 85, 5)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)  # Set size of color symbols in legend
  )

# Alt-text for the plot
fmt_cpwb          <- function(x) scales::dollar(round(x, 0))
pure_earth_cpwb   <- pull_charity("Pure Earth", CpWB)
taimaka_cpwb      <- pull_charity("Taimaka", CpWB)
friendship_cpwb   <- pull_charity("Friendship Bench", CpWB)
strongminds_cpwb  <- pull_charity("StrongMinds", CpWB)
fbb_cpwb          <- pull_charity("Football Beyond Borders", CpWB)
guide_dogs_cpwb   <- pull_charity("Guide Dogs", CpWB)

title_CpWB <- paste0(
  "Dot plot of ", nrow(living_review_data), " interventions ranked by cost per WELLBY. ",
  "LMIC interventions dominate the cheapest end, led by Pure Earth Ghana (", fmt_cpwb(pure_earth_cpwb), "), ",
  "Taimaka (", fmt_cpwb(taimaka_cpwb), "), ",
  "Friendship Bench (", fmt_cpwb(friendship_cpwb), "), and ",
  "StrongMinds (", fmt_cpwb(strongminds_cpwb), "). ",
  "HIC interventions cluster at the expensive end, with Football Beyond Borders at ",
  fmt_cpwb(fbb_cpwb), " per WELLBY",
  if (current_settings$version == "all" && length(guide_dogs_cpwb) > 0)
    paste0(" and Guide Dogs UK costing over ", fmt_cpwb(guide_dogs_cpwb))
  else "",
  "."
)

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "CpWB"),
  plot = p_CpWB,
  width = 3000,
  height = height_large_graphs,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = title_CpWB
)

#~============================================================================~=
## Combined dot plot (CpWB + WBp1k) ----
#~============================================================================~=

p_double <- make_double(p_CpWB, p_WBp1k)

hli_double_save(
  filename_no_end = file.path(graph_dir, "double"),
  plot = p_double,
  width = 6000,
  height = height_large_graphs,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = paste(title_CpWB, title_WBp1k)
)

#~############################################################################~#
# CpWB graph - HIC charities only ----
#~############################################################################~#

p_CpWB_HIC <- living_review_data %>%
  filter(country_income_simple == "HICs") %>%
  ggplot(aes(y = reorder(charity_label, -CpWB), x = CpWB,
             color = evaluator,
             size = depth_of_analysis_num,
  )) +
  geom_text(
    aes(label = scales::dollar_format()(CpWB)),
    hjust = -0.3, color = "black", size = 4, alpha = 1
  ) +
  coord_cartesian(xlim = c(0,current_settings$max_x_CpWB)) +
  scale_x_continuous(
    labels = scales::dollar,
    breaks = seq(0, current_settings$max_x_CpWB, current_settings$breaks_x_CpWB),
    expand = expansion(mult = c(0.03,0.07))
  ) +
  geom_segment(
    aes(x = 0, xend = CpWB, y = charity_label, yend = charity_label),
    linewidth = 1, show.legend = FALSE
  ) +
  geom_point() +
  theme_hli_wbg() +
  scale_alpha(range = c(0.5, 1), guide = "none") +
  ylab("") +
  xlab("Cost per WELLBY ($) - High-Income Countries") +
  evaluator_color_scale +
  depth_size_scale +
  dotplot_legend_theme +
  theme(
    legend.position = c(0.55, 0.75),
    legend.box.margin = margin(3, 5, 3, 5)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)
  )

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "CpWB_HIC"),
  plot = p_CpWB_HIC,
  width = 3000,
  height = 1500,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T
)

#~############################################################################~#
# Evaluator comparison ----
#~############################################################################~#

#~=======================================================~=
## Preparation ----
#~=======================================================~=

living_review_data_evaluators <- living_review_data %>% group_by(evaluator) %>%
  summarise(
    n_charities = n(),
    mean_CpWB = geom_mean(CpWB, na.rm = T),
    min_CpWB = min(CpWB, na.rm = T),
    max_CpWB = max(CpWB, na.rm = T),
    ratio_CpWB = max_CpWB/min_CpWB,
    mean_WBp1k = geom_mean(WBp1k, na.rm = T),
    min_WBp1k = min(WBp1k, na.rm = T),
    max_WBp1k = max(WBp1k, na.rm = T),
    ratio_WBp1k = max_WBp1k/min_WBp1k
  ) %>%
  arrange(desc(mean_WBp1k))

text_size_evaluators <- 3

#~=======================================================~=
## CpWB ----
#~=======================================================~=

p_evaluators_CpWB <- living_review_data %>%
  ggplot(aes(y = evaluator, x = CpWB,
             color = evaluator)) +
  geom_point(shape = 16, alpha = 0.7) +
  geom_segment(
    data = living_review_data_evaluators,
    aes(y = evaluator, yend = evaluator, x = min_CpWB, xend = max_CpWB, color = evaluator),
    size = 1, alpha = 0.5
  ) +
  geom_point(
    data = living_review_data_evaluators,
    aes(y = evaluator, x = mean_CpWB),
    shape = 13, size = 5, alpha = 1
  ) +
  geom_text(
    data = living_review_data_evaluators,
    aes(y = evaluator, x = mean_CpWB, label = scales::dollar_format()(mean_CpWB)),
    hjust = -0.1, color = "black", size = text_size_evaluators, alpha = 1,
    vjust = -1.5
  ) +
  geom_text(
    data = living_review_data_evaluators,
    aes(y = evaluator, x = mean_CpWB, label = paste0("x", round(ratio_CpWB, 0))),
    hjust = -0.1, color = "black", size = text_size_evaluators, alpha = 1,
    vjust = 2,
  ) +
  scale_x_continuous(
    labels = scales::dollar,
    breaks = seq(0, current_settings$max_x_evaluators_CpWB, current_settings$breaks_x_evaluators_CpWB),
    expand = expansion(mult = c(0.02,0.05))
  ) +
  evaluator_color_scale +
  theme_hli_wbg() +
  ylab("") +
  xlab("Average cost per WELLBY ($)") +
  theme(
    text = element_text(family = "Avenir"),
    legend.position = "none"
  )

# Alt-text for the plot
fmt_cpwb_eval <- function(ev) {
  scales::dollar(round(
    living_review_data_evaluators %>% filter(evaluator == ev) %>% pull(mean_CpWB), 0
  ))
}

title_evaluators_CpWB <- paste0(
  "Bar chart comparing average cost per WELLBY by evaluator. ",
  "The Happier Lives Institute evaluations average ", fmt_cpwb_eval("Happier Lives Institute"),
  ", far cheaper than State of Life (", fmt_cpwb_eval("State of Life"),
  "), Krekel and colleagues (", fmt_cpwb_eval("Krekel and colleagues"),
  "), and Pro Bono Economics (", fmt_cpwb_eval("Pro Bono Economics"),
  "), reflecting the Happier Lives Institute's focus on LMIC interventions."
)

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "evaluators_CpWB"),
  plot = p_evaluators_CpWB,
  width = 8*300,
  height = 3*300,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = title_evaluators_CpWB
)

#~=======================================================~=
## WBp1k ----
#~=======================================================~=

p_evaluators_WBp1k <- living_review_data %>%
  ggplot(aes(y = evaluator, x = WBp1k,
             color = evaluator)) +
  geom_point(shape = 16, alpha = 0.7) +
  geom_segment(
    data = living_review_data_evaluators,
    aes(y = evaluator, yend = evaluator, x = min_WBp1k, xend = max_WBp1k, color = evaluator),
    size = 1, alpha = 0.5
  ) +
  geom_point(
    data = living_review_data_evaluators,
    aes(y = evaluator, x = mean_WBp1k),
    shape = 13, size = 5, alpha = 1
  ) +
  geom_text(
    data = living_review_data_evaluators,
    aes(y = evaluator, x = mean_WBp1k, label = round_c(mean_WBp1k)),
    hjust = -0.1, color = "black", size = text_size_evaluators, alpha = 1,
    vjust = -1.5
  ) +
  geom_text(
    data = living_review_data_evaluators,
    aes(y = evaluator, x = mean_WBp1k, label = paste0("x", round(ratio_WBp1k, 0))),
    hjust = -0.1, color = "black", size = text_size_evaluators, alpha = 1,
    vjust = 2,
  ) +
  scale_x_continuous(
    breaks = seq(0, current_settings$max_x_evaluators_WBp1k, current_settings$breaks_x_evaluators_WBp1k),
    expand = expansion(mult = c(0.02,0.05))
  ) +
  evaluator_color_scale +
  theme_hli_wbg() +
  ylab("") +
  xlab("Average WELLBY created per $1,000 donated") +
  theme(
    text = element_text(family = "Avenir"),
    legend.position = "none"
  )

# Alt-text for the plot
fmt_WBp1k_eval <- function(ev) {
  round_c(living_review_data_evaluators %>% filter(evaluator == ev) %>% pull(mean_WBp1k))
}

title_evaluators_WBp1k <- paste0(
  "Bar chart comparing average WELLBYs created per $1,000 donated by evaluator. ",
  "The Happier Lives Institute evaluations average ", fmt_WBp1k_eval("Happier Lives Institute"),
  ", more WELLBYs created per $1,000 donated than State of Life (", fmt_WBp1k_eval("State of Life"),
  "), Krekel and colleagues (", fmt_WBp1k_eval("Krekel and colleagues"),
  "), and Pro Bono Economics (", fmt_WBp1k_eval("Pro Bono Economics"),
  "), reflecting the Happier Lives Institute's focus on LMIC interventions."
)

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "evaluators_WBp1k"),
  plot = p_evaluators_WBp1k,
  width = 8*300,
  height = 3*300,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = title_evaluators_WBp1k
)

#~=======================================================~=
## Combined evaluators (CpWB + WBp1k) ----
#~=======================================================~=

p_evaluators_double <- make_double(p_evaluators_CpWB, p_evaluators_WBp1k)

hli_double_save(
  filename_no_end = file.path(graph_dir, "evaluators_double"),
  plot = p_evaluators_double,
  width = 16*300,
  height = 3*300,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = paste(title_evaluators_CpWB, title_evaluators_WBp1k)
)

#~############################################################################~#
# Comparison ----
#~############################################################################~#

#~=======================================================~=
## Preparation ----
#~=======================================================~=

# Create comparison data based on version
if(current_settings$version == "living_review") {

  data_comparison <- rbind(
    living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>%
      filter(rank < 6) %>% summarise(
        charity = "Top 5 charities",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>% filter(charity == "Pure Earth (Ghana)") %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = "Most cost-effective charity\nin sample (Pure Earth)"
      ),
    living_review_data %>% filter(charity == "Football Beyond Borders") %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = "Least cost-effective\ncharity in sample\n(Football Beyond Borders)"
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

} else if(current_settings$version %in% c("all", "only_add_typical_acts")) {

  data_comparison <- rbind(
    living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>%
      filter(rank < 6) %>% summarise(
        charity = "Top 5 charities",
        CpWB = geom_mean(CpWB, na.rm = T),
        WBp1k = geom_mean(WBp1k, na.rm = T)
      ),
    living_review_data %>% filter(charity == "Pure Earth (Ghana)") %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = "Most cost-effective charity\nin sample (Pure Earth)"
      ),
    living_review_data %>% filter(charity == "Football Beyond Borders") %>%
      select(charity, CpWB, WBp1k) %>% mutate(
        charity = "Least cost-effective\ncharity in sample\n(Football Beyond Borders)"
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

# Helper: pull a variable for the comparison group matching a (fixed) name pattern
cmp <- function(pattern, var) {
  data_comparison %>% filter(grepl(pattern, charity, fixed = TRUE)) %>% pull({{ var }})
}

arrow_length <- 0.02
text_size <- 3.5
curve_length <- -0.35

#~=======================================================~=
## CpWB ----
#~=======================================================~=

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Living review ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plot based on version
if(current_settings$version == "living_review") {

  endpoint_common_arrow <- 2000

  p_comparison_CpWB <- data_comparison %>%
    ggplot(aes(x = CpWB, y = reorder(charity, -CpWB),
               fill = charity)) +
    geom_col(width = 0.4) +
    geom_text(
      aes(label = scales::dollar_format()(round(CpWB, 0))),
      hjust = -0.1, color = "black", size = 4, alpha = 1
    ) +
    scale_x_continuous(
      breaks = seq(0, 40000, 2500),
      labels = scales::dollar,
      expand = expansion(mult = c(0,0.05)),
      limits = c(0, 12500)
    ) +
    # Adding all the arrows - coordinates adjusted for vertical bars
    # First to Last
    geom_curve(
      y = 5, x = 600, xend = 9000, yend = 1.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 6400, y = 4.5,
      label = paste0("×", round(cmp("Football", CpWB) / cmp("Pure Earth", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # First to HICs
    geom_curve(
      y = 5, x = 600, xend = endpoint_common_arrow, yend = 2.2,
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 2250, y = 4.25,
      label = paste0("×", round(cmp("HICs (UK)", CpWB) / cmp("Pure Earth", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Top 5 to HICs
    geom_curve(
      y = 4, x = 800, xend = endpoint_common_arrow, yend = 2.2,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 1100, y = 4.3,
      label = paste0("×", round(cmp("HICs (UK)", CpWB) / cmp("Top 5", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    #  LMICs to HICs
    geom_curve(
      y = 3, x = 800, xend = endpoint_common_arrow, yend = 2.2,
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 850, y = 3.5,
      label = paste0("×", round(cmp("HICs (UK)", CpWB) / cmp("LMICs", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # General settings
    scale_fill_manual(values = comparison_fill_5) +
    theme_hli_wbg() +
    ylab("") +
    xlab("Average cost per WELLBY ($)") +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none"
    )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### All ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

} else if(current_settings$version %in% c("all", "only_add_typical_acts")) {

  endpoint_common_arrow <- 4500
  
  p_comparison_CpWB <- data_comparison %>%
    ggplot(aes(x = CpWB, y = reorder(charity, -CpWB),
               fill = charity)) +
    geom_col(width = 0.4) +
    geom_text(
      aes(label = scales::dollar_format()(round(CpWB, 0))),
      hjust = -0.1, color = "black", size = 4, alpha = 1
    ) +
    scale_x_continuous(
      breaks = seq(0, 40000, 10000),
      labels = scales::dollar,
      expand = expansion(mult = c(0,0.05)),
      limits = c(0, 40000)
    ) +
    # Adding all the arrows - coordinates adjusted for horizontal bars
    # Best to worst in sample
    geom_curve(
      y = 6, x = 2100, xend = 11500, yend = 2.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 11500, y = 4.5,
      label = paste0("×", round(cmp("Football", CpWB) / cmp("Pure Earth", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Best to BOTECs
    geom_curve(
      y = 6, x = 2100, xend = 32500, yend = 1.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 25750, y = 4.5,
      label = paste0("×", round(cmp("BOTECs", CpWB) / cmp("Pure Earth", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Best to UK charities
    geom_curve(
      y = 6, x = 2100, xend = endpoint_common_arrow, yend = 3.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 7500, y = 4.5,
      label = paste0("×", round(cmp("HICs (UK)", CpWB) / cmp("Pure Earth", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Top 5 to UK charities
    geom_curve(
      y = 5, x = 3000, xend = endpoint_common_arrow, yend = 3.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 3000, y = 4.5,
      label = paste0("×", round(cmp("HICs (UK)", CpWB) / cmp("Top 5", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # LMICs to UK charities
    geom_curve(
      y = 4, x = 3000, xend = endpoint_common_arrow, yend = 3.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 2500, y = 3.6,
      label = paste0("×", round(cmp("HICs (UK)", CpWB) / cmp("LMICs", CpWB), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # General settings
    scale_fill_manual(values = comparison_fill_6) +
    theme_hli_wbg() +
    ylab("") +
    xlab("Average cost per WELLBY ($)") +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none",
      axis.text.y = element_text(size = 10)
    )
}

# Alt-text for the plot
dc_top5  <- cmp("Top 5", CpWB)
dc_hics  <- cmp("HICs (UK)", CpWB)
dc_pe    <- cmp("Pure Earth", CpWB)
dc_fbb   <- cmp("Football", CpWB)
dc_botec <- cmp("BOTECs", CpWB)
fmt_dc   <- function(x) scales::dollar(round(x, 0))

title_comparison_CpWB <- paste0(
  "Bar chart showing cost per WELLBY across charity groups. ",
  "Top 5 LMIC charities (", fmt_dc(dc_top5), ") are ",
  round(dc_hics / dc_top5, 0), " times cheaper than UK charities (", fmt_dc(dc_hics), "). ",
  "Pure Earth (", fmt_dc(dc_pe), ") is ",
  round(dc_fbb / dc_pe, 0), " times more cost-effective than the least cost-effective ",
  "evaluated charity, Football Beyond Borders (", fmt_dc(dc_fbb), ")",
  if (current_settings$version %in% c("all", "only_add_typical_acts") && length(dc_botec) > 0)
    paste0(" and ", round(dc_botec / dc_pe, 0), " times more cost-effective than an average of ",
           "Guide Dogs and helping with homelessness (", fmt_dc(dc_botec), ")")
  else "",
  "."
)

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "comparison_CpWB"),
  plot = p_comparison_CpWB,
  width = 8*300,
  height = current_settings$comparison_height,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = title_comparison_CpWB
)

#~=======================================================~=
## WBp1k ----
#~=======================================================~=

endpoint_common_arrow <- 10

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Living review ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create plot based on version
if(current_settings$version == "living_review") {

  p_comparison_WBp1k <- data_comparison %>%
    ggplot(aes(x = WBp1k, y = reorder(charity, WBp1k),
               fill = charity)) +
    geom_col(width = 0.4) +
    geom_text(
      aes(label = round_c(WBp1k,2)),
      hjust = -0.1, color = "black", size = 4, alpha = 1
    ) +
    coord_cartesian(xlim = c(0,120)) +
    scale_x_continuous(
      breaks = seq(0, 120, 10),
      expand = expansion(mult = c(0,0.06))
    ) +
    # Adding all the arrows - coordinates adjusted for vertical bars
    # First to Last
    geom_curve(
      y = 4.75, x = 110, xend = endpoint_common_arrow, yend = 1,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 65, y = 0.9,
      label = paste0("×", round(cmp("Pure Earth", WBp1k) / cmp("Football", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # First to HICs
    geom_curve(
      y = 4.75, x = 110, xend = endpoint_common_arrow, yend = 2,
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 30, y = 1.25,
      label = paste0("×", round(cmp("Pure Earth", WBp1k) / cmp("HICs (UK)", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Top 5 to HICs
    geom_curve(
      y = 3.75, x = 60, xend = endpoint_common_arrow, yend = 2,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 45, y = 2,
      label = paste0("×", round(cmp("Top 5", WBp1k) / cmp("HICs (UK)", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    #  LMICs to HICs
    geom_curve(
      y = 2.75, x = 40, xend = endpoint_common_arrow, yend = 2,
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 27.5, y = 2.5,
      label = paste0("×", round(cmp("LMICs", WBp1k) / cmp("HICs (UK)", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # General settings
    scale_fill_manual(values = comparison_fill_5) +
    theme_hli_wbg() +
    ylab("") +
    xlab("Average WELLBYs created per $1,000 donated") +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none"
    )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### All ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} else if(current_settings$version %in% c("all", "only_add_typical_acts")) {

  p_comparison_WBp1k <- data_comparison %>%
    ggplot(aes(x = WBp1k, y = reorder(charity, WBp1k),
               fill = charity)) +
    geom_col(width = 0.4) +
    geom_text(
      aes(label = round_c(WBp1k,2)),
      hjust = -0.1, color = "black", size = 4, alpha = 1
    ) +
    coord_cartesian(xlim = c(0,120)) +
    scale_x_continuous(
      breaks = seq(0, 120, 10),
      expand = expansion(mult = c(0,0.06))
    ) +
    # Adding all the arrows - coordinates adjusted for vertical bars
    # First to Last
    geom_curve(
      y = 5.75, x = 110, xend = endpoint_common_arrow, yend = 2,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 20, y = 1.65,
      label = paste0("×", round(cmp("Pure Earth", WBp1k) / cmp("Football", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # First to HICs
    geom_curve(
      y = 5.75, x = 110, xend = endpoint_common_arrow, yend = 3,
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 30, y = 2.4,
      label = paste0("×", round(cmp("Pure Earth", WBp1k) / cmp("HICs (UK)", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Top 5 to HICs
    geom_curve(
      y = 4.75, x = 60, xend = endpoint_common_arrow, yend = 3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 40, y = 3.5,
      label = paste0("×", round(cmp("Top 5", WBp1k) / cmp("HICs (UK)", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    #  LMICs to HICs
    geom_curve(
      y = 3.75, x = 37.5, xend = endpoint_common_arrow, yend = 3,
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 30, y = 3.5,
      label = paste0("×", round(cmp("LMICs", WBp1k) / cmp("HICs (UK)", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # First to BOTECs
    geom_curve(
      y = 5.75, x = 110, xend = 10, yend = 1,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 15, y = 0.8,
      label = paste0("×", round(cmp("Pure Earth", WBp1k) / cmp("BOTECs", WBp1k), 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # General settings
    scale_fill_manual(values = comparison_fill_6) +
    theme_hli_wbg() +
    ylab("") +
    xlab("Average WELLBYs created per $1,000 donated") +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none",
      axis.text.y = element_text(size = 10)
    )
}

# Alt-text for the plot
dc_top5  <- cmp("Top 5", WBp1k)
dc_hics  <- cmp("HICs (UK)", WBp1k)
dc_pe    <- cmp("Pure Earth", WBp1k)
dc_fbb   <- cmp("Football", WBp1k)
dc_botec <- cmp("BOTECs", WBp1k)
fmt_dc   <- function(x) round_c(x, 2)

title_comparison_WBp1k <- paste0(
  "Bar chart showing WELLBYs created per $1,000 donated across charity groups. ",
  "Top 5 LMIC charities (", fmt_dc(dc_top5), ") are ",
  round(dc_top5 / dc_hics, 0), " times more cost-effective than UK charities (", fmt_dc(dc_hics), "). ",
  "Pure Earth (", fmt_dc(dc_pe), ") is ",
  round(dc_pe / dc_fbb, 0), " times more cost-effective than the least cost-effective ",
  "evaluated charity, Football Beyond Borders (", fmt_dc(dc_fbb), ")",
  if (current_settings$version %in% c("all", "only_add_typical_acts") && length(dc_botec) > 0)
    paste0(" and ", round(dc_pe / dc_botec, 0), " times more cost-effective than an average of ",
           "Guide Dogs and helping with homelessness (", fmt_dc(dc_botec), ")")
  else "",
  "."
)

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "comparison_WBp1k"),
  plot = p_comparison_WBp1k,
  width = 8*300,
  height = current_settings$comparison_height,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = title_comparison_WBp1k
)

#~=======================================================~=
## Combined comparison (CpWB + WBp1k) ----
#~=======================================================~=

p_comparison_double <- make_double(p_comparison_CpWB, p_comparison_WBp1k)

hli_double_save(
  filename_no_end = file.path(graph_dir, "comparison_double"),
  plot = p_comparison_double,
  width = 16*300,
  height = current_settings$comparison_height,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = paste(title_comparison_CpWB, title_comparison_WBp1k)
)
