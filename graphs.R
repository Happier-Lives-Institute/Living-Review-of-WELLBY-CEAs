#~############################################################################~#
# Preparations ----
#~############################################################################~#

# Get the data
source("wrangle.R")

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
      "<span style='color:#0072B2'>(HIC)</span>",   # Blue
      "<span style='color:#E69F00'>(LMIC)</span>"    # Orange
    ),
    charity_label = ifelse(
      !is.na(intervention),
      paste0(
        charity, " ", income_label,
        "<br><span style='font-size:9pt'>[", intervention, "]</span>"
      ),
      paste0(
        charity, " ", income_label
      )
    ),
    WBp1k_label = ifelse(
      WBp1k < 1, paste0(" ", round_c(WBp1k, 2)),
      paste0(" ", round_c(WBp1k, 1))
    )
  )

#~############################################################################~#
# WBp1k graph ----
#~############################################################################~#

p_WBp1k <- living_review_data %>% 
  arrange(desc(WBp1k)) %>%
  ggplot(aes(y = reorder(charity_label, WBp1k), x = WBp1k, 
             color = evaluator, 
             size = depth_of_analysis,
  )) +
  geom_text(
    aes(label = WBp1k_label), 
    hjust = -0.4, color = "black", size = 4, alpha = 1
  ) + 
  coord_cartesian(xlim = c(0,115)) + 
  scale_x_continuous(
    breaks = seq(0, 110, 10),
    expand = expansion(mult = c(0.02,0.06))
  ) +
  geom_segment(
    aes(x = 0, xend = WBp1k, y = charity_label, yend = charity_label),
    size = 1, show.legend = FALSE
  ) +  # Thin lines to y-axis
  geom_point() +
  theme_hli_wbg() + 
  scale_alpha(range = c(0.5, 1), guide = "none") + 
  ylab("") +
  xlab("WELLBYs created per $1,000 donated") + 
  scale_color_manual(
    name = "Evaluator",
    values = evaluator_colours
  ) + 
  scale_size_continuous(
    name = "Depth of analysis",
    breaks = c(1, 2, 4), 
    labels = c("Shallow", "Medium", "In-depth"),
    guide = "legend"
  ) + 
  annotate(
    "richtext",
    x = 66,
    y = 2.3,
    label = paste0(
      "<span style='color:#E69F00;font-size:9pt;'>(LMIC)</span> operates in low- or<br>",
      "middle-income countries<br><br>",
      "<span style='color:#0072B2;font-size:9pt'>(HIC)</span> operates in<br>high-income countries"
    ),
    hjust = 0,
    size = 4,
    family = "Avenir",
    fill = NA,
    label.color = NA
  ) +
  theme(
    text = element_text(family = "Avenir"),
    axis.text.y = ggtext::element_markdown(),
    legend.position = c(0.55, 0.27),  # Move the legend inside the plot area
    legend.box.background = element_rect(fill = "transparent", color = "black"),
    legend.box.margin = margin(3, 4, 85, 4)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)  # Set size of color symbols in legend
  ); p_WBp1k

# Save the plot
ggsave(
  filename = "graphs/WBp1k.png",
  plot = p_WBp1k,
  width = 8.5, height = 9, dpi = 1200
)

#~############################################################################~#
# CpWB graph ----
#~############################################################################~#

p_CpWB <- living_review_data %>% 
  arrange(CpWB) %>%
  ggplot(aes(y = reorder(charity_label, -CpWB), x = CpWB, 
             color = evaluator, 
             size = depth_of_analysis,
  )) +
  geom_text(
    aes(label = paste0(scales::dollar_format()(CpWB))), 
    hjust = -0.4, color = "black", size = 4, alpha = 1
  ) + 
  coord_cartesian(xlim = c(0,12500)) + 
  scale_x_continuous(
    labels = scales::dollar, 
    breaks = seq(0, 12500, 2500),
    expand = expansion(mult = c(0.03,0.05))
  ) +
  geom_segment(
    aes(x = 0, xend = CpWB, y = charity_label, yend = charity_label),
    size = 1, show.legend = FALSE
  ) +  # Thin lines to y-axis
  geom_point() +
  theme_hli_wbg() + 
  scale_alpha(range = c(0.5, 1), guide = "none") + 
  ylab("") +
  xlab("Cost per WELLBY ($)") + 
  scale_color_manual(
    name = "Evaluator",
    values = evaluator_colours
  ) + 
  scale_size_continuous(
    name = "Depth of analysis",
    breaks = c(1, 2, 4), 
    labels = c("Shallow", "Medium", "In-depth"),
    guide = "legend"
  ) + 
  annotate(
    "richtext",
    x = 7100,
    y = 10.2, 
    label = paste0(
      "<span style='color:#E69F00;font-size:9pt;'>(LMIC)</span> operates in low- or<br>",
      "middle-income countries<br><br>",
      "<span style='color:#0072B2;font-size:9pt'>(HIC)</span> operates in<br>high-income countries"
    ),
    hjust = 0,
    size = 4,
    family = "Avenir",
    fill = NA,
    label.color = NA
  )+
  theme(
    text = element_text(family = "Avenir"),
    axis.text.y = ggtext::element_markdown(),
    legend.position = c(0.55, 0.75),  # Move the legend inside the plot area
    legend.box.background = element_rect(fill = "transparent", color = "black"),
    legend.box.margin = margin(3, 4, 85, 4)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)  # Set size of color symbols in legend
  ); p_CpWB

# Save the plot
ggsave(
  filename = "graphs/CpWB.png",
  plot = p_CpWB,
  width = 8.5, height = 9, dpi = 1200
)

#~############################################################################~#
# Evaluator comparison ----
#~############################################################################~#

living_review_data_evaluators <- living_review_data %>% group_by(evaluator) %>%
  summarise(
    n_charities = n(),
    CpWB = mean(CpWB, na.rm = TRUE),
    WBp1k = mean(WBp1k, na.rm = TRUE)
  ) %>%
  arrange(desc(WBp1k))

p_evaluators <- living_review_data_evaluators %>% 
  ggplot(aes(x = reorder(evaluator, -CpWB), y = CpWB, 
             fill = evaluator)) +
  geom_col(width = 0.4) +
  geom_text(
    aes(label = paste0(scales::dollar_format()(CpWB))), 
    hjust = -0.1, color = "black", size = 4, alpha = 1
  ) + 
  scale_y_continuous(
    labels = scales::dollar, 
    breaks = seq(0, 12500, 1000),
    expand = expansion(mult = c(0,0.05)),
    limits = c(0, 7000)
  ) +
  coord_flip() +
  scale_fill_manual(values = evaluator_colours) +
  theme_hli_wbg() +
  xlab("") +
  ylab("Average WELLBYs created per $1,000 donated") +
  theme(
    text = element_text(family = "Avenir"),
    legend.position = "none"
  ); p_evaluators

# Save the plot
ggsave(
  filename = "graphs/evaluators.png",
  plot = p_evaluators,
  width = 8, height = 3, dpi = 1200
)

#~############################################################################~#
# Comparison ----
#~############################################################################~#

living_review_data_comparison <- rbind(
  living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>% 
    filter(rank < 6) %>% summarise(
      charity = "Top 5 charities",
      CpWB = mean(CpWB, na.rm = TRUE),
      WBp1k = mean(WBp1k, na.rm = TRUE)
    ),
  living_review_data %>% filter(charity == "Pure Earth") %>% 
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
      CpWB = mean(CpWB, na.rm = TRUE),
      WBp1k = mean(WBp1k, na.rm = TRUE)
    ),
  living_review_data %>% 
    filter(country_income_simple == "LMICs") %>% summarise(
      charity = "Charities operating in LMICs",
      CpWB = mean(CpWB, na.rm = TRUE),
      WBp1k = mean(WBp1k, na.rm = TRUE)
    )
) %>% arrange(CpWB)

arrow_length <- 0.02  
text_size <- 3.5
curve_length <- -0.35
endpoint_common_arrow <- 3750

p_comparison <- living_review_data_comparison %>% 
  ggplot(aes(x = reorder(charity, -CpWB), y = CpWB, 
             fill = charity)) +
  geom_col(width = 0.4) +
  geom_text(
    aes(label = paste0(scales::dollar_format()(round(CpWB,0)))), 
    hjust = -0.4, color = "black", size = 4, alpha = 1
  ) + 
  scale_y_continuous(
    breaks = seq(0, 40000, 2500),
    labels = scales::dollar,
    expand = expansion(mult = c(0,0.05)),
    limits = c(0, 12500)
  ) +
  coord_flip() +
  # Adding all the arrows
  # First to Last
  geom_curve(
    aes(x = 5, y = 800, yend = 10000, xend = 1.3),
    arrow = arrow(length = unit(arrow_length, "npc")),
    curvature = curve_length,
    colour = "black"
  ) +
  annotate(
    "text",
    y = 7800, x = 4,
    label = paste0("×", round(
      living_review_data_comparison$CpWB[nrow(living_review_data_comparison)] /
                     living_review_data_comparison$CpWB[1]
      , 0)),
    size = text_size,
    fontface = "bold"
  ) +
  # First to HICs
  geom_curve(
    aes(x = 5, y = 800, yend = endpoint_common_arrow, xend = 2.2),
    # arrow = arrow(length = unit(arrow_length, "npc")),
    curvature = curve_length,
    colour = "black"
  ) +
  annotate(
    "text",
    y = 3400, x = 4,
    label = paste0("×", round(
      living_review_data_comparison$CpWB[4] /
        living_review_data_comparison$CpWB[1]
      , 0)),
    size = text_size,
    fontface = "bold"
  ) +
  # Top 5 to HICs
  geom_curve(
    aes(x = 4, y = 1200, yend = endpoint_common_arrow, xend = 2.2),
    arrow = arrow(length = unit(arrow_length, "npc")),
    curvature = curve_length,
    colour = "black"
  ) +
  annotate(
    "text",
    y = 2200, x = 3.4,
    label = paste0("×", round(
      living_review_data_comparison$CpWB[4] /
        living_review_data_comparison$CpWB[2]
      , 0)),
    size = text_size,
    fontface = "bold"
  ) +
  #  LMICs to HICs
  geom_curve(
    aes(x = 3, y = 1200, yend = endpoint_common_arrow, xend = 2.2),
    # arrow = arrow(length = unit(arrow_length, "npc")),
    curvature = curve_length,
    colour = "black"
  ) +
  annotate(
    "text",
    y = 2400, x = 2.8,
    label = paste0("×", round(
      living_review_data_comparison$CpWB[4] /
        living_review_data_comparison$CpWB[3]
      , 0)),
    size = text_size,
    fontface = "bold"
  ) +
  # General settings
  scale_fill_manual(values = c(
    "Least cost-effective\ncharity in sample\n(Football Beyond Borders)" = "#3498DB",
    "Charities operating in HICs (UK)" = "#B39BC8",
    "Top 5 charities" = "#F5B041",
    "Most cost-effective charity\nin sample (Pure Earth)" = "#27AE60",
    "Charities operating in LMICs" = "#D98880"
  )) +
  theme_hli_wbg() +
  xlab("") +
  ylab("Average cost per WELLBY ($)") +
  theme(
    text = element_text(family = "Avenir"),
    legend.position = "none"
  ); p_comparison

# Save the plot
ggsave(
  filename = "graphs/comparison.png",
  plot = p_comparison,
  width = 8, height = 3, dpi = 1200
)
