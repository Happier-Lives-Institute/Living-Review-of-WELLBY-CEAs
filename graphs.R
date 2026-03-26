#~############################################################################~#
# Preparations ----
#~############################################################################~#

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

#~############################################################################~#
# WBp1k graph ----
#~############################################################################~#

p_WBp1k <- living_review_data %>%
  ggplot(aes(y = reorder(charity_label, WBp1k), x = WBp1k, 
             color = evaluator, 
             size = depth_of_analysis,
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
    x = current_settings$richtext_x_WBp1k,
    y = current_settings$richtext_y_WBp1k,
    label = paste0(
      "<span style='color:#E69F00;font-size:9pt;'>(LMIC)</span> operates in low- or<br>",
      "middle-income countries<br><br>",
      "<span style='color:#0072B2;font-size:9pt'>(HIC)</span> operates in<br>high-income countries"
    ),
    hjust = 0,
    # size = 4,
    family = "Avenir",
    fill = NA,
    label.color = NA
  ) +
  theme(
    text = element_text(family = "Avenir"),
    axis.text.y = ggtext::element_markdown(),
    legend.position = c(0.50, 0.45),
    legend.box.background = element_rect(fill = "transparent", color = "black"),
    legend.box.margin = margin(3, 5, 85, 5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)
  ); p_WBp1k

# Save the plot
hli_double_save(
  filename_no_end = paste0("graphs/", current_settings$version, "_WBp1k"),
  plot = p_WBp1k,
  width = 3000,
  height = height_large_graphs,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = TRUE
)

#~############################################################################~#
# CpWB graph ----
#~############################################################################~#

p_CpWB <- living_review_data %>%
  ggplot(aes(y = reorder(charity_label, -CpWB), x = CpWB, 
             color = evaluator, 
             size = depth_of_analysis,
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
    x = current_settings$richtext_x_CpWB,
    y = current_settings$richtext_y_CpWB, 
    label = paste0(
      "<span style='color:#E69F00;font-size:9pt;'>(LMIC)</span> operates in low- or<br>",
      "middle-income countries<br><br>",
      "<span style='color:#0072B2;font-size:9pt'>(HIC)</span> operates in<br>high-income countries"
    ),
    hjust = 0,
    # size = 4,
    family = "Avenir",
    fill = NA,
    label.color = NA
  )+
  theme(
    text = element_text(family = "Avenir"),
    axis.text.y = ggtext::element_markdown(),
    legend.position = c(0.55, 0.60),  # Move the legend inside the plot area
    legend.box.background = element_rect(fill = "transparent", color = "black"),
    legend.box.margin = margin(3, 5, 85, 5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)  # Set size of color symbols in legend
  ); p_CpWB

# Save the plot
hli_double_save(
  filename_no_end = paste0("graphs/", current_settings$version, "_CpWB"),
  plot = p_CpWB,
  width = 3000,
  height = height_large_graphs,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = TRUE
)

#~############################################################################~#
# CpWB graph - HIC charities only ----
#~############################################################################~#

p_CpWB_HIC <- living_review_data %>%
  filter(country_income_simple == "HICs") %>%
  ggplot(aes(y = reorder(charity_label, -CpWB), x = CpWB, 
             color = evaluator, 
             size = depth_of_analysis,
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
  theme(
    text = element_text(family = "Avenir"),
    axis.text.y = ggtext::element_markdown(),
    legend.position = c(0.55, 0.75),
    legend.box.background = element_rect(fill = "transparent", color = "black"),
    legend.box.margin = margin(3, 5, 3, 5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)
  ); p_CpWB_HIC

# Save the plot
hli_double_save(
  filename_no_end = paste0("graphs/", current_settings$version, "_CpWB_HIC"),
  plot = p_CpWB_HIC,
  width = 3000,
  height = 1750,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = TRUE
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
    aes(label = scales::dollar_format()(CpWB)),
    hjust = -0.1, color = "black", size = 4, alpha = 1
  ) + 
  scale_y_continuous(
    labels = scales::dollar, 
    breaks = seq(0, current_settings$max_x_evaluators, current_settings$breaks_x_evaluators),
    expand = expansion(mult = c(0,0.05)),
    limits = c(0, current_settings$max_x_evaluators)
  ) +
  coord_flip() +
  scale_fill_manual(values = evaluator_colours) +
  theme_hli_wbg() +
  xlab("") +
  ylab("Average cost per WELLBY ($)") +
  theme(
    text = element_text(family = "Avenir"),
    legend.position = "none"
  ); p_evaluators

# Save the plot
hli_double_save(
  filename_no_end = paste0("graphs/", current_settings$version, "_evaluators"),
  plot = p_evaluators,
  width = 8*300,
  height = 3*300,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = TRUE
)

#~############################################################################~#
# Comparison ----
#~############################################################################~#

# Create comparison data based on version
if(current_settings$version == "living_review") {
  
  living_review_data_comparison <- rbind(
    living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>% 
      filter(rank < 6) %>% summarise(
        charity = "Top 5 charities",
        CpWB = mean(CpWB, na.rm = TRUE),
        WBp1k = mean(WBp1k, na.rm = TRUE)
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
  
  data_comparison <- living_review_data_comparison
  
} else if(current_settings$version == "all") {
  
  all_data_comparison <- rbind(
    living_review_data %>% mutate(rank = rank(CpWB, ties.method = "first")) %>% 
      filter(rank < 6) %>% summarise(
        charity = "Top 5 charities",
        CpWB = mean(CpWB, na.rm = TRUE),
        WBp1k = mean(WBp1k, na.rm = TRUE)
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
        CpWB = mean(CpWB, na.rm = TRUE),
        WBp1k = mean(WBp1k, na.rm = TRUE)
      ),
    living_review_data %>% 
      filter(country_income_simple == "LMICs") %>% summarise(
        charity = "Charities operating in LMICs",
        CpWB = mean(CpWB, na.rm = TRUE),
        WBp1k = mean(WBp1k, na.rm = TRUE)
      ),
    living_review_data %>%
      filter(publication_status == "BOTEC for WHR chapter") %>% summarise(
        charity = "BOTECs of Guide Dogs UK\nand homelessness\nfor the WHR chapter",
        CpWB = mean(CpWB, na.rm = TRUE),
        WBp1k = mean(WBp1k, na.rm = TRUE)
      )
  ) %>% arrange(CpWB)
  
  data_comparison <- all_data_comparison
}

arrow_length <- 0.02  
text_size <- 3.5
curve_length <- -0.35

# Create plot based on version
if(current_settings$version == "living_review") {
  
  endpoint_common_arrow <- 3500
  
  p_comparison <- data_comparison %>% 
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
      y = 5, x = 600, xend = 9500, yend = 1.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 6500, y = 4.5,
      label = paste0("×", round(
        data_comparison$CpWB[nrow(data_comparison)] /
                       data_comparison$CpWB[1]
        , 0)),
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
      x = 3000, y = 4.25,
      label = paste0("×", round(
        data_comparison$CpWB[4] /
          data_comparison$CpWB[1]
        , 0)),
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
      x = 1800, y = 4.1,
      label = paste0("×", round(
        data_comparison$CpWB[4] /
          data_comparison$CpWB[2]
        , 0)),
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
      x = 1900, y = 3.3,
      label = paste0("×", round(
        data_comparison$CpWB[4] /
          data_comparison$CpWB[3]
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
    ylab("") +
    xlab("Average cost per WELLBY ($)") +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none"
    )
    
} else if(current_settings$version == "all") {
  
  p_comparison <- data_comparison %>% 
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
      x = 12000, y = 4.5,
      label = paste0("×", round(
        data_comparison$CpWB[nrow(data_comparison)-1] /
          data_comparison$CpWB[1]
        , 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Best to BOTECs
    geom_curve(
      y = 6, x = 2100, xend = 35000, yend = 1.2,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 30000, y = 4.5,
      label = paste0("×", round(
        data_comparison$CpWB[nrow(data_comparison)] /
          data_comparison$CpWB[1]
        , 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # Top 5 to UK charities
    geom_curve(
      y = 5, x = 3000, xend = 6000, yend = 3.3,
      arrow = arrow(length = unit(arrow_length, "npc")),
      curvature = curve_length,
      colour = "black"
    ) +
    annotate(
      "text",
      x = 7500, y = 4.5,
      label = paste0("×", round(
        data_comparison$CpWB[4] /
          data_comparison$CpWB[2]
        , 0)),
      size = text_size,
      fontface = "bold"
    ) +
    # General settings
    scale_fill_manual(values = c(
      "Least cost-effective\ncharity in sample\n(Football Beyond Borders)" = "#3498DB",
      "Charities operating in HICs (UK)\nnot counting Guide Dogs UK\nand homelessness BOTECS" = "#B39BC8",
      "Top 5 charities" = "#F5B041",
      "Most cost-effective charity\nin sample (Pure Earth)" = "#27AE60",
      "Charities operating in LMICs" = "#D98880",
      "BOTECs of Guide Dogs UK\nand homelessness\nfor the WHR chapter" = "#F1948A"
    )) +
    theme_hli_wbg() +
    ylab("") +
    xlab("Average cost per WELLBY ($)") +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none",
      axis.text.y = element_text(size = 10)
    )
}

p_comparison

# Save the plot
hli_double_save(
  filename_no_end = paste0("graphs/", current_settings$version, "_comparisons"),
  plot = p_comparison,
  width = 8*300,
  height = current_settings$comparison_height,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = TRUE
)
