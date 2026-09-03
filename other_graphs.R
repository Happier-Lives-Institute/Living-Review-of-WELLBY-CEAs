#~############################################################################~#
# Charity comparisons ----
#~############################################################################~#

#~=======================================================~=
## Evaluated charities ----
#~=======================================================~=

p_charity_comparisons <- charity_comparison_data %>%
  arrange(desc(WBp1k)) %>%
  ggplot(aes(y = reorder(charity_label, WBp1k), x = WBp1k,
             color = recommendation,
  )) +
  geom_text(
    aes(label = paste0(" ", round_c(WBp1k, 2))), 
    hjust = -0.4, color = "black", size = 4, alpha = 1
  ) + 
  coord_cartesian(xlim = c(0,120), clip = "off") +
  scale_x_continuous(
    breaks = seq(0, 120, 10),
    expand = expansion(mult = c(0.02,0.06))
  ) +
  geom_segment(
    aes(x = 0, xend = WBp1k, y = charity_label, yend = charity_label),
    linewidth = 1, show.legend = F
  ) + 
  geom_point(aes(size = depth_of_analysis)) +
  theme_hli_wbg() + 
  scale_alpha(range = c(0.5, 1), guide = "none") + 
  ylab("") +
  xlab("WELLBYs created per $1,000 donated") + 
  scale_color_manual(
    name = "Recommendation level",
    values = c(
      "Top Charity" = "#1c5fb8", 
      "Promising Charity" = "#fc9736",
      "Honourable Mention" = "#ffe655",
      "Not Currently Recommended" = "#b4c3cb"
    ),
    limits = c(
      "Top Charity", 
      "Promising Charity", 
      "Honourable Mention", 
      "Not Currently Recommended"
    )
  ) + 
  scale_size_continuous(
    name = "Depth of analysis",
    breaks = c(1, 2, 4), 
    labels = c("Shallow", "Medium", "In-depth"),
    guide = "legend"
  ) + 
  theme(
    axis.text = element_text(size = 11),
    legend.position = c(0.6, 0.4), 
    legend.box.background = element_rect(color = "black"),  
    legend.box.margin = margin(3, 3, 3, 3),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)
  ); p_charity_comparisons

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "charity_comparisons"),
  plot = p_charity_comparisons,
  width = 3000,
  height = max(80*nrow(charity_comparison_data), 1500),
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = paste0(
    "Dot plot of ", nrow(charity_comparison_data), " charities evaluated by HLI, ranked by WELLBYs created per $1,000 donated. ",
    "Pure Earth leads at ", round_c(charity_comparison_data %>% filter(charity == "Pure Earth (Ghana)") %>% pull(WBp1k), 1),
    ", followed by Taimaka (", round_c(charity_comparison_data %>% filter(charity == "Taimaka") %>% pull(WBp1k), 1),
    "), Friendship Bench (", round_c(charity_comparison_data %>% filter(charity == "Friendship Bench") %>% pull(WBp1k), 1),
    "), and StrongMinds (", round_c(charity_comparison_data %>% filter(charity == "StrongMinds") %>% pull(WBp1k), 1),
    "). GiveDirectly, the cash transfer benchmark, scores ", round_c(charity_comparison_data %>% filter(charity == "GiveDirectly") %>% pull(WBp1k), 1), "."
  )
)

#~=======================================================~=
## Clickable charity links ----
#~=======================================================~=
# hli_double_save has already stripped the CDATA and added the alt text, so this
# only adds the <a> wrappers.

cc_svg <- file.path(graph_dir, "charity_comparisons.svg")
svg_string <- readChar(cc_svg, file.info(cc_svg)$size)

for (charity in charity_comparison_data$charity) {
  
  # Match the full <text>...</text> node that contains exactly the charity name
  # Escape special regex characters in the charity name (e.g. parentheses in "Pure Earth (Ghana)")
  charity_escaped <- gsub("([\\(\\)\\[\\]\\{\\}\\.\\*\\+\\?\\^\\$\\|\\\\])", "\\\\\\1", charity, perl = TRUE)
  pattern <- paste0(
    "(<text[^>]*>)(", charity_escaped, ")(</text>)"
  )
  
  # Get the corresponding URL
  url <- charity_comparison_data$url[charity_comparison_data$charity == charity]
  
  # Wrap the entire <text> element in an <a> tag
  replacement <- paste0(
    "<a xlink:href=\"", url, "\" target=\"_blank\">\\1\\2\\3</a>"
  )
  # The 1, 2, 3 refers the to the previous regex in ()
  
  svg_string <- gsub(pattern, replacement, svg_string, perl = TRUE)
}

# Modify the styling of the links
new_styles <- "
    a text {
      fill: blue;
      text-decoration: underline;
      cursor: pointer;
    }
    a text:hover {
      fill: darkblue;
      text-decoration: underline;
    }
"

svg_string <- sub("</style>", paste0(new_styles, "  </style>"), svg_string, fixed = TRUE)
writeChar(svg_string, cc_svg, eos = NULL)

#~=======================================================~=
## Wider figure ----
#~=======================================================~=

charity_comparisons_wide <- living_review_data


p_WBp1k <- charity_comparisons_wide %>%
  arrange(desc(WBp1k)) %>%
  ggplot(aes(y = reorder(charity_label, WBp1k), x = WBp1k,
             color = recommendation,
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
    linewidth = 1, show.legend = FALSE
  ) + 
  geom_point(aes(size = depth_of_analysis_num)) +
  theme_hli_wbg() + 
  scale_alpha(range = c(0.5, 1), guide = "none") + 
  ylab("") +
  xlab("WELLBYs created per $1,000 donated") + 
  scale_color_manual(
    name = "Recommendation level",
    values = c(
      "Top Charity" = "#1c5fb8", 
      "Promising Charity" = "#fc9736",
      "Honourable Mention" = "#ffe655",
      "Not Currently Recommended" = "#b4c3cb"
    ),
    limits = c("Top Charity", "Promising Charity", "Honourable Mention", "Not Currently Recommended")
  ) + 
  scale_size_continuous(
    name = "Depth of analysis",
    breaks = c(1, 2, 4), 
    labels = c("Shallow", "Medium", "In-depth"),
    guide = "legend"
  ) + 
  annotate(
    "richtext",
    x = 55,
    y = 4.5,
    label = paste0(
      "<span style='color:#E69F00;font-size:8pt;'>(LMIC)</span> operates in low- or<br>",
      "middle-income countries<br><br>",
      "<span style='color:#0072B2;font-size:8pt'>(HIC)</span> operates in<br>high-income countries"
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
    legend.position = c(0.45, 0.3), 
    legend.box.background = element_rect(fill = "transparent", color = "black"),
    legend.box.margin = margin(3, 4, 90, 4),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1)  
  ); p_WBp1k

# Save the plot
hli_double_save(
  filename_no_end = file.path(graph_dir, "charity_comparisons_full_WBp1k"),
  plot = p_WBp1k,
  width = 3000, 
  height = max(80*nrow(charity_comparisons_wide), 1500),
  units = "px",
  dpi = 300, 
  set_svg_same_ratio = T,
  svg_title = paste0(
    "Dot plot of ", nrow(charity_comparisons_wide), " charities ranked by WELLBYs created per $1,000 donated. ",
    "Pure Earth leads at ", round_c(charity_comparisons_wide %>% filter(charity == "Pure Earth (Ghana)") %>% pull(WBp1k), 1), 
    ", followed by Taimaka (", round_c(charity_comparisons_wide %>% filter(charity == "Taimaka") %>% pull(WBp1k), 1),
    "), Friendship Bench (", round_c(charity_comparisons_wide %>% filter(charity == "Friendship Bench") %>% pull(WBp1k), 1), 
    "), and StrongMinds (", round_c(charity_comparisons_wide %>% filter(charity == "StrongMinds") %>% pull(WBp1k), 1), 
    "). HIC interventions cluster near zero, with Guide Dogs UK producing just ", round_c(charity_comparisons_wide %>% filter(charity == "Guide Dogs UK") %>% pull(WBp1k), 2), " WELLBYs per $1,000 donated."
  )
)

#~############################################################################~#
# x1000 times ----
#~############################################################################~#

#~=======================================================~=
## Ratios ----
#~=======================================================~=
# data_comparison and cmp() come from graphs.R, which builds them per version.
# A category a version does not contain gives no ratio.

ratio_or_na <- function(x, y) {
  if(length(x) == 0 || length(y) == 0) NA_real_ else round(x / y, 0)
}

#~=======================================================~=
## Bar graph ----
#~=======================================================~=

bar_graph_order <- c(1:7, 9, 8)

bar_graph_label <- c(
  "Average charity",
  "General public guess (Graham et al.)",
  "General public guess (Caviola et al.)",
  "General public guess (HLI/Prolific)",
  "Expert guess (Caviola et al.)",
  "HLI data (Top 5 vs UK charities)",
  "HLI data (Pure Earth vs Football Beyond Borders)",
  "HLI data-informed guess",
  "HLI data (Pure Earth vs\nextending beyond sample)"
)

bar_graph_simple_label <- c(
  "Average charity",
  "Public guess of best charity",
  "Public guess of best charity",
  "Public guess of best charity",
  "Expert guess of best charity",
  "Top 5 vs UK average in data",
  "Top 1 vs least cost-effective in data",
  "Informed estimate",
  "Top 1 vs everyday charitable donations"
)

bar_graph_color <- c(
  "#A6CEE3",
  "#B2DF8A",
  "#FDBF6F",
  "#CAB2D6",
  "#00ACC1",
  "#FFA726",
  "#29B6F6",
  "#66BB6A",
  "#AB47BC"
)

plot_ratio_bar <- function(
    data,
    x_label = "Cost-effectiveness ratio",
    order_col = "ratio",
    y_col = "label",
    x_end_limit = 4500,
    breaks = seq(0, 5000, 1000)
) {
  ggplot(data, aes(y = reorder(.data[[y_col]], -.data[[order_col]]), x = ratio, fill = color)) +
    geom_bar(stat = "identity", width = 0.33) +
    geom_text(aes(label = paste0("x", ratio), hjust = -0.2, size = 3)) +
    scale_fill_identity() +
    labs(y = "", x = x_label) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.1)),
      limits = c(0, x_end_limit),
      breaks = breaks,
    ) +
    theme_hli_wbg() +
    theme(
      text = element_text(family = "Avenir"),
      legend.position = "none",
    )
}

#~=======================================================~=
## Ratio bars ----
#~=======================================================~=

ratio_bar_data <- data.frame(
  order        = bar_graph_order,
  ratio        = c(1, 1.4, 1.5, 3, 100,
                   ratio_or_na(cmp("HICs (UK)", CpWB), cmp("Top 5", CpWB)),
                   ratio_or_na(cmp("Least cost-effective", CpWB), cmp("Most cost-effective", CpWB)),
                   1000,
                   ratio_or_na(cmp("BOTECs", CpWB), cmp("Most cost-effective", CpWB))
  ),
  label        = bar_graph_label,
  simple_label = bar_graph_simple_label,
  color        = bar_graph_color
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Graph (select information and order) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bar_graph_hli_guess <- ratio_bar_data %>%
  filter(label %ni% c(
    "General public guess (Graham et al.)",
    "General public guess (Caviola et al.)"
  )) %>%
  mutate(label = str_wrap(label, width = 30)) %>%
  plot_ratio_bar(
    order_col = "order",
    x_end_limit = 4000
  ); bar_graph_hli_guess

hli_double_save(
  filename_no_end = file.path(graph_dir, "ratio_bar"),
  plot = bar_graph_hli_guess,
  width = 8,
  height = 4,
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = paste0(
    "Bar chart comparing estimates of how much better the best charity is than ",
    "the average. The general public guesses 3 times; experts guess 100 times. ",
    "HLI data shows ",
    ratio_or_na(cmp("HICs (UK)", CpWB), cmp("Top 5", CpWB)),
    " times (top 5 vs UK charities), ",
    ratio_or_na(cmp("Least cost-effective", CpWB), cmp("Most cost-effective", CpWB)),
    " times (", most_cost_effective_charity, " vs ", least_cost_effective_charity, "), and up to ",
    ratio_or_na(cmp("BOTECs", CpWB), cmp("Most cost-effective", CpWB)),
    " times when extending beyond the sample. HLI's overall informed estimate is 1,000 times."
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Graph (average charity and public guess) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bar_graph_public_guess <- ratio_bar_data %>%
  mutate(
    label = recode(
      label,
      "Average charity" = "Average charity",
      "General public guess (HLI/Prolific)" = "General public guess"
    )
  ) %>% filter(label %in% c("Average charity", "General public guess")) %>%
  ggplot(aes(x = label, y = ratio, fill = color)) +
  geom_bar(stat = "identity", width = 0.33) +
  geom_text(aes(label = paste0("x", ratio), hjust = 0.2, vjust = -1, size = 3)) +
  scale_fill_identity() +
  labs(
    x = "",
    y = "Cost-effectiveness ratio"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
  ) +
  theme_hli_wbg() +
  theme(
    text = element_text(family = "Avenir"),
    legend.position = "none",
  ); bar_graph_public_guess

hli_double_save(
  filename_no_end = file.path(graph_dir, "ratio_bar_public_guess"),
  plot = bar_graph_public_guess,
  width = 6,
  height = 3.5,
  dpi = 300,
  set_svg_same_ratio = T,
  svg_title = paste0(
    "Bar chart of what the general public thinks. Asked how much more ",
    "cost-effective the best charity is than an average one, people guess 3 times."
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Charity line ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_charity_line <- living_review_data_in_sample %>%
  mutate(inv_rank = rank(WBp1k, ties.method = "first"))

lowest_WBp1k <- dat_charity_line %>% filter(charity == "Football Beyond Borders") %>% pull(WBp1k)
FBB_rank     <- dat_charity_line %>% filter(charity == "Football Beyond Borders") %>% pull(inv_rank)

SM_WBp1k <- dat_charity_line %>% filter(charity == "StrongMinds") %>% pull(WBp1k)
SM_rank   <- dat_charity_line %>% filter(charity == "StrongMinds") %>% pull(inv_rank)

PE_WBp1k <- dat_charity_line %>% filter(charity == "Pure Earth (Ghana)") %>% pull(WBp1k)
PE_rank   <- dat_charity_line %>% filter(charity == "Pure Earth (Ghana)") %>% pull(inv_rank)

lay_impact    <- lowest_WBp1k*3
expert_impact <- lowest_WBp1k*100

hli_blue <- "#1c5fb8"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Charity line (dark version - simple) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p_charity_line_dark_simple <- dat_charity_line %>%
  ggplot(aes(
    y = WBp1k,
    x = inv_rank
  )) +
  geom_smooth(
    aes(group = 1),
    method = "loess",
    formula = y ~ x,
    span = 0.2,
    se = F,
    colour = "white",
    linewidth = 1.5
  ) +
  scale_shape_manual(
    name = "Country charity operates in",
    values = c("HICs" = 18, "LMICs" = 16)
  ) +
  scale_color_manual(
    name = "Recommendation level",
    values = c(
      "Top Charity"               = "#82b8ff",
      "Promising Charity"         = "#fc9736",
      "Honourable Mention"        = "#ffe655",
      "Not Currently Recommended" = "#c0c0c0"
    ),
    limits = c("Top Charity", "Promising Charity", "Honourable Mention", "Not Currently Recommended")
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, 15),
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.2, 0.35))
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Data: HLI (2026)",
           hjust = 1.1, vjust = -0.5, size = 4, colour = "white") +
  # Annotation: Football Beyond Borders
  annotate("segment", x = FBB_rank, xend = FBB_rank, y = 6, yend = lowest_WBp1k + 0.3,
           colour = "white") +
  annotate(
    "richtext", x = 1, y = 7.5,
    label = "Mentoring in the UK<br><b>Impact x1</b>",
    size = 6, colour = "white", fill = hli_blue
  ) +
  # Annotation: StrongMinds
  annotate("segment", x = SM_rank - 3, xend = SM_rank - 0.3, y = 47, yend = SM_WBp1k,
           colour = "white") +
  annotate(
    "richtext",
    x = SM_rank - 9,
    y = 47,
    label = paste0("Psychotherapy in<br>low-income countries<br><b>Impact ~x350</b>"),
    size = 6, colour = "white", fill = hli_blue
  ) +
  # Annotation: Pure Earth
  annotate("segment", x = PE_rank - 3, xend = PE_rank - 0.3, y = 105, yend = PE_WBp1k,
           colour = "white") +
  annotate(
    "richtext",
    x = PE_rank - 9,
    y = 105,
    label = paste0("Reducing lead exposure<br><b>Impact ~x900</b>"),
    size = 6, colour = "white", fill = hli_blue
  ) +
  # Annotation: Lay vs Expert guess
  annotate("segment", x = PE_rank + 5, xend = PE_rank + 0.3, y = lay_impact + 5, yend = lay_impact,
           colour = "white") +
  annotate(
    "point",
    x = PE_rank,
    y = lay_impact,
    size = 4,
    shape = 21, fill = hli_blue, colour = "white", stroke = 1.5
  ) +
  annotate(
    "richtext",
    x = PE_rank + 11,
    y = lay_impact + 5,
    label = paste0("People think the best charity is<br><b>Impact x3</b>"),
    size = 6, colour = "white", fill = hli_blue
  ) +
  annotate("segment", x = PE_rank + 5, xend = PE_rank + 0.3, y = expert_impact + 5, yend = expert_impact,
           colour = "white") +
  annotate(
    "point",
    x = PE_rank,
    y = expert_impact,
    size = 4,
    shape = 21, fill = hli_blue, colour = "white", stroke = 1.5
  ) +
  annotate(
    "richtext",
    x = PE_rank + 11,
    y = expert_impact + 5,
    label = paste0("Experts think the best charity is<br><b>Impact x100</b>"),
    size = 6, colour = "white", fill = hli_blue
  ) +
  geom_point(aes(color = recommendation, shape = country_income_simple), size = 5) +
  theme_hli_wbg() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.background  = element_rect(fill = hli_blue, colour = NA),
    panel.background = element_rect(fill = hli_blue, colour = NA),
    panel.grid.major.y = element_line(colour = "white", linetype = 1, linewidth = 0.3),
    axis.text  = element_text(size = 17, colour = "white"),
    axis.title = element_text(size = 18, colour = "white"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal",
    legend.background = element_rect(fill = hli_blue, colour = NA),
    legend.key = element_rect(fill = hli_blue, colour = NA),
    legend.title = element_text(size = 15, face = "bold", colour = "white"),
    legend.text  = element_text(size = 14, colour = "white"),
    legend.key.size = unit(1.5, "lines"),
  ) +
  guides(
    color = guide_legend(title.position = "top", nrow = 2),
    shape = guide_legend(title.position = "top", nrow = 1, override.aes = list(colour = "white"))
  ) +
  labs(x = "", y = "WELLBYs created per $1,000 donated"); p_charity_line_dark_simple

# Save dark version
hli_double_save(
  filename_no_end = file.path(graph_dir, "charity_line_wbp1k_dark_simple"),
  plot = p_charity_line_dark_simple,
  width = 2000*1.5,
  height = 2000*1.5,
  units = "px",
  dpi = 300,
  set_svg_same_ratio = T
)

