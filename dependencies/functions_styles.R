# Combine cowplot and a white background
# The sizes are optional, only applied when given.
theme_hli_wbg <- function(axis_title_size = NULL, axis_text_size = NULL,
                          legend_title_size = NULL, legend_text_size = NULL) {
  my_theme <- theme_cowplot() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  if (!is.null(axis_title_size)) {
    my_theme <- my_theme + theme(axis.title = element_text(size = axis_title_size))
  }
  if (!is.null(axis_text_size)) {
    my_theme <- my_theme + theme(axis.text = element_text(size = axis_text_size))
  }
  if (!is.null(legend_title_size)) {
    my_theme <- my_theme + theme(legend.title = element_text(size = legend_title_size))
  }
  if (!is.null(legend_text_size)) {
    my_theme <- my_theme + theme(legend.text = element_text(size = legend_text_size))
  }

  my_theme
}

# Function to save a plot as both PNG and SVG
hli_double_save <- function(filename_no_end, plot, width, height, dpi,
                            set_svg_same_ratio = FALSE, units = "in",
                            svg_title = NULL, svg_desc = NULL) {

  # --- PNG ---
  ggsave(
    filename = paste0(filename_no_end, ".png"),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    units    = units
  )

  # --- SVG ---
  svg_path <- paste0(filename_no_end, ".svg")

  if(isTRUE(set_svg_same_ratio)) {
    ggsave(
      filename = svg_path,
      plot     = plot,
      width    = width,
      height   = height,
      units     = units
    )
  } else if(is.numeric(set_svg_same_ratio) && length(set_svg_same_ratio) == 2) {
    ggsave(
      filename = svg_path,
      plot     = plot,
      width    = set_svg_same_ratio[1],
      height   = set_svg_same_ratio[2],
      units     = units
    )
  } else {
    ggsave(
      filename = svg_path,
      plot     = plot
    )
  }

  svg_string <- readChar(svg_path, file.info(svg_path)$size)

  # Remove CDATA wrapper (breaks inline SVG in WordPress)
  svg_string <- gsub("<![CDATA[", "", svg_string, fixed = TRUE)
  svg_string <- gsub("]]>",       "", svg_string, fixed = TRUE)

  # Remove XML declaration (breaks inline SVG embedding)
  svg_string <- gsub("^<\\?xml[^\\?]*\\?>\\s*", "", svg_string, perl = TRUE)

  # Make responsive: width=100%, drop fixed height (viewBox preserves aspect ratio)
  svg_string <- gsub("(<svg[^>]*) width='[^']*'",  "\\1 width='100%'", svg_string, perl = TRUE)
  svg_string <- gsub("(<svg[^>]*) height='[^']*'", "\\1",              svg_string, perl = TRUE)

  # --- Accessibility ---
  if (!is.null(svg_title)) {
    title_id <- paste0(basename(filename_no_end), "-title")
    desc_id  <- paste0(basename(filename_no_end), "-desc")

    # Build aria-labelledby value
    labelledby <- title_id

    # Build nodes to inject
    a11y_nodes <- sprintf('<title id="%s">%s</title>', title_id, svg_title)

    if (!is.null(svg_desc)) {
      labelledby  <- paste(title_id, desc_id)
      a11y_nodes  <- paste0(a11y_nodes, sprintf('<desc id="%s">%s</desc>', desc_id, svg_desc))
    }

    # Add role and aria-labelledby to opening <svg> tag
    svg_string <- gsub("(<svg)([^>]*>)",
                       sprintf('\\1 role="img" aria-labelledby="%s"\\2', labelledby),
                       svg_string, perl = TRUE)

    # Inject title (and desc) immediately after the opening <svg ...> tag
    svg_string <- gsub("(<svg[^>]*>)", paste0("\\1", a11y_nodes), svg_string, perl = TRUE)
  }

  writeChar(svg_string, svg_path, eos = NULL)
}

# Wrap the bold runs of an element_markdown axis in links.
# element_markdown gives one <text> per word, so a label is a run of consecutive
# nodes sharing a y; only the <b> charity name is bold.
svg_link_bold_labels <- function(svg_path, charity, url) {

  svg_string <- readChar(svg_path, file.info(svg_path)$size)

  # gridtext renders straight quotes as typographic ones
  tidy_quotes <- function(x) gsub("[‘’]", "'", x)

  keep   <- !is.na(url)
  lookup <- setNames(url[keep], tidy_quotes(charity[keep]))

  # Any bold weight: Avenir's bold is written 900, Montserrat's lighter
  node_pat <- "<text[^>]*font-weight: (bold|[6-9]00)[^>]*>[^<]*</text>"
  loc      <- str_locate_all(svg_string, node_pat)[[1]]
  nodes    <- str_sub(svg_string, loc[, 1], loc[, 2])
  ys       <- str_match(nodes, "y='([^']*)'")[, 2]
  words    <- str_match(nodes, ">([^<]*)</text>")[, 2]
  xs       <- as.numeric(str_match(nodes, "x='([^']*)'")[, 2])
  lens     <- as.numeric(str_match(nodes, "textLength='([^p]*)px'")[, 2])
  sizes    <- as.numeric(str_match(nodes, "font-size: ([0-9.]+)px")[, 2])

  runs <- data.frame(
    start = tapply(loc[, 1], ys, min),
    end   = tapply(loc[, 2], ys, max),
    label = tapply(words, ys, function(w) tidy_quotes(paste(w, collapse = " "))),
    x0    = tapply(xs, ys, min),
    x1    = tapply(xs + lens, ys, max),
    size  = tapply(sizes, ys, max)
  )
  runs$y   <- as.numeric(rownames(runs))
  runs$url <- unname(lookup[runs$label])
  runs <- runs[!is.na(runs$url), ]

  # Splice from the bottom up so earlier positions stay valid
  for (i in order(runs$start, decreasing = TRUE)) {
    # One rule per run: text-decoration would underline each word separately,
    # leaving the spaces bare
    underline <- sprintf(
      "<line class='hli-link-underline' x1='%.2f' y1='%.2f' x2='%.2f' y2='%.2f' stroke-width='%.2f' />",
      runs$x0[i], runs$y[i] + runs$size[i] * 0.13,
      runs$x1[i], runs$y[i] + runs$size[i] * 0.13,
      runs$size[i] * 0.06
    )
    svg_string <- paste0(
      str_sub(svg_string, 1, runs$start[i] - 1),
      "<a xlink:href=\"", runs$url[i], "\" target=\"_blank\">",
      str_sub(svg_string, runs$start[i], runs$end[i]),
      underline,
      "</a>",
      str_sub(svg_string, runs$end[i] + 1)
    )
  }

  # .svglite prefix keeps these to the chart, since an inline <style> applies to the
  # whole web page, and outranks svglite's own '.svglite line' rule
  new_styles <- "
    .svglite a text {
      fill: blue;
      cursor: pointer;
    }
    .svglite line.hli-link-underline {
      stroke: blue;
      stroke-linecap: butt;
      cursor: pointer;
    }
    .svglite a:hover text {
      fill: darkblue;
    }
    .svglite a:hover line.hli-link-underline {
      stroke: darkblue;
    }
"

  svg_string <- sub("</style>", paste0(new_styles, "  </style>"), svg_string, fixed = TRUE)
  writeChar(svg_string, svg_path, eos = NULL)
}