#~############################################################################~#
# Dependencies ----
#~############################################################################~#

# Clean
rm(list=ls())

# Set random seed for reproducibility
set.seed(123)

# Libraries
library(cowplot)
library(readr)
library(tidyverse)
library(magrittr)
library(ggpattern)
library(RColorBrewer)
library(patchwork)
library(ggtext)
library(lubridate)
library(flextable)

# get rid of summarise messages
options(dplyr.summarise.inform = FALSE)

# Load custom functions.
# For basic functions
source("dependencies/functions_basic.R")
# For reporting functions
source("dependencies/functions_reporting.R")
# Custom themes and styles
source("dependencies/functions_styles.R")

geom_mean <- function(x, na.rm = TRUE) {
  if (any(x <= 0, na.rm = TRUE)) stop("all values must be positive")
  exp(mean(log(x), na.rm = na.rm))
}
