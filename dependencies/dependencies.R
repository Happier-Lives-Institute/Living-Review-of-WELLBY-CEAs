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
library(googlesheets4)
library(ggpattern)
library(RColorBrewer)
library(patchwork)
library(ggtext)
library(lubridate)
library(xml2)

# get rid of summarise messages
options(dplyr.summarise.inform = FALSE)

# Load custom functions that we have in our githubs.
# For basic functions
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_basic.R"))
# For reporting functions
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_reporting.R"))
# Custom themes and styles
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_styles.R"))
