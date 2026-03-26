#~############################################################################~#
# Dependencies ----
#~############################################################################~#

# Clean
rm(list=ls())

# Libraries
library(cowplot)
library(readr)
library(tidyverse)
library(magrittr)
library(meta)
library(metafor)
library(googlesheets4)
library(ggpattern)
library(RColorBrewer)
library(patchwork)
library(ggrepel)
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
