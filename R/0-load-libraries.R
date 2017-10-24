###################
#
# Author: Jakob Speier <jakob.speier+r(at)gmail.com>
# Filename: 0-load-libraries.R
# Last Edited: 2017-10-16
# Purpose: Load/Install all necessary libraries
# 
###################

# General Purpose ----

library(tidyverse)
library(ggthemes)
library(purrr)
library(lubridate)
library(magrittr)

# Data Input ----

# devtools::install_github("krlmlr/here")
library(here)
library(readxl)
library(cellranger)
library(countrycode)
library(haven) # read DTA files

# API wrappers ----
# devtools::install_github("mrpsonglao/data360r")
library(data360r) # Worldbank Trade & Competetiveness Data
library(WDI)  # World Development Indicators

# Modelling ----
library(plm) # Econometrics/Panel Data Models

