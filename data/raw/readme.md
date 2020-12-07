# _setup.r
# Run analysis
# 20201207

library(arm)
library(arsenal)
library(broom)
library(car)
library(conflicted)
library(feather)
library(here)
library(janitor)
library(parsedate)
library(readxl)
library(skimr)
library(tidyverse)

options(width=300)

conflict_prefer('filter', 'dplyr')
conflict_prefer('recode', 'dplyr')
conflict_prefer('select', 'dplyr')
conflict_prefer('parse_date', 'parsedate')

RAW_FILE <- 'DATA_FILE.xlsx'
RAW_PATH <- here('data','raw')
INTERMEDIATES_PATH <- here('data','intermediates')
RECODED_PATH <- here('data','recoded')

source(here('r', '1_import.r'))
message('Import step complete.')
source(here('r', '2_preprocess.r'))
message('\nPreprocess step complete.')
source(here('r', '3_generate_tables.r'))
message('\nGenerated tables.')
source(here('r', '4_fit_models.r'))
message('\nFit models.')

