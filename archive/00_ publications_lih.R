############################################################################
# Preamble
############################################################################
### Generic preamble
rm(list=ls())

options(scipen = 5) # To deactivate annoying scientific number notation
set.seed(1337) # To have a seed defined for reproducability

### Load standard packages
library(tidyverse)
library(magrittr)

### Extra packages
library(bibliometrix)
library(tidygraph)

# read data

data_org <- read_tsv('../../data/publications_lih.txt')
colnames(data_org) <-colnames(data_org) %>% str_to_lower() %>% str_replace_all(' ', '_') %>% str_remove_all('[^[:alnum:]_]')
data_org %<>% filter(year >= 2016, year <= 2021)

data %>% count(parent_organisational_units)

# first cleaning
var_dept <- c('Department of Cancer Research', 'Department of Infection and Immunity', 'Department of Precision Health', 'Translational Medicine Operations Hub', 'Transversal Translational Medicine')

data_org %<>% 
  mutate(doi = dois_digital_object_identifiers %>% str_remove('^.*doi.org/') %>% str_remove('^.*dx\\.') %>% str_remove(' ') %>% str_replace_all('%', '/')) %>%
  filter(parent_organisational_units %in% var_dept) %>%
  mutate(unit_short = case_when(
    parent_organisational_units == 'Department of Cancer Research' ~ "DCR",
    parent_organisational_units == 'Department of Infection and Immunity' ~ "DII",
    parent_organisational_units == 'Department of Precision Health' ~ "DPH",
    parent_organisational_units == 'Translational Medicine Operations Hub' ~ "TMOH",
    parent_organisational_units == 'Transversal Translational Medicine' ~ "TMOH")
  )

# complete missing DOI
data_org %<>% 
  group_by(pure_id) %>%
  arrange(pure_id, doi) %>%
  fill(doi, .direction = 'down') %>%
  ungroup()


# Missing data
data <- data_org %>% drop_na(doi) 
data_miss <- data_org %>% filter(is.na(doi)) %>% distinct(pure_id, unit_short, .keep_all = TRUE) 

data %>% filter(unit_short != 'other') %>%
  distinct(doi, unit_short) %>%
  arrange(unit_short, doi) %>%
  write_excel_csv2('../output/pub_doi_lih.csv')






