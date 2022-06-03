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

data_org <- read_tsv('../data/publications_liser.txt')
colnames(data_org) <-colnames(data_org) %>% str_to_lower() %>% str_replace_all(' ', '_') %>% str_remove_all('[^[:alnum:]_]')

data_org %<>% filter(year >= 2016, year <= 2021)

# first cleaning
data_org %<>% 
  mutate(doi = dois_digital_object_identifiers %>% str_remove('^.*doi.org/') %>% str_remove('^.*dx\\.') %>% str_remove(' ') %>% str_replace_all('%', '/')) %>%
  left_join(read_csv2('../data/mapping_unit_liser.csv'), by = c('organisations_of_contributors' = 'unit_old')) %>%
  mutate(unit_short = case_when(
    unit == 'Urban Development & Mobility' ~ "UD",
    unit == 'Living Conditions' ~ "LC",
    unit == 'Labour Market' ~ "LM",
    TRUE ~ "other")
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

data %>% 
  filter(unit_short != 'other') %>%
  distinct(doi, unit_short) %>%
  arrange(unit_short, doi) %>%
  write_excel_csv2('../output/pub_doi_liser.csv')






