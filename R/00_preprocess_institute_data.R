###########################################################################################
########################### Preamble
###########################################################################################

### Generic preamble
# rm(list=ls())
set.seed(1337)

### Load packages  
library(tidyverse)
library(magrittr)

###########################################################################################
########################### Workflow description
###########################################################################################

# For the whole project, the following workflow is applied (-> we are here)
# 
# 1. get data from institutes and departments on their publications
# --> 2. Run 00_preproccess_institute_data script on the institution
# 3. Upload to Scival by matching DOIs (record nonmatching DOIs)
# 4. Generate standard scientific evaluation based in Scival
#
# 5. Download a.) Bibliography of whole institute from Scopus as CSV, b.) Scival records of departments as CSV
# 6. Filter Institute from Scopus by Department in SciVal by EID
# 7. Run 11_preprocessing_seed to identify the department seeds
# 8. Take seed EIDs and query the 2k most relevant articles to each
# 9. Run 12_preprocessing_all
# 10. Run 91_descriptives on it.

###########################################################################################
########################### Variable definitions
###########################################################################################

# Institute selection
var_inst <- 'LISER'

# Other variables
PY_min = 2016
PY_max = 2021

###########################################################################################
########################### Read data (delivered by institute)
###########################################################################################

# First read in the original list of publications provided by the institute
# This is saved as TSV.

data_org <- read_tsv(paste0('../data/publications_', str_to_lower(var_inst), '.txt'))

colnames(data_org) <-colnames(data_org) %>% str_to_lower() %>% str_replace_all(' ', '_') %>% str_remove_all('[^[:alnum:]_]')

data_org %<>% 
  # Filter timeframe NOTE: Not for now, we filter ex-post, since institute and scopus might have different years
  # filter(year >= PY_min,
  #       year <= PY_max) %>%
  # clean up DOIs
  mutate(doi = dois_digital_object_identifiers %>% 
           str_remove('^.*doi.org/') %>% 
           str_remove('^.*dx\\.') %>% 
           str_remove('^/') %>% 
           str_remove(' ') %>% 
           str_squish() %>%
           str_replace_all('%', '/')) %>%
  # complete missing DOI
  group_by(pure_id) %>%
  arrange(pure_id, doi) %>%
  fill(doi, .direction = 'downup') %>%
  ungroup()

# ! Note: For matching with departments 2 different forkflows for the institutes
# filter institute & departments
if(var_inst == 'LISER'){ data_org %<>% rename(unit = organisations_of_contributors) }
if(var_inst == 'LIH'){ data_org %<>% rename(unit = parent_organisational_units) }

data_org %<>% 
  inner_join(read_csv2('../data/mapping_units.csv') %>% filter(institute == var_inst, !is.na(unit_short)) %>% select(unit_old, unit_short),
             by = c('unit' = 'unit_old')) 


# Missing data
data <- data_org %>% drop_na(doi) 
data_miss <- data_org %>% filter(is.na(doi)) %>% distinct(pure_id, unit_short, .keep_all = TRUE) 

# save
data %>%
  distinct(doi, unit_short) %>%
  arrange(unit_short, doi) %>%
  write_excel_csv2(paste0('../output/pub_doi_', str_to_lower(var_inst), '.csv'))