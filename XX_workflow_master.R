###########################################################################################
########################### Workflow description
###########################################################################################

# For the whole project, the following workflow is applied (-> we are here)
# 
# 1. get data from institutes and departments on their publications
# 2. Run 00_preproccess_institute_data script on the institution
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
########################### Preamble
###########################################################################################

### Generic preamble
rm(list=ls())
set.seed(1337)

library(tidyverse)

###########################################################################################
########################### Preamble
###########################################################################################

# Use all or filter for what you need
select_dept <- read_csv2('../data/names_inst_dept.csv') #%>% 
  #filter(institute %in% c('LISER', 'LIST', 'LIH')) %>% 
  #filter(department %in% c('ERIN'))

###########################################################################################
########################### Create department 
###########################################################################################

# 1. get data from institutes and departments on their publications
# --> 2. Run 00_preproccess_institute_data script on the institution

rm(list=setdiff(ls(), "select_dept"))

# Select institute
select_inst <- select_dept %>% distinct(institute) %>% filter(!(institute %in% c('LIST') ))

for(i in 1:nrow(select_inst)){
  var_inst <- select_inst[i, 'institute']
  print(paste0('=======> Starting Processing: ', str_to_lower(var_inst)))
  source('R/00_preprocess_institute.R')
}
rm(select_inst)
###########################################################################################
########################### 1. Select Seed
###########################################################################################

# 6. Filter Institute from Scopus by Department in SciVal by EID
# 7. Run 11_preprocessing_seed to identify the department seeds


rm(list=setdiff(ls(), "select_dept"))

for(i in 1:nrow(select_dept)){
  skip_row = 18
  var_inst <- select_dept[i, 'institute']
  var_dept <- select_dept[i, 'department']
  print(paste0('=======> Starting Processing ',i, '-', nrow(select_dept), ': ', str_to_lower(var_inst), '_', str_to_lower(var_dept)))
  source('R/11_preprocess_seed.R')
  print(paste0('=======> Finished Processing ',i, '-', nrow(select_dept), ': ', str_to_lower(var_inst), '_', str_to_lower(var_dept)))
}

# For printing all the seeds for C&P in Scopus
for(i in 1:nrow(select_dept)){
  var_inst <- select_dept[i, 'institute']
  var_dept <- select_dept[i, 'department']
  print(paste0('=======> Seed Articles ',i, '-', nrow(select_dept), ': ', str_to_lower(var_inst), '_', str_to_lower(var_dept)))
  print(
    read_csv( paste0('output/seed/scopus_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '_seed.csv'), show_col_types = FALSE) %>%
      filter(seed_com == TRUE) %>%
      group_by(com) %>% slice_max(dgr_int, n = 1, with_ties = FALSE) %>% ungroup() %>%
      pull(UT) %>% paste0('EID(', .,')', collapse = ' OR ')  )
  print('==============================')
}



###########################################################################################
########################### Field Mapping
###########################################################################################
 
# 9. Run 12_preprocessing_all
# --> 10. Run 91_descriptives on it.


for(i in 1:nrow(select_dept)){
  skip_row = 18
  var_inst <- select_dept[i, 'institute']
  var_dept <- select_dept[i, 'department']
  print(paste0('=======> Starting Processing ',i, '-', nrow(select_dept), ': ', str_to_lower(var_inst), '_', str_to_lower(var_dept)))
  source('R/12_preprocess_all.R')
  print(paste0('=======> Finished Processing ',i, '-', nrow(select_dept), ': ', str_to_lower(var_inst), '_', str_to_lower(var_dept)))
}

###########################################################################################
########################### Report creation
###########################################################################################

# 9. Run 12_preprocessing_all
# --> 11. Create all reports

##########
### Field mapping general
##########

rm(list=setdiff(ls(), "select_dept"))

for(i in 1:nrow(select_dept)){
  print(paste0('=======> Starting Processing ',i, '-', nrow(select_dept)))
  rmarkdown::render("R/91_descriptives_general.Rmd", params = list(
    institute = select_dept[i, 'institute'],
    department = select_dept[i, 'department']),
    output_file = paste0('../output/field_mapping/field_mapping_general_', str_to_lower(select_dept[i, 'institute']), '_', str_to_lower(select_dept[i, 'department']), '.html'))
  print(paste0('=======> Finished Processing ',i, '-', nrow(select_dept)))
}

##########
### Field mapping bibliometric categorization
##########

rm(list=setdiff(ls(), "select_dept"))

for(i in 1:nrow(select_dept)){
  print(paste0('=======> Starting Processing ',i, '-', nrow(select_dept)))
  rmarkdown::render("R/92_descriptives_mapping.Rmd", params = list(
    institute = select_dept[i, 'institute'],
    department = select_dept[i, 'department']),
    output_file = paste0('../output/field_mapping/field_mapping_', str_to_lower(select_dept[i, 'institute']), '_', str_to_lower(select_dept[i, 'department']), '.html'))
  print(paste0('=======> Finished Processing ',i, '-', nrow(select_dept)))
}




