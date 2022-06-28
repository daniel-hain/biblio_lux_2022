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
select_dept <- read_csv2('../data/names_inst_dept.csv') #%>% filter(institute %in% c('LISER'), department %in% c('UD'))

###########################################################################################
########################### Create department 
###########################################################################################

# 1. get data from institutes and departments on their publications
# --> 2. Run 00_preproccess_institute_data script on the institution

rm(list=setdiff(ls(), "select_dept"))

select_inst <- select_dept %>% distinct(institute) %>% filter(institute != 'LIST')

for(i in 1:nrow(select_inst)){
  var_inst <- select_inst[i, 'institute']
  source('R/00_preprocess_institute.R')
}


###########################################################################################
########################### Create department 
###########################################################################################

# 6. Filter Institute from Scopus by Department in SciVal by EID
# 7. Run 11_preprocessing_seed to identify the department seeds


rm(list=setdiff(ls(), "select_dept"))

for(i in 1:nrow(select_dept)){
  skip_row = 18
  var_inst <- select_dept[i, 'institute']
  var_dept <- select_dept[i, 'department']
  source('R/11_preprocess_seed.R')
}

 



