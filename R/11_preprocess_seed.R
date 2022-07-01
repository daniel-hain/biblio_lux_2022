###########################################################################################
########################### Preamble
###########################################################################################

### Generic preamble
# rm(list=ls())
set.seed(1337)

### Load packages  
library(tidyverse)
library(magrittr)

### Extra packages
library(bibliometrix)
library(tidygraph)

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
# 7. -> Run 11_preprocessing_seed to identify the department seeds
# 8. Take seed EIDs and query the 2k most relevant articles to each
# 9. Run 12_preprocessing_all
# 10. Run 91_descriptives on it.

###########################################################################################
########################### Variable definitions
###########################################################################################

source("functions/00_parameters.R")

#var_inst <- 'LIST'
#var_dept <- 'ERIN'

###########################################################################################
########################### Load & preprocessing articles
###########################################################################################

print('Starting Preprocessing of seed articles')

# skip_row = 18
data_scival_dept <- read_csv(paste0('../data/scival_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.csv'), skip = skip_row) 
colnames(data_scival_dept) <- colnames(data_scival_dept) %>% str_to_lower() %>% str_squish() %>% str_replace_all("[ /-]", "_") %>% str_remove_all('[()\\*\\.\\,]')

data_scival_dept %<>% 
  # Nest ll the multiple entry columns
  mutate(across(c(authors, scopus_author_ids, institutions, open_access, scopus_affiliation_ids, scopus_affiliation_names, country_region, all_science_journal_classification_asjc_code, all_science_journal_classification_asjc_field_name, sustainable_development_goals_2021),
                ~ .x %>% str_remove_all(' ') %>% str_split('\\|') )) %>%
  # set the S in EID small to match with Scopus
  mutate(eid = eid %>% str_replace_all('s', 'S'))

# save all
read_csv(paste0('../data/scopus_', str_to_lower(var_inst), '.csv')) %>% 
  semi_join(data_scival_dept %>% mutate(eid = eid %>% str_replace_all('S', 's')), by = c('EID' = 'eid')) %>%
  write_csv( paste0('../data/seeds/scopus_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '_seed_0.csv'))

M <- convert2df(file = paste0('../data/scopus_', str_to_lower(var_inst), '.csv'), dbsource = "scopus", format = "csv") %>%
  # Filter out other departments only.
  inner_join(data_scival_dept %>% select(eid, field_weighted_citation_impact), by = c('UT' = 'eid')) %>%
  #filter by max year
  filter(PY >= PY_min,
         PY <= PY_max)

# create label
M %<>% rownames_to_column('XX') %>% 
  mutate(XX = paste(str_extract(XX, pattern = ".*\\d{4}"), str_sub(TI, 1,25)) %>% str_replace_all("[^[:alnum:]]", " ") %>% str_squish() %>% str_replace_all(" ", "_") %>% make.unique(sep='_')) %>%
  # filter for publications with enough references
  mutate(CR_n = CR %>% str_count(';')) %>%
  filter(CR_n > 5) %>%
  # Filter for the oines with abstract
  filter(AB != '') %>%
  filter(AB %>% str_length() > 25) %>%
  # GEnerate TC_year
  mutate(TC_year = TC / (PY_max + 1 - PY)) 

# Setting rownames
rownames(M) <- M$XX

###########################################################################################
########################### Bibliographic coupling communities
###########################################################################################

g_bib  <- M  %>% biblioNetwork(analysis = "coupling", network = "references", sep = ";", shortlabel =  FALSE) %>% 
  igraph::graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) %>% 
  igraph::simplify() %>%
  as_tbl_graph(directed = FALSE)

# Restrict the network
g_bib <- g_bib %E>% 
  filter(weight >= cutof_edge_bib)  %N>%
  filter(!node_is_isolated()) %N>%
  mutate(dgr = centrality_degree(weights = weight)) %N>% 
  filter(dgr >= cutof_node_bib) %N>%
  # JAccard weighting
  left_join(M %>% select(XX, PY, CR_n, TC_year), by = c("name" = "XX")) %E>% 
  mutate(weight_jac = weight / (.N()$CR_n[from] + .N()$CR_n[to] - weight) ) %E>%
  mutate(weight_jac = if_else(weight_jac > 1, 1, weight_jac) ) %N>%
  mutate(dgr_jac = centrality_degree(weights = weight_jac)) %N>%
  # Community Detection
  mutate(com = group_louvain(weights = weight_jac)) %>%
  morph(to_split, com) %>% 
  mutate(dgr_int = centrality_degree(weights = weight_jac)) %N>%
  unmorph()

g_bib %N>% as_tibble() %>% count(com, sort = TRUE)

# Community size restriction
g_bib <- g_bib %N>%
  # Update degree
  mutate(dgr = centrality_degree(weights = weight),
         dgr_jac = centrality_degree(weights = weight_jac))

# Seed articles
com_n_seed <- 5
com_size_seed <- (g_bib %N>% as_tibble() %>% nrow()) * 0.05

# Merge with main data
seed <- M %>% select(XX, AU, PY, TI, UT) %>% inner_join(g_bib %N>% as_tibble() %>% select(name, dgr, dgr_jac, com, dgr_int), by = c('XX' = 'name')) %>%
  distinct(XX, .keep_all = TRUE) %>%
  group_by(com) %>%
  mutate(n_com = n()) %>%
  slice_max(dgr_int, n = com_n_seed, with_ties = FALSE) %>%
  ungroup() %>%
  select(com, n_com, dgr_int, PY, TI, AU, UT) %>%
  arrange(com, desc(dgr_int)) %>%
  mutate(seed_com = n_com >= com_size_seed)

###########################################################################################
########################### Save & export
###########################################################################################
seed %>% write_csv( paste0('output/seed/scopus_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '_seed.csv'))

# report EIDS for C&P in Scopus advanced search
seed %>% 
  filter(seed_com == TRUE) %>%
  group_by(com) %>%
  slice_max(dgr_int, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  pull(UT) %>%
  paste0('EID(', .,')', collapse = ' OR ') 
