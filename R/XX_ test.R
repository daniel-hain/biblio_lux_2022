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



# Load bibliographic data

M <- convert2df(file = '../data/scopus_pub_lih_dii.csv', dbsource = "scopus", format = "csv") %>%
  rownames_to_column('XX') %>%
  mutate(XX = paste(str_extract(XX, pattern = ".*\\d{4}"), str_sub(TI, 1,25)) %>% str_replace_all("[^[:alnum:]]", " ") %>% str_squish() %>% str_replace_all(" ", "_") %>% make.unique(sep='_')) %>%
  mutate(CR_n = CR %>% str_count(';')) %>%
  filter(CR_n >= 2)
rownames(M) <- M %>% pull(XX)

M %>% glimpse()

el_2m <- M %>% create_nw_biblio(index = 'XX', field = 'CR', sep = ';') %>% 
  group_by(from) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  group_by(to) %>%
  filter(n() >= 2) %>%
  ungroup()
 
el_m1 <- el_2m  %>% project_el_to_1m(var_m1 = from, var_m2 = to, projection = 'm1', remove_duplicates = TRUE, projection_aggregate = TRUE, report_n = FALSE)

# JAccard weighting
el_m1 %<>% 
  left_join(M %>% select(XX, CR_n), by = c('from' = 'XX')) %>% 
  left_join(M %>% select(XX, CR_n), by = c('to' = 'XX')) %>%  
  rename(n_from = CR_n.x, n_to = CR_n.y) %>%
  mutate(w_jac = (n / (n_from + n_to - n)) %>% round(3) ) %>%
  select(-n, -n_from, -n_to) %>%
  filter(w_jac >= 0.01)

g_bib <- el_m1 %>% as_tbl_graph(directed = FALSE)

