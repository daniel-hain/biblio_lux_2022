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
# Biblis & NWs
library(bibliometrix)
library(tidygraph)

# own Parameters
source("functions/00_parameters.R")
source("functions/functions_basic.R")

# institute and department
#var_inst <- 'LISER'
#var_dept <- 'UD'

M <-read_rds(paste0('../temp/M_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds'))

###########################################################################################
########################### Collaboration NW
###########################################################################################

pub_inst <- M %>% as_tibble() %>% metaTagExtraction(Field = "AU_UN") %>% 
  select(XX, UT, PY, int_dept, AU_UN) %>% 
  # Add department as own entry in AU_UN
  mutate(AU_UN = ifelse(int_dept == TRUE, paste0(AU_UN, ';', var_inst, ' ', var_dept), AU_UN)) %>%
  # Sepperate AU_UN for 2_m edgelist
  separate_rows(AU_UN, sep = ';') %>%
  # filter
  drop_na(AU_UN) %>%
  filter(!(AU_UN %in% c('', ' ', 'NA', 'NOTREPORTED', 'NOTDECLARED'))) %>%
  # Only 1 link per paper, independent of author number
  distinct(UT, AU_UN, .keep_all = TRUE) 

el_inst <- pub_inst %>% 
  left_join(pub_inst %>% select(UT, AU_UN), by = 'UT') %>% 
  rename(from = AU_UN.x, to = AU_UN.y) %>%
  filter(from != to) %>%
  group_by(from, to) %>%
  summarise(weight = n()) %>%
  ungroup()

# Save it 
pub_inst %>% saveRDS(paste0('../temp/pub_inst_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds'))
el_inst %>% saveRDS(paste0('../temp/el_inst_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds'))

# clean 
# rm(pub_inst, el_inst)

###########################################################################################
########################### Similarity to past & future
###########################################################################################

text_lda_gamma <- readRDS(paste0('../temp/text_LDA_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds')) %>% 
  tidy(matrix = "gamma")

el_sim_topic <- text_lda_gamma %>%
  pairwise_similarity(document, topic, gamma, diag = FALSE, upper = TRUE)

# join with year = uni
el_sim_topic %<>%
  inner_join(M %>% select(XX, PY), by = c('item1' = 'XX')) %>%
  inner_join(M %>% select(XX, PY), by = c('item2' = 'XX')) %>%
  rename(PY_from = PY.x, PY_to = PY.y)

# decide for similarity past or future
el_sim_topic %<>%
  mutate(delta = PY_to - PY_from) %>%
  mutate(sim_type = case_when(
    delta == 0 ~ "present",
    delta >= 1 ~ "future",
    delta <= -1 ~ "past") 
  )

# aggregate on document level
pub_sim <- el_sim_topic %>%
  group_by(item1, sim_type) %>%
  summarise(sim = mean(similarity)) %>%
  pivot_wider(names_from = sim_type, names_prefix = 'sim_', values_from = sim) %>%
  drop_na()

uni_sim <- pub_sim %>%
  inner_join(pub_inst %>% select(XX, AU_UN), by = c('item1' = 'XX')) %>%
  group_by(AU_UN) %>%
  summarise(sim_past = mean(sim_past),
            sim_present = mean(sim_present),
            sim_future = mean(sim_future),
            n = n()) %>%
  mutate(future_trend = sim_future - sim_past)

# Save it 
uni_sim %>% saveRDS(paste0('../temp/uni_sim_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds'))


# clean 
# rm(pub_inst, el_inst)


