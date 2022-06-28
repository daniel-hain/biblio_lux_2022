rm(list=ls())
source("functions/functions_basic.R")
source("functions/00_parameters.R")
source("functions/functions_summary.R")

var_inst <- 'LIST'
var_dept <- 'ERIN'

M_bib <- readRDS(paste0('../temp/M_bib_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds')) %>% as_tibble()
C_nw <- readRDS(paste0('../temp/C_nw_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds'))
text_lda_gamma <- readRDS(paste0('../temp/text_LDA_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds'))  %>% tidy(matrix = "gamma") 
el_2m <- readRDS(paste0('../temp/el_2m_', str_to_lower(var_inst), '_', str_to_lower(var_dept), '.rds')) %>% drop_na()
##### TOPICS


com_names_top <- tibble( 
  com = 1:(text_lda_gamma %>% pull(topic) %>% n_distinct()),
  type = 'TP',
  col = com %>% gg_color_select(pal = pal_tp),
  com_name = paste(type, 1:(text_lda_gamma %>% pull(topic) %>% n_distinct())))

text_lda_gamma %<>% left_join(com_names_top %>% select(com, com_name, col), by = c('topic' = 'com'))


##### Knowledge Bases
com_names_cit <- tibble( 
  com = 1:(C_nw %>% pull(com) %>% n_distinct()),
  type = 'KB',
  col = com %>% gg_color_select(pal = pal_kb),
  com_name = paste(type, 1:(C_nw %>% pull(com) %>% n_distinct())))

C_nw %<>% left_join(com_names_cit %>% select(com, com_name, col), by = "com")

##### Research Areas
com_names_bib <- tibble( 
  com = 1:(M_bib %>% pull(com) %>% n_distinct()),
  type = 'RA',
  col = com %>% gg_color_select(pal = pal_ra),
  com_name = paste(type, 1:(M_bib %>% pull(com) %>% n_distinct())))

M_bib %<>% left_join(com_names_bib %>% select(com, com_name, col), by = "com")

# Nodes
nl_3m <- com_names_bib %>%
  bind_rows(com_names_cit) %>%
  bind_rows(com_names_top) %>%
  rename(name = com_name,
         com_nr = com) %>%
  relocate(name)

# Edges
el_2m_kb <- el_2m %>%
  select(-from, -to) %>%
  inner_join(com_names_cit %>% select(com, com_name), by = c('com_cit' = 'com')) %>%
  inner_join(com_names_bib %>% select(com, com_name, col), by = c('com_bib' = 'com')) %>%
  mutate(weight = 1) %>%
  rename(from = com_name.x,
         to = com_name.y) %>% # generic
  select(from, to, weight, col) %>% 
  drop_na() %>% 
  count(from, to, col, wt = weight, name = 'weight') %>%
  filter(percent_rank(weight) >= 0.25) %>%
  weight_jaccard(i = from, j = to, w = weight) %>% 
  select(-weight)

el_2m_topic <- text_lda_gamma %>% select(-topic, -col) %>%
  left_join(M_bib %>% select(XX, com) %>% drop_na(com), by = c('document' = 'XX')) %>%
  inner_join(com_names_bib %>% select(com, com_name, col), by = c('com' = 'com')) %>%
  rename(from = com_name.y,
         to = com_name.x,
         weight = gamma) %>% # generic
  select(from, to, weight, col) %>% 
  drop_na() %>% 
  count(from, to, col, wt = weight, name = 'weight') %>%
  filter(percent_rank(weight) >= 0.25) %>%
  weight_jaccard(i = from, j = to, w = weight) %>% select(-weight)

# graph
g_3m <- el_2m_kb %>% 
  bind_rows(el_2m_topic) %>%
  as_tbl_graph(directed = TRUE) %N>%
  left_join(nl_3m, by = 'name') %>%
  mutate(
    level = case_when(
      type == "KB" ~ 1,
      type == "RA" ~ 2,
      type == "TP" ~ 3),
    coord_y = 0.1,
    coord_x = 0.001 + 1/(max(level)-1) * (level-1)
    )  %N>%
  filter(!node_is_isolated(), !is.na(level))



## Build sankey plot
fig <- plot_ly(type = "sankey", 
               orientation = "h",
               arrangement = "snap",
  node = list(
    label = g_3m %N>% as_tibble() %>% pull(name),
    x = g_3m %N>% as_tibble() %>% pull(coord_x),
    y = g_3m %N>% as_tibble() %>% pull(coord_y),
    color = g_3m %N>% as_tibble() %>% pull(col), 
    pad = 4
  ), 
  link = list(
    source = (g_3m %E>% as_tibble() %>% pull(from)) -1,
    target = (g_3m %E>% as_tibble() %>% pull(to)) -1,
    value =  g_3m %E>% as_tibble() %>% pull(weight_jac),
    color = g_3m %E>% as_tibble() %>% pull(col) %>% col2rgb() %>% as.matrix() %>% t() %>% as_tibble() %>% 
      mutate(col_rgb = paste0('rgba(', red, ',' , green, ',', blue, ',0.75)')) %>%  pull(col_rgb)
    )
) %>% 
  layout(title = "Basic Sankey Diagram",
         margin = list(l = 50, r = 50, b = 100, t = 100, pad = 2)) 

fig


  