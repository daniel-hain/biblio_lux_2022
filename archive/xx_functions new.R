
##########################################################################################
############################### Helper function ##########################################
##########################################################################################
# START

### Create network
create_nw_biblio <- function(data, index = 'XX', field = 'CR', sep = ';', projection = '2m', ...) {
  require(tidyverse)
  
  data <- data %>% as.data.frame() 
  
  data <- data[!is.na(data[,index]),]
  
  rownames(data) <- data[,index]
  
  x <- data %>% as.data.frame() %>% bibliometrix::cocMatrix(Field = field, sep = sep)
  
  if(projection == 'm1') { x <- Matrix::tcrossprod(x) }
  if(projection == 'm2') { x <- Matrix::crossprod(x) }  
  
  x <- x %>% 
    igraph::graph_from_incidence_matrix(directed = TRUE, mode = 'out') %>% 
    igraph::simplify() %>%
    igraph::get.edgelist() %>%
    as_tibble() %>%
    distinct(V1, V2) %>%
    rename(from = V1, to = V2)
  
  return(x)
}

### Project 2m edgelist
project_el_to_1m <- function(data, var_m1, var_m2, projection = 'm1', remove_duplicates = TRUE, projection_aggregate = FALSE, report_n = FALSE) {
  
  data <- data %>% select({{var_m1}}, {{var_m2}}) 
  colnames(data) <- c('m1', 'm2')
  
  # save n of the modes
  if(report_n == TRUE){
    n_m1 <- data %>% count(m1, name = 'n_m')
    n_m2 <- data %>% count(m2, name = 'n_m')
  }
  
  # Projection
  if(projection == 'm1'){ data <- data %>% left_join(data, by = 'm2')  %>% rename(from = m1.x, to = m1.y, p2m = m2) %>% select(from, to, p2m)  }
  if(projection == 'm2'){ data <- data %>% left_join(data, by = 'm1')  %>% rename(from = m2.x, to = m2.y, p2m = m1) %>% select(from, to, p2m)  }
  
  data <- data %>% filter(from != to)
  
  # Remove duplicates
  if(remove_duplicates == TRUE){data <- data %>% remove_duplicates_el(var_from = from, var_to = to, var_match = p2m)}
  
  # Agrgegate
  if(projection_aggregate == TRUE){data <- data %>% count(from, to)}
  
  # join n of modes back n of the modes
  if(report_n == TRUE){
    if(projection == 'm1'){data <- data %>% left_join(n_m1, by = c('from' = 'm1')) %>% left_join(n_m1, by = c('to' = 'm1')) %>% rename(n_from = n_m.x, n_to = n_m.y)}
    if(projection == 'm2'){data <- data %>% left_join(n_m2, by = c('from' = 'm2')) %>% left_join(n_m2, by = c('to' = 'm2')) %>% rename(n_from = n_m.x, n_to = n_m.y)}      
  }
  
  return(data)
}


### Rewmove duplicates
remove_duplicates_el <- function(data, var_from, var_to, var_match = NULL){
  
  data <- data %>%
    mutate(V1s = if_else({{ var_from }} < {{ var_to }}, {{ var_from}}, {{ var_to }}),
           V2s = if_else({{ var_from }} < {{ var_to }}, {{ var_to}}, {{var_from }})) 
  
  if(is.null(ensym(var_match))){data <- data %>% distinct(V1s, V2s, .keep_all = TRUE)}
  
  if(!is.null(ensym(var_match))) {data <- data %>% distinct(V1s, V2s, {{ var_match }}, .keep_all = TRUE)}
  
  data <- data %>% select(-V1s, -V2s)
  
  return(data)
}

### Create network (Jaccard Weight)
weight_jaccard <- function(data, var_from, var_to, weight = NULL, return_weights = TRUE){
  
  if(is.null({{weight}})){
    data <- data %>% mutate(w = 1) %>% relocate({{ var_from }}, {{ var_to }}, w)
  } else{
    data <- data %>% relocate({{ var_from }}, {{ var_to }}, {{ weight }})
  }
  
  colnames(data)[1:3] <- c('from', 'to', 'w')
  
  data <- data %>% 
    group_by(from) %>% mutate(w_from = sum(w)) %>% ungroup() %>%
    group_by(to) %>% mutate(w_to = sum(w)) %>% ungroup() %>%
    mutate(w_jac = w / (w_from + w_to - w)) 
  
  if(return_weights == FALSE){data <- data %>% select(-w, -w_from, -w_to)}
  
  return(data)
}  

# END
##########################################################################################
############################### Helper function ##########################################
##########################################################################################