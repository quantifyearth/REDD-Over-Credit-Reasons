
library(tidyverse)
library(magrittr)
library(stringr)
library(MatchIt)

# comment out match vars as appropriate

match_vars <- c(#'cpc10_d',
                'cpc5_d',
                'cpc0_d',
                #'cpc10_u',
                'cpc5_u',
                'cpc0_u',
                'access','slope','elevation')

# secondary function for matching
  
assess_balance<-function(data){
  
  # the match formula
  fmla <- as.formula(paste("treatment ~ ", paste(match_vars, collapse= "+")))
  
  # matching
  matchit(
    fmla,
    method = 'nearest',
    distance = 'mahalanobis',
    ratio = 1,
    order = 'smallest',
    replace = FALSE,
    discard = 'none',
    data = data %>% 
      as.data.frame %>% 
      mutate(treatment = ifelse(type == "Project", 1, 0))
  )
}

# primary function

std_mean_diff <- function(all_data,project_no) {
  
  # filter data to include project and reference only
  
  ref_input <- filter(all_data,type!='Counterfactual')
  
  # do matching
  
  ref_output <- assess_balance(ref_input)
  
  # extracting smd from outputs
  
  ref_results <- data.frame(summary(ref_output)$sum.matched[,3]) %>% 
    rownames_to_column()
  rownames(ref_results) <- NULL
  colnames(ref_results) <- c('variable','smd')
  
  # project and counterfactual points are already matched
  # matching is preserved in the structure of the dataset
  # so we just need to calculate SMD
  
  cf_results <- data.frame(matrix(ncol=2,nrow=0))
  colnames(cf_results) <- c('variable','smd')
  
  for (var in match_vars) {
    
    proj_vec <- all_data %>% 
      filter(type=='Project') %>%
      select(var) %>%
      pull() %>%
      as.vector()
    
    cf_vec <- all_data %>% 
      filter(type=='Counterfactual') %>%
      select(var) %>%
      pull() %>%
      as.vector()
    
    proj_mean <- mean(proj_vec, na.rm = TRUE)
    cf_mean <- mean(cf_vec, na.rm = TRUE)
    
    proj_sd <- sd(proj_vec, na.rm = TRUE)
    cf_sd <- sd(cf_vec, na.rm = TRUE)
    
    pooled_sd <- sqrt((proj_sd^2 + cf_sd^2) / 2)
    smd <- (proj_mean - cf_mean) / pooled_sd
    
    cf_results <- rbind(cf_results, data.frame(variable = var, smd = smd, stringsAsFactors = FALSE))
  }
  
  # Returning the results:
  
  results <- rbind(ref_results,cf_results)
  results$type <- c(rep(times=length(match_vars),x='Reference'),rep(times=length(match_vars),x='Counterfactual'))
  results$project_no <- project_no
  
  return(results)
  
}
