
library(tidyverse)
library(magrittr)
library(stringr)
library(MatchIt)

# secondary functions

cpc_rename<-function(x, t0){
  x %<>% 
    as.data.frame() %>%
    select(starts_with('cpc'))
  mycolnames<-colnames(x)
  years<-mycolnames %>% str_extract('[:digit:]+') %>% as.numeric()
  suffix<-mycolnames %>% str_extract('_u|_d') %>%
    str_replace('u', '1') %>%
    str_replace('d', '3')
  newnames<-paste0('JRC', t0-years, suffix)
  colnames(x)<-newnames
  return(x)
}

tmfemi_reformat<-function(df, t0){
  df %<>%
    st_as_sf(coords = c("lng","lat")) %>%
    rename_with(~ gsub("luc_", "JRC", .x, fixed = TRUE))
  
  other<-df %>% select(-starts_with('cpc'))
  
  cpcs<-df %>%
    select(starts_with('cpc')) %>%
    cpc_rename(t0 = t0)
  
  df<-cbind(df, cpcs)
  # JRC2002_1 = cpc10_u,
  # JRC2007_1 = cpc5_u,
  # JRC2012_1 = cpc0_u,
  # JRC2002_3 = cpc10_d,
  # JRC2007_3 = cpc5_d,
  # JRC2012_3 = cpc0_d) %>%
  
  if(any(colnames(df) %>% str_detect('ecoregion')))
    df %<>% rename(biome = ecoregion)
  return(df)
}

make_match_formula<-function(prefix,
                             t0,
                             match_years,
                             match_classes, 
                             suffix){
  
  # generate the match variables:
  match_var_grid<-expand.grid(prefix = prefix, 
                              years = t0 + match_years, # the years to match on
                              # match_years should be zero 
                              # or negative
                              classes = match_classes, 
                              suffix = "")
  
  match_vars<-apply(match_var_grid, 1, function(x){
    paste(x['prefix'], x['years'], '_', x['classes'], x['suffix'], sep='')
    # %>% str_replace('(^_|_$)', '')
  }) %>% 
    c("access", "elevation", "slope")
  
  # the match formula
  fmla <- as.formula(paste("treatment ~ ", paste(match_vars, collapse= "+")))
  
  return(fmla)
}

assess_balance<-function(pts, class_prefix, t0, match_years, match_classes){
  fmla<-make_match_formula(prefix = class_prefix,
                           t0 = t0,
                           match_years = match_years,
                           match_classes = match_classes,
                           suffix = '')
  
  matchit(
    fmla,
    method = 'nearest',
    distance = 'mahalanobis',
    ratio = 1,
    order = 'smallest',
    replace = FALSE,
    discard = 'none',
    data = pts %>% 
      as.data.frame %>% 
      mutate(treatment = ifelse(type == "Project", 1, 0))
  )
}

# primary function

class_prefix <- 'JRC'
match_years <- c(0, -5, -10)
match_classes <- c(1,3)

std_mean_diff <- function(all_data,project_no,t0) {
  
  # filter data
  
  project<-all_data %>% 
    filter(type=='Project') %>% 
    tmfemi_reformat(t0=t0)
  
  reference<-all_data %>% 
    filter(type=='Reference') %>% 
    tmfemi_reformat(t0=t0)
  
  cf<-all_data %>% 
    filter(type=='Counterfactual') %>% 
    tmfemi_reformat(t0=t0)
  
  # run assess_balance for reference
  
  pts_ref<-rbind(reference,project)
  output_ref <- assess_balance(pts_ref, class_prefix, t0, match_years, match_classes)
  
  # run assess_balance for cf
  
  pts_cf <- rbind(cf,project)
  output_cf <- assess_balance(pts_cf, class_prefix, t0, match_years, match_classes)
  
  # extracting std mean differences from outputs
  
  df <- data.frame(summary(output_ref)$sum.matched[,3]) %>% 
    rownames_to_column() %>% 
    cbind(summary(output_cf)$sum.matched[,3])
  rownames(df) <- NULL
  colnames(df) <- c('variable','Reference','Counterfactual')
  
  # formatting df for return
  
  df_melt <- melt(df)
  colnames(df_melt) <- c('variable','type','std_mean_diff')
  df_melt$project <- replicate(nrow(df_melt),project_no)
  return(df_melt)
  
}




