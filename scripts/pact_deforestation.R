
compare_def_rates <- function(project_no,t0,tend,jrc_period_length,all_data){
  
  # filter out reference data if present
  
  all_data <- all_data %>% filter(type!='Reference')
  
  # count number of 1s at project start
  
  t0_index <- grep(paste0('luc_',t0),colnames(all_data))
  
  data_filtered <- all_data[all_data[,t0_index]==1,]
  
  project_1s <- data_filtered %>% filter(type=='Project') %>% 
    nrow()
  
  cf_1s <- data_filtered %>% filter(type=='Counterfactual') %>% 
    nrow()
  
  # identify where there have been changes (1 == change)
  
  if(exists(paste0('luc_',tend),where=data_filtered)==TRUE) {
  
  luc_tend <- data_filtered %>% 
    select(paste0('luc_',tend))
  
  response <- case_when(
    luc_tend==1 ~ 0,
    luc_tend==2 ~ 1,
    luc_tend==3 ~ 1,
    luc_tend==4 ~ 1,
    luc_tend>4 ~ 0)
  
  data_filtered$response <- response
  
  # count up number of pixels where there have been changes for each type
  
  project_changes <- data_filtered %>% filter(type=='Project' & response==1) %>% 
    nrow()
  
  cf_changes <- data_filtered %>% filter(type=='Counterfactual' & response==1) %>% 
    nrow()
  
  # calculate deforestation rates
  
  project_def_rate <- project_changes/project_1s
  cf_def_rate <- cf_changes/cf_1s
  
  } else {
  
    project_def_rate <- NA
    cf_def_rate <- NA
  
  }
  
  df <- data.frame(matrix(nrow=1,ncol=3))
  colnames(df) <- c('project_no','pact_project','pact_cf')
  df[1,1] <- project_no
  df[1,2] <- 100*(project_def_rate/jrc_period_length) # convert to annual percentage
  df[1,3] <- 100*(cf_def_rate/jrc_period_length) # convert to annual percentage
  
  return(df)
  
}







