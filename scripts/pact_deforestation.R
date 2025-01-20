
calculate_def_rate <- function(project_no,t0,tend,data){
  
  # filter out columns with all NAs
  
  data <- data %>% 
    dplyr::select(where(~ !all(is.na(.))))
  
  # check if dataframe contains both of the columns we're interested in 
  
  required_columns <- paste0("luc_", c(t0, tend))
  
  if(all(required_columns %in% colnames(data))){
    
  # count number of 1s at project start
  
  t0_index <- grep(paste0('luc_',t0),colnames(data))
  
  data_filtered <- data[data[,t0_index]==1,]
  
  no_1s <- data_filtered %>% 
    nrow()
  
  # identify where there have been changes (1 == change)
  
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
  
  changes <- data_filtered %>% filter(response==1) %>% 
    nrow()
  
  # calculate deforestation rates as percentage
  
  def_rate <- 100*(changes/no_1s)
  
  } else {
  
    def_rate <- NA
  
  }
  
  return(def_rate)
  
}







