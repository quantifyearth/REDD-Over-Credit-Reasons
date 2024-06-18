library(dplyr)

ref_hist_period <- function(reference_data,project_no,hist_period_start,hist_period_end,hist_period_length){

  # filtering to pixels which are forested at start of historical reference period
  
  t0_index <- grep(paste0('luc_',hist_period_start),colnames(reference_data))
  data_filtered <- reference_data[reference_data[,t0_index]==1,]

  # identify pixels which have undergone changes in historical reference period
  
  luc_tend <- data_filtered %>% 
    select(paste0('luc_',hist_period_end))
  
  response <- case_when(
    luc_tend==1 ~ 0,
    luc_tend==2 ~ 1,
    luc_tend==3 ~ 1,
    luc_tend==4 ~ 1,
    luc_tend>4 ~ 0)
  
  data_filtered$response <- response
  
  # identify where there have been changes

  ref_overall_changes <- data_filtered %>% filter(response==1)
  
  # calculate rate
  
  ref <- nrow(ref_overall_changes)/nrow(data_filtered)
  
  # make df
  
  def_hist <- data.frame(matrix(nrow=1,ncol=2))
  colnames(def_hist) <- c('project_no','hist_rate')
  
  def_hist[1,1] <- project_no
  def_hist[1,2] <- ref
  
  # divide all rates by number of years of historical reference period and * 100
  # such that output is % per year
  
  def_hist[,2] <- 100*def_hist[,2]/hist_period_length
  
  return(def_hist)

}
  












