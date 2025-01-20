
library(dplyr)
library(arrow)
library(stringr)

tidy_data <- function(project_no, base_path){
  
    # extract project no and set up paths
    
    pact_path <- paste0(base_path,'/pact_data_raw/',project_no) # path to pact data
    ref_path <- paste0(base_path,'/reference_raw/',project_no,'.parquet') # path to reference data
    
    # manual correction for 1112
    
    if(project_no == 1112){
      
      ref_path <- paste0(base_path,'/reference_raw/1113.parquet')

    }
    
    # read in pact data
    
    if(dir.exists(pact_path)){
      
      # get filenames and filter for matched points
      
      files_full <- list.files(path=pact_path,
                               pattern='*.parquet',full.names=T,recursive=F)
      files_full <- files_full[!grepl('matchless',files_full)]
      files_short <- list.files(path=pact_path,
                                pattern='*.parquet',full.names=F,recursive=F)
      files_short <- files_short[!grepl('matchless',files_short)]
      
      # initialise dfs
      
      vars <- colnames(read_parquet(files_full[1]))
      paired_data_raw <- data.frame(matrix(ncol=length(vars),nrow=0))
      colnames(paired_data_raw) <- vars
      paired_data_raw$x_pair <- factor()
      
      for(j in 1:length(files_full)){
        
        # read in all parquet files for a given project
        
        f <- data.frame(read_parquet(files_full[j]))
        
        # add identity column so we know which file the pair came from
        
        f$x_pair <- as.factor(c(replicate(nrow(f),files_short[j])))
        
        # append data to bottom of df
        
        paired_data_raw <- bind_rows(paired_data_raw,f)
        
      }
      
      # generate separate datasets for project and counterfactual 
      
      project <- paired_data_raw %>% 
        dplyr::select(starts_with('k'),x_pair) %>% 
        mutate(x_type = 'Project')
      cf <- paired_data_raw %>%
        dplyr::select(starts_with('s'),x_pair) %>% 
        mutate(x_type = 'Counterfactual')
      
      # create project-counterfactual merged dataset
      
      colnames(cf) <- colnames(project)
      merged_df <- bind_rows(project,cf)
      names(merged_df) <- str_sub(names(merged_df),3)
      merged_df <- merged_df %>% select_if(~ !any(is.na(.)))
      
      print(paste('PACT data tidied for project',project_no))
      
      # add reference data if present
      
      if(file.exists(ref_path)){
        
        reference <- read_parquet(ref_path) %>% 
          mutate(type = 'Reference', pair = NA)
        merged_df <- bind_rows(merged_df,reference)
        
        print(paste('Reference data added for project',project_no))
        
      }
      
      # check no negative access values
      
      merged_df <- filter(merged_df,access >= 0)
      
      # write data to csv
      
      write.csv(merged_df, paste0(base_path,'/tidy_data/',project_no,'.csv')) # keep index column for use in PCA overlap analysis (row.names = T)
      
    }
    
    print(paste('Project',project_no,'complete'))
    
}
  





