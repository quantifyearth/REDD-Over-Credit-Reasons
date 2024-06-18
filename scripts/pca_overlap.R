library(factoextra)
library(FactoMineR)
library(sf)

pca_overlap <- function(all_data,project_no){
  
  # get minimum size out of all the groups (reference, project, counterfactual)
  
  min_samp_size <- all_data %>%
    group_by(type) %>%
    count() %>% 
    ungroup() %>%
    select(n) %>%
    min()
  
# process data for pca

data <- all_data %>%
  group_by(type) %>%
  sample_n(min_samp_size) %>%
  ungroup()

# saving type and index columns to keep track of points

type <- data$type
index <- data$X

# subsetting data for pca

data_for_pca <- data %>% 
  select(-c(X,type,pair,lat,lng,starts_with('luc'))) %>% 
  data.frame()

# run pca and get ellipse data

pca <- PCA(data_for_pca,graph=F,scale.unit = T,ncp=2)

plot_data <- fviz_pca_ind(pca,col.ind = type,geom='point',alpha.ind=0.5,addEllipses = T)

# get ellipses

ellipses <- ggplot_build(plot_data)$data[[2]]

# converting to polygons

pca_polys_df <- st_as_sf(ellipses[,3:5],coords=c('x','y'))
pca_polys <- st_sf(aggregate(pca_polys_df$geometry, list(pca_polys_df$group),
                             function(g){
                               st_cast(st_combine(g),'POLYGON')
                             }))
colnames(pca_polys)[1] <- 'type'

# renaming groups according to identity

pca_polys[1,1] <- 'Counterfactual'
pca_polys[2,1] <- 'Project'
pca_polys[3,1] <- 'Reference'

# getting pca points

pca_points <- plot_data$data
pca_points$index <- index # allows us to track individual points between datasets

# get polygons

proj <- pca_polys[2,2]
cf <- pca_polys[1,2]
ref <- pca_polys[3,2]

# get intersections

intersection_ref <- st_intersection(proj,ref)
intersection_cf <- st_intersection(proj,cf)

# get areas

area_proj <- st_area(proj)
area_ref <- st_area(ref)
area_cf <- st_area(cf)
area_int_ref <- st_area(intersection_ref)
area_int_cf <- st_area(intersection_cf)

# set up df for storing calculations

overlap_results <- data.frame(matrix(ncol=9,nrow=1))
colnames(overlap_results) <- c('project','ref_overlap','cf_overlap',
                               'ref_distance','cf_distance',
                               '1-ref_distance','1-cf_distance',
                               'avg_ref','avg_cf')

overlap_results[1,1] <- project_no

if(length(area_int_ref)==0) { # account for non-overlap
  
  overlap_results[1,2] <- 0
  
} else {
  
  overlap_results[1,2] <- area_int_ref/area_proj
}

if(length(area_int_cf)==0) {
  
  overlap_results[1,3] <- 0
  
} else {
  
  overlap_results[1,3] <- area_int_cf/area_proj 
  
}

# dissimilarity 

if(length(area_int_ref)==0) { # account for non-overlap
  
  overlap_results[1,4] <- 0
  
} else {
  
  overlap_results[1,4] <- (area_ref-area_int_ref)/area_ref
}

if(length(area_int_cf)==0) {
  
  overlap_results[1,5] <- 0
  
} else {
  
  overlap_results[1,5] <- (area_cf-area_int_cf)/area_cf
  
}

# additional calculations

overlap_results[1,6] <- 1-overlap_results[1,4]
overlap_results[1,7] <- 1-overlap_results[1,5]
overlap_results[1,8] <- (overlap_results[1,2]+overlap_results[1,6])/2
overlap_results[1,9] <- (overlap_results[1,3]+overlap_results[1,7])/2

# calculations of numbers of points



return(overlap_results)

}

