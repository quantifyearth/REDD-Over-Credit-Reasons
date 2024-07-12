
make_summary_fig <- function(all_data,project_sf,ref_sf,output_path,
                           country,project_no){

# PLOT 1/3 MAP

# get cf points from existing df

cf <- all_data %>% filter(type=='Counterfactual')
cf_points <- data.frame(lon=cf$lng,lat=cf$lat)

# get map data as polygon

world <- fortify(map("worldHires", fill=TRUE, plot=FALSE))

country_map <- world %>% filter(region==country)

map_plot <- ggplot(data=cf_points,aes(x=lon,y=lat))+
  geom_map(data=country_map,map=country_map,aes(x=long,y=lat,map_id=region),
           colour='black',fill='grey93',linewidth=1.2)+
  geom_point(aes(col='Counterfactual'),alpha=0.4,size=0.4)+
  geom_sf(data=ref_sf,inherit.aes=FALSE,aes(fill='Reference'),colour=NA,alpha=0.7)+
  geom_sf(data=project_sf,inherit.aes=FALSE,aes(fill='Project'),colour=NA,alpha=0.9)+
  scale_colour_manual(name='Legend',labels=c('Counterfactual'),values=c('blue'))+
  scale_fill_manual(name='Legend',labels=c('Project','Reference'),values=c('grey25','red'))+
  coord_sf()+
  annotation_scale(text_cex = 1.7)+
  theme_void()+
  theme(legend.title = element_blank(),
        text=element_text(size=14),
        legend.position='none')

# PLOT 2/3 UNIVARIATE COMPARISON

colours <- c('grey30','blue','red')

cont_data <- all_data %>% dplyr::select(type,elevation,slope,access,
                                        starts_with('cpc'))
cont_data[,5:length(cont_data)] <- 100*cont_data[,5:length(cont_data)]
cont_data <- melt(cont_data)

# rename labels

cont_data$variable <- factor(cont_data$variable,levels=c('access','cpc0_u','cpc0_d',
                                                         'slope','cpc5_u','cpc5_d',
                                                         'elevation','cpc10_u','cpc10_d'))

levels(cont_data$variable) <- c('Inaccessibility',
                                'Forest~cover~t[0]',
                                'Deforestation~t[0]',
                                'Slope',
                                'Forest~cover~t[-5]',
                                'Deforestation~t[-5]',
                                'Elevation',
                                'Forest~cover~t[-10]',
                                'Deforestation~t[-10]')

continuous <- ggplot(data=cont_data,mapping=aes(x=value,colour=type))+
  geom_density(adjust=10,size=1)+
  facet_wrap(scales='free',nrow=3,~variable,labeller=label_parsed)+
  ylab('Density')+
  scale_colour_manual(values=colours,labels=c('Project','Counterfactual','Reference',guide='none'))+
  scale_x_continuous(n.breaks=3,guide = guide_axis(check.overlap = TRUE))+
  theme_classic()+
  theme(text=element_text(size=16),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position='none')

# PLOT 3/3 PCA

library(factoextra)
library(FactoMineR)

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

type <- data$type

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

# getting prop variances (for this reason we need to re-run the PCA)

pc1_propvar <- round(pca$eig[1,2],digits=1)
pc2_propvar <- round(pca$eig[2,2],digits=1)

# plot custom plot

pca_plot <- pca_polys %>% 
  mutate(order = c(2,3,1)) %>% 
  arrange(order) %>% 
  ggplot()+
  geom_sf(mapping=aes(colour=type),fill=NA,linewidth=1.5)+
  scale_colour_manual(name='Legend',
                      breaks=c('Project','Reference','Counterfactual'),
                      values=c('grey30','red','blue'))+
  scale_fill_manual(name='Legend',
                    breaks=c('Project','Reference','Counterfactual'),
                    values=c('grey30','red','blue'))+
  xlab(paste0('PC1 - ',pc1_propvar,'%'))+
  ylab(paste0('PC2 - ',pc2_propvar,'%'))+
  theme_classic()+
  theme(text=element_text(size=18),
        legend.title=element_blank(),
        #legend.box.background=element_rect(),
        legend.position = 'none')

summary_plot <- (map_plot | (continuous / pca_plot)) + 
  plot_layout(widths=c(7,7),heights=c(8,7,5))+
  plot_annotation(tag_levels = 'a',title=paste0('Project ',project_no))&
  theme(plot.tag = element_text(size = 20), 
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.margin = margin(t = 30,b=0,r=0,l=0))

ggsave(filename=paste0(output_path,'/',project_no,'_summary_fig.png'),plot=summary_plot,dpi=300,width=14,height=12)

}





