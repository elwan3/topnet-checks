library(tidyverse)
library(sf)
library(lubridate)
library(cowplot)
library(arrow)
library(feather)
library(tmap)
library(tmaptools)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(data.table)


# flow measurements  ------------------------------------------------------
#gaugings
#continuous flow measurements 

rua_flow_obs <- read_parquet("parquet/rua_flow_obs_2compare.parquet")
str(rua_flow_obs)


# runoff flows from topnet swn ------------------------------------

#flow_t <- read_parquet("sfr_build_outs/flow_m3d.parquet")
runoff_t <- read_parquet("sfr_build_outs/runoff_m3d.parquet")

#convert catno column to rownames
#flow_t1 <- flow_t  %>%  remove_rownames %>% column_to_rownames(var="catno")
runoff_t1 <- runoff_t  %>%  remove_rownames %>% column_to_rownames(var="catno")

#tranpose 
#flow_m3d_topnet <- transpose(flow_t1)
runoff_m3d_topnet <- transpose(runoff_t1)

# get row and colnames in order
#colnames(flow_m3d_topnet) <- rownames(flow_t1)
#rownames(flow_m3d_topnet) <- colnames(flow_t1)


colnames(runoff_m3d_topnet) <- rownames(runoff_t1)
rownames(runoff_m3d_topnet) <- colnames(runoff_t1)

#convert rownames to column names
#ncol_flow <- ncol(flow_m3d_topnet)+1
ncol_runoff <- ncol(runoff_m3d_topnet)+1


runoff_m3d_topnet1 <- runoff_m3d_topnet %>% 
  rownames_to_column(var="date") %>% 
  dplyr::mutate(Date = lubridate::date(date)) %>% 
  dplyr::select(date, Date, everything()) %>% 
  dplyr::select(-date) %>% 
  tidyr::gather("catno", "value", 2:ncol_runoff) %>% 
  dplyr::mutate(value = round(value, 3)) %>% 
  dplyr::mutate(id = "runoff_topnet") %>% 
  dplyr::mutate(value = ifelse(value ==0, NA, value))

runoff_cat_vec <- unique(runoff_m3d_topnet1[,2])

flow_m3d_topnet1 <- read_parquet("sfr_build_outs/flow_m3d_topnet.parquet") 
  
# flow_m3d_topnet1 <- flow_m3d_topnet %>% 
#   rownames_to_column(var="date") %>% 
#   #dplyr::mutate(Date = lubridate::ymd(date)) %>% 
#   dplyr::select(Date, everything()) %>% 
#   dplyr::select(-date) %>% 
#   tidyr::gather("catno", "value", 2:ncol_flow) %>% 
#   dplyr::mutate(id = "flow_topnet") %>% 
#   dplyr::filter(catno %in% runoff_cat_vec)


#write_parquet(flow_m3d_topnet1, "compare/flow_m3d_topnet.parquet")


runoff_percent <- runoff_m3d_topnet1 %>% 
  dplyr::rename(runoff = value) %>% 
  dplyr::select(-id) %>% 
  dplyr::left_join(., flow_m3d_topnet1) %>%
  dplyr::select(-id) %>% 
  dplyr::rename(flow =value) %>% 
  dplyr::mutate(month = lubridate::month(Date, label=TRUE)) %>% 
  dplyr::mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                                ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                       ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter","Spring")))) %>% 
  dplyr::group_by(catno, season) %>% 
  dplyr::mutate(mean_flow = mean(flow, na.rm =T)) %>% 
  dplyr::mutate(mean_runoff= mean(runoff, na.rm =T)) %>% 
  dplyr::mutate(`runoff_topnet/flow_topnet` = round( mean_runoff/mean_flow , 2)) %>% 
  dplyr::mutate(median_flow = median(flow, na.rm =T)) %>% 
  dplyr::mutate(median_runoff= median(runoff, na.rm =T)) %>% 
  #dplyr::mutate(`median(runoff/flow)` = round( median_runoff/median_flow , 2)) %>% 
  ungroup() %>% 
  dplyr::select(catno, season, `runoff_topnet/flow_topnet`#, `median(runoff/flow)`
                ) %>% 
  distinct()


check_zero <- runoff_m3d_topnet1 %>% 
  dplyr::rename(runoff = value) %>% 
  dplyr::select(-id) %>% 
  dplyr::left_join(., flow_m3d_topnet1) %>%
  dplyr::select(-id) %>% 
  dplyr::rename(flow =value) %>% 
  dplyr::mutate(month = lubridate::month(Date, label=TRUE)) %>% 
  dplyr::mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                                ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                       ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter","Spring")))) %>% 
  dplyr::group_by(catno, season) %>% 
  dplyr::mutate(mean_flow = mean(flow, na.rm =T)) %>% 
  dplyr::mutate(mean_runoff= mean(runoff, na.rm =T)) %>% 
  dplyr::mutate(runoff_mean_perc = round( mean_runoff/mean_flow , 2)) %>% 
  dplyr::mutate(median_flow = median(flow, na.rm =T)) %>% 
  dplyr::mutate(median_runoff= median(runoff, na.rm =T)) %>% 
  dplyr::mutate(runoff_median_perc = round( median_runoff/median_flow , 2)) %>% 
  dplyr::filter(catno == 8168618)


#check same no as runoff (done)
flow_cat_vec <- unique(flow_m3d_topnet1[,2])

topnet_runoff_flow <- runoff_m3d_topnet1 %>% 
  dplyr::bind_rows(., flow_m3d_topnet1) %>% 
  dplyr::mutate(id = factor(id, levels = c("runoff_topnet", 
                                           "flow_topnet"))) %>% 
  dplyr::filter(Date > "1989-06-30")

str(topnet_runoff_flow)



# check runoff higher than flow -------------------------------------------

runoff_vs_flow <-  runoff_m3d_topnet1 %>% 
  dplyr::filter(Date > "1989-06-30") %>% 
  dplyr::rename(runoff = value) %>% 
  dplyr::mutate(runoff =ifelse(is.na(runoff), 0, runoff)) %>% 
  dplyr::select(-id) %>% 
  dplyr::left_join(., flow_m3d_topnet1) %>%
  dplyr::select(-id) %>% 
  dplyr::rename(flow =value) %>% 
  dplyr::mutate(diff = runoff - flow) %>% 
  dplyr::mutate(`runoff_topnet/flow_topnet` = round(runoff/flow, 4)) %>% 
  dplyr::mutate(higher_runoff = ifelse(diff > 0, 1, 0))


#segment to check 
check_8170268 <- runoff_vs_flow %>% 
  dplyr::filter(catno  == "8170268")

#write_parquet(runoff_vs_flow, "compare/runoff_vs_flow_topnet.parquet")
save(runoff_vs_flow, file ="compare/runoff_vs_flow_topnet.rdata")

high_runoff_filter <- runoff_vs_flow %>% 
  dplyr::filter(higher_runoff ==1) %>% 
  #dplyr::filter(`runoff/flow` >= 1.20) %>% 
  add_count(catno) %>% 
  droplevels()

catno_higher_runoff <- unique(high_runoff_filter[,2]) 


write_parquet(high_runoff_filter, "compare/runoff_higherthan_topnetflow.parquet")  
  
# shapefiles sfr swn  -----------------------------------------------------
#this data is from sfr build outs
#these are 
#reach_df -> rua_factor_reaches
#site_df -> sw_sites_joined_factor
#geom -> model_sfr_network_rua_{0}
#gages_dict -> gage_segnum.csv

#modelled
geoms <- sf::st_read("sfr_build_outs/model_sfr_network_rua_5.shp", crs = 2193)
plot(geoms)


#niwa_dn3 
reach_df <- sf::st_read("sfr_build_outs/rua_5_reaches.shp", crs = 2193)
plot(reach_df)


site_df <- sf::st_read("sfr_build_outs/sw_sites_joined_5.shp", crs = 2193)
plot(site_df)


#gage_dict <- read.csv("sfr_build_outs/gage_segnum.csv")
gage_dict  <- read.table("sfr_build_outs/gage_segnum.csv", col.names = paste("V",1:131), fill = T, sep = ",")
#dt <- dt[,which(!is.na(dt[1,]))]


vec_gage <- gage_dict[,1] 
vec_gage_no <- NROW(vec_gage)

for(i in 1:vec_gage_no) {
  
  index_gage <- unname(which(vec_gage == vec_gage[i]))
  
  row <- gage_dict[index_gage,] %>% 
    select_if(~ !any(is.na(.)))
  
  row_vec <- row %>% slice(1) %>% unlist() %>% unname()
  
  print(row_vec)
  # do stuff with row
}



# prepare for loop flow measurements vs topnet flow and runoff ------------

#how many sites for each catno 
#how many of these sites are concurrent gaugings vs continuous flow measurements 

site_ids <- c("35", "36", 
              "187", "188",
              "167", "169", "170",
              #"12", 
              "8", "9",
              "17",
              "30",
              "49"#,# "50"
              
)


site_df_catno <- site_df %>% 
  dplyr::mutate(id = ifelse(str_detect(Site, "_gaugings"), "gaugings", "continuous_flow")) %>% 
  add_count(segnum) %>% 
  dplyr::select(Site, SiteID, n, everything()) %>% 
  dplyr::filter(SiteID %in% site_ids)


rua_flow_obs_filter <- rua_flow_obs %>% 
  dplyr::left_join(., site_df_catno1) %>% 
  dplyr::filter(!is.na(segnum)) %>% 
  dplyr::select(Site) %>% 
  distinct() %>% 
  dplyr::mutate(site_flow = Site)

site_df_catno1 <- data.frame(site_df_catno) %>% 
  dplyr::select(-geometry) %>% 
  dplyr::select(Site, segnum) %>% 
  distinct() 

merge_check <- site_df_catno1 %>% 
  dplyr::left_join(., rua_flow_obs_filter)



#merge catno with flow dataset
site_df1 <- data.frame(site_df) %>%
  dplyr::select(-geometry) %>% 
  dplyr::select(Site, segnum) %>% 
  dplyr::rename(catno = segnum)


rua_flow_catno <- rua_flow_obs %>% 
  dplyr::select(Date, flow_m3d, Site, flow_id, group) %>% 
  dplyr::rename(value = flow_m3d) %>% 
  dplyr::left_join(., site_df1) %>% 
  dplyr::mutate(date = lubridate::date(Date)) %>% 
  dplyr::select(-Date) %>% 
  dplyr::rename(Date = date) %>% 
  dplyr::select(Date, everything())








# get topnet cat poly shapefile -------------------------------------------

topnet_cat <- sf::st_read("shp/tukituki_wsdStrah3_9Jun.shp", crs = 2193)

topnet_rua_cat <- topnet_cat %>% 
  dplyr::filter(catno %in% runoff_cat_vec) 




# other shapefiles --------------------------------------------------------

Rua_domain_mod <- sf::st_read("shp/modified_model_bnd.shp",
                              type = 3, crs= 2193, agr="constant")

Rua_domain_new <- sf::st_read("shp/model_boundary_edited.shp",crs= 2193)

rua_streams <- sf::st_read("sfr_build_outs/rua_5_segment_data.shp")



# create a loop to plot topnet flow and runoff for each topnet sub --------

runoff_cat_vec <- unique(runoff_m3d_topnet1[,2])
runoff_cat_no <- NROW(runoff_cat_vec)


for(i in 1:runoff_cat_no){
  
  flow_runoff_catno <- topnet_runoff_flow %>% 
    dplyr::filter(catno == runoff_cat_vec[i])
  
  
  ts_perc_table <- runoff_percent %>% 
    dplyr::filter(catno == runoff_cat_vec[i]) %>% 
    distinct()
  
  
  tm_cat_id <- topnet_rua_cat %>% 
    dplyr::filter(catno %in% runoff_cat_vec[i]) %>% 
    dplyr::select(catno, geometry) %>% 
    distinct()
  
  p_flow_runoff_compare <-
  
  ggplot(flow_runoff_catno, aes(Date, value, col = id))+
    #geom_line(size = 0.4, alpha =0.85)+
    geom_line(alpha =0.65)+
    scale_color_manual(values =c("tomato", "dodgerblue2"))+
    #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
    facet_wrap(~catno)+
    scale_y_log10()+
    theme_bw()+
    labs(x = "",
         y = "m3/d")+
    scale_x_date( date_labels = "%b-%Y")+    
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                                   legend.title = element_blank(),
                                                   legend.position = "top")
  
  cat_bbox <- st_as_sfc(sf::st_bbox(tm_cat_id, crs =2193)) %>% 
    st_buffer(., 6000)
  
  tm_catid <- tm_shape(cat_bbox)+
    tm_polygons(alpha = 0,
                border.alpha = 0,
                legend.show = F)+
    tm_shape(tm_cat_id)+
    tm_polygons(col = "grey60",
                border.alpha = 0.7,
                lwd = 0.5,
                border.col ="black"
                )+
    #tm_shape(sw_sites_pts)+
    #tm_dots(col = "red", size = 0.08, alpha =0.6)+
    #tm_text("Site.Name", col = "black", ymod = 0.5, xmod =0.5, size = 0.4)+
    tm_shape(rua_streams)+
    tm_lines(col = "blue", lwd = 0.2)+
    tm_shape(Rua_domain_new)+
    tm_polygons("Id",
                border.col ="red",
                border.alpha = 0.6,
                alpha = 0.0,
                legend.show = F
    )
  

  
  tm_inset <- 
    tm_shape(Rua_domain_new)+
    tm_polygons("Id",
                border.col ="red",
                border.alpha = 0.6,
                alpha = 0.0,
                legend.show = F
    )+
    tm_shape(topnet_rua_cat)+
    tm_polygons(border.col ="grey",
                border.alpha = 0.4,
                alpha = 0.0, lwd = 0.1)+
    tm_shape(tm_cat_id)+
    tm_polygons(col = "grey40",
                border.alpha = 0.8, 
                border.col ="black",
                lwd = 1)
  
  
  p <- plot_grid(p_flow_runoff_compare, tableGrob(ts_perc_table), ncol =1, 
                 rel_heights = c(3, 1))
  
  
  tm_catid_g <- tmap_grob(tm_catid)
  tm_inset_g <- tmap_grob(tm_inset)
  
  tm_g <- ggdraw()+
    draw_plot(tm_catid_g) +
    draw_plot(tm_inset_g,
              width = 0.25, height = 0.25,
              x = 0.05, y = 0.60)
  
  
  plot_combined <- plot_grid(tm_g, p,  rel_widths = c(1,2))
  
  save_plot(paste0("plots/runoff_vs_flow_topnet/", runoff_cat_vec[i], ".png", sep ="") , plot_combined, base_height = 8, base_aspect_ratio = 1.5)
  
  
}



# create a loop to plot catchments with runoff higher than flow --------


higher_runoff_no <- NROW(catno_higher_runoff)


for(i in 1:higher_runoff_no){
  
  flow_runoff_catno <- topnet_runoff_flow %>% 
    dplyr::filter(catno == catno_higher_runoff[i])
  
  
  ts_perc_table <- runoff_percent %>% 
    dplyr::filter(catno == catno_higher_runoff[i]) %>% 
    distinct()
  
  
  tm_cat_id <- topnet_rua_cat %>% 
    dplyr::filter(catno %in% catno_higher_runoff[i]) %>% 
    dplyr::select(catno, geometry) %>% 
    distinct()
  
  
  p_flow_runoff_compare <-
    
    ggplot(flow_runoff_catno, aes(Date, value, col = id))+
    #geom_line(size = 0.4, alpha =0.85)+
    geom_line(alpha =0.65)+
    scale_color_manual(values =c("tomato", "dodgerblue2"))+
    #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
    facet_wrap(~catno)+
    scale_y_log10()+
    theme_bw()+
    labs(x = "",
         y = "m3/d")+
    scale_x_date( date_labels = "%b-%Y")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.title = element_blank(),
          legend.position = "top")
  
  cat_bbox <- st_as_sfc(sf::st_bbox(tm_cat_id, crs =2193)) %>% 
    st_buffer(., 6000)
  
  tm_catid <- tm_shape(cat_bbox)+
    tm_polygons(alpha = 0,
                border.alpha = 0,
                legend.show = F)+
    tm_shape(tm_cat_id)+
    tm_polygons(col = "grey60",
                border.alpha = 0.7,
                lwd = 0.5,
                border.col ="black"
    )+
    #tm_shape(sw_sites_pts)+
    #tm_dots(col = "red", size = 0.08, alpha =0.6)+
    #tm_text("Site.Name", col = "black", ymod = 0.5, xmod =0.5, size = 0.4)+
    tm_shape(rua_streams)+
    tm_lines(col = "blue", lwd = 0.2)+
    tm_shape(Rua_domain_new)+
    tm_polygons("Id",
                border.col ="red",
                border.alpha = 0.6,
                alpha = 0.0,
                legend.show = F
    )
  
  
  
  tm_inset <- 
    tm_shape(Rua_domain_new)+
    tm_polygons("Id",
                border.col ="red",
                border.alpha = 0.6,
                alpha = 0.0,
                legend.show = F
    )+
    tm_shape(topnet_rua_cat)+
    tm_polygons(border.col ="grey",
                border.alpha = 0.4,
                alpha = 0.0, lwd = 0.1)+
    tm_shape(tm_cat_id)+
    tm_polygons(col = "grey40",
                border.alpha = 0.8, 
                border.col ="black",
                lwd = 1)
  
  
  p <- plot_grid(p_flow_runoff_compare, tableGrob(ts_perc_table), ncol =1, 
                 rel_heights = c(3, 1))
  
  
  tm_catid_g <- tmap_grob(tm_catid)
  tm_inset_g <- tmap_grob(tm_inset)
  
  tm_g <- ggdraw()+
    draw_plot(tm_catid_g) +
    draw_plot(tm_inset_g,
              width = 0.25, height = 0.25,
              x = 0.05, y = 0.60)
  
  
  plot_combined <- plot_grid(tm_g, p,  rel_widths = c(1,2))
  
  save_plot(paste0("plots/runoff_higherthan_topnetflow/", catno_higher_runoff[i], ".png", sep ="") , plot_combined, base_height = 8, base_aspect_ratio = 1.5)
  
  
}




#loop to plot catchments with measured/correlated flow vs runoff --------

#to do 
#add shapefile of the sites to the plots

rua_flow_catno <- rua_flow_obs %>% 
  dplyr::select(Date, flow_m3d, Site, flow_id, group, x, y) %>% 
  dplyr::rename(value = flow_m3d) %>% 
  dplyr::left_join(., site_df1) %>% 
  dplyr::mutate(date = lubridate::date(Date)) %>% 
  dplyr::select(-Date) %>% 
  dplyr::rename(Date = date) %>% 
  dplyr::select(Date, everything())


site_df1 <- site_df %>% 
  dplyr::mutate(Site = as.factor(Site)) %>% 
  dplyr::rename(catno = segnum) %>% 
  dplyr::mutate(catno = as.numeric(catno))

rua_flow_catno1 <- rua_flow_catno %>% 
  dplyr::filter(catno %in% runoff_cat_vec) 

catno_measuredflow <- unique(rua_flow_catno1[, 8])
catno_measuredflow_no <- NROW(catno_measuredflow)



for(i in 1:catno_measuredflow_no){
  
  flow_runoff_catno <- topnet_runoff_flow %>% 
    dplyr::filter(catno == catno_measuredflow[i])
  
  
  measuredflow_catno <- rua_flow_catno1  %>% 
    dplyr::filter(catno == catno_measuredflow[i])
  
  count_measu_catno <- measuredflow_catno %>% 
    dplyr::select(catno, group) %>% distinct() %>% 
    add_count(catno)
  
  
  count_measu <- count_measu_catno[1,3]
  
  
  ts_perc_table <- runoff_percent %>% 
    dplyr::filter(catno == catno_measuredflow[i]) %>% 
    distinct()# %>% 
    #dplyr::select(-`median(runoff/flow)`)

  
  tmap_site <- rua_flow_catno1 %>%
    dplyr::filter(catno %in% catno_measuredflow[i]) %>%
    dplyr::select(catno, Site, x, y) %>%  distinct() %>%
    sf::st_as_sf(coords = c("x", "y"), remove =F, crs =2193)
  
  
  tm_cat_id <- topnet_rua_cat %>% 
    dplyr::filter(catno == catno_measuredflow[i]) %>% 
    dplyr::select(catno, geometry) %>% 
    distinct()
  
  
  if(count_measu > 1) {
  
  measuredflow_continuous <- measuredflow_catno %>% 
    dplyr::filter(group == "continuous_flow")
  
  measuredflow_gauging <- measuredflow_catno %>% 
    dplyr::filter(group == "gauging")
  
  
  
  p_flow_runoff_compare <-
    
    ggplot(flow_runoff_catno, aes(Date, value, col = id))+
    #geom_line(size = 0.4, alpha =0.85)+
    geom_line(alpha =0.65)+
    geom_line(data = measuredflow_continuous, aes(x = Date , y = value, col = Site), alpha = 0.65)+
    geom_point(data =measuredflow_gauging, aes(x = Date , y = value, col = Site), size = 1.2)+
    #scale_color_manual(values =c("tomato", "dodgerblue2"))+
    #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
    facet_wrap(~catno)+
    scale_y_log10()+
    theme_bw()+
    labs(x = "",
         y = "m3/d")+
    scale_x_date( date_labels = "%b-%Y")+
    guides(colour=guide_legend(ncol=3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "top", 
          legend.title = element_blank(),
          legend.text = element_text(size=8))
  

  } else {
    # measuredflow_continuous <- measuredflow_catno %>% 
    #   dplyr::filter(group == "continuous_flow")
    
    measuredflow_gauging <- measuredflow_catno %>% 
      dplyr::filter(group == "gauging")
    
    p_flow_runoff_compare <-
      
      ggplot(flow_runoff_catno, aes(Date, value, col = id))+
      #geom_line(size = 0.4, alpha =0.85)+
      geom_line(alpha =0.65)+
      #geom_line(data = measuredflow_continuous, aes(x = Date , y = value, col = Site))+
      geom_point(data =measuredflow_gauging, aes(x = Date , y = value, col = Site), size = 1.2)+
      #scale_color_manual(values =c("tomato", "dodgerblue2"))+
      #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
      facet_wrap(~catno)+
      scale_y_log10()+
      theme_bw()+
      labs(x = "",
           y = "m3/d")+
      scale_x_date( date_labels = "%b-%Y")+
      guides(colour=guide_legend(ncol=3)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "top", 
            legend.title = element_blank(),
            legend.text = element_text(size=8))
    
  }
  
  
  cat_bbox <- st_as_sfc(sf::st_bbox(tm_cat_id, crs =2193)) %>% 
    st_buffer(., 6000)
  
  tm_catid <- tm_shape(cat_bbox)+
    tm_polygons(alpha = 0,
                border.alpha = 0,
                legend.show = F)+
    tm_shape(tm_cat_id)+
    tm_polygons(col = "grey60",
                border.alpha = 0.7,
                lwd = 0.5,
                border.col ="black"
    )+
    #tm_shape(sw_sites_pts)+
    #tm_dots(col = "red", size = 0.08, alpha =0.6)+
    #tm_text("Site.Name", col = "black", ymod = 0.5, xmod =0.5, size = 0.4)+
    tm_shape(rua_streams)+
    tm_lines(col = "blue", lwd = 0.2)+
    tm_shape(tmap_site)+
    tm_dots(col = "red", size = 0.04, alpha =0.6)+
    tm_text("Site", col = "black", auto.placement = TRUE, size = 0.35)+
    tm_shape(Rua_domain_new)+
    tm_polygons("Id",
                border.col ="red",
                border.alpha = 0.6,
                alpha = 0.0,
                legend.show = F
    )
  
  rm(tmap_site)
  
  tm_inset <- 
    tm_shape(Rua_domain_new)+
    tm_polygons("Id",
                border.col ="red",
                border.alpha = 0.6,
                alpha = 0.0,
                legend.show = F
    )+
    tm_shape(topnet_rua_cat)+
    tm_polygons(border.col ="grey",
                border.alpha = 0.4,
                alpha = 0.0, lwd = 0.1)+
    tm_shape(tm_cat_id)+
    tm_polygons(col = "grey40",
                border.alpha = 0.8, 
                border.col ="black",
                lwd = 1)
  
  
  p <- plot_grid(p_flow_runoff_compare, tableGrob(ts_perc_table), ncol =1, 
                 rel_heights = c(3, 1))
  
  tm_catid_g <- tmap_grob(tm_catid)
  tm_inset_g <- tmap_grob(tm_inset)
  
  tm_g <- ggdraw()+
    draw_plot(tm_catid_g) +
    draw_plot(tm_inset_g,
              width = 0.25, height = 0.25,
              x = 0.05, y = 0.60)
  
  
  plot_combined <- plot_grid(tm_g, p,  rel_widths = c(1,2))
  
  save_plot(paste0("plots/measured_correlated_flow_vs_runoff_topnet/", catno_measuredflow[i], ".png", sep ="") , plot_combined, base_height = 8, base_aspect_ratio = 1.5)
  

}




# compare runoff to measured flow -----------------------------------------

#get dataframe of measured flow with catno 
#merge it with runoff for same catno on the same date 

#calculate the difference and percent 
#get a list of catno that have runoff higher than measured/correlated flows 


measured_flow_vs_runoff_topnet <-  rua_flow_catno1 %>%
  dplyr::mutate(catno = as.character(catno)) %>% 
  dplyr::rename(flow = value) %>%
  dplyr::left_join(., runoff_m3d_topnet1) %>% 
  dplyr::filter(Date > "1989-06-30") %>% 
  dplyr::rename(runoff = value) %>% 
  dplyr::rename(runoff_id = id) %>% 
  dplyr::mutate(runoff =ifelse(is.na(runoff), 0, runoff)) %>% 
  #dplyr::select(-id) %>% 
  #dplyr::left_join(., flow_m3d_topnet1) %>%
  #dplyr::select(-id) %>% 
  #dplyr::rename(flow =value) %>% 
  dplyr::mutate(diff = runoff - flow) %>% 
  dplyr::mutate(`runoff/flow` = round(runoff/flow, 4)) %>% 
  dplyr::mutate(higher_runoff = ifelse(diff > 0, 1, 0))

write_parquet(measured_flow_vs_runoff_topnet, "compare/topnetrunoff_vs_HBRCflows.parquet")


runoff_higherthan_hbrcflows <- measured_flow_vs_runoff_topnet %>% 
  dplyr::filter(higher_runoff ==1) %>% 
  #dplyr::filter(`runoff/flow` >= 1.20) %>% 
  add_count(catno) %>% 
  droplevels()

#get list of sites to filter out most upstream or sites along tribs
measu_site_list <- runoff_higherthan_hbrcflows %>% 
  dplyr::select(Site, catno) %>% 
  distinct()



catno_runoff_higherthan_hbrcflows <- unique(runoff_higherthan_hbrcflows[,8]) 





# higher topnet runoff compared to HBRC measured/correlated flows ---------
higher_runoff_id <- measured_flow_vs_runoff_topnet %>% 
  dplyr::filter(!Site %in% c("Mangamate Stream D/S Confl",
                             "Parikaka Stream at Mill Road")) %>% 
  dplyr::select(Date, catno, Site, higher_runoff) %>% 
  dplyr::filter(higher_runoff == 1) %>% 
  dplyr::mutate(catno = as.numeric(catno))



rua_runoff_higher_2hbrcflows <- rua_flow_catno1 %>% 
  dplyr::left_join(., higher_runoff_id) %>% 
  #dplyr::filter(higher_runoff == 1)
  dplyr::group_by(catno, Site) %>% 
  dplyr::filter(any(higher_runoff == 1)) %>% 
  ungroup()


rua_runoff_higher_2hbrcflows <- data.frame(rua_runoff_higher_2hbrcflows)

save(rua_runoff_higher_2hbrcflows, file ="compare/topnetrunoff_higherthan_HBRCflows.rdata")


catno_runoff_higherthan_hbrcflows <- unique(rua_runoff_higher_2hbrcflows[,8])  
catno_runoff_higherthan_hbrcflows_no <- NROW(catno_runoff_higherthan_hbrcflows)

for(i in 1:catno_runoff_higherthan_hbrcflows_no){
  
  flow_runoff_catno <- topnet_runoff_flow %>% 
    dplyr::filter(catno == catno_runoff_higherthan_hbrcflows[i])
  
  
  measuredflow_catno <- rua_runoff_higher_2hbrcflows  %>% 
    dplyr::filter(catno == catno_runoff_higherthan_hbrcflows[i])
  
  count_measu_catno <- measuredflow_catno %>% 
    dplyr::select(catno, group) %>% distinct() %>% 
    add_count(catno)
  
  
  count_measu <- count_measu_catno[1,3]
  
  
  ts_perc_table <- runoff_percent %>% 
    dplyr::filter(catno == catno_runoff_higherthan_hbrcflows[i]) %>% 
    distinct()# %>% 
  #dplyr::select(-`median(runoff/flow)`)
  
  
  tmap_site <- rua_runoff_higher_2hbrcflows %>%
    dplyr::filter(catno %in% catno_runoff_higherthan_hbrcflows[i]) %>%
    dplyr::select(catno, Site, x, y) %>%  distinct() %>%
    sf::st_as_sf(coords = c("x", "y"), remove =F, crs =2193)
  
  
  tm_cat_id <- topnet_rua_cat %>% 
    dplyr::filter(catno == catno_runoff_higherthan_hbrcflows[i]) %>% 
    dplyr::select(catno, geometry) %>% 
    distinct()
  
  
  
  if(count_measu > 1) {
    
    measuredflow_continuous <- measuredflow_catno %>% 
      dplyr::filter(group == "continuous_flow")
    
    measuredflow_gauging <- measuredflow_catno %>% 
      dplyr::filter(group == "gauging")
    
    
    
    p_flow_runoff_compare <-
      
      ggplot(flow_runoff_catno, aes(Date, value, col = id))+
      #geom_line(size = 0.4, alpha =0.85)+
      geom_line(alpha =0.65)+
      geom_line(data = measuredflow_continuous, aes(x = Date , y = value, col = Site), alpha = 0.65)+
      geom_point(data =measuredflow_gauging, aes(x = Date , y = value, col = Site), size = 1.2)+
      #scale_color_manual(values =c("tomato", "dodgerblue2"))+
      #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
      facet_wrap(~catno)+
      scale_y_log10()+
      theme_bw()+
      labs(x = "",
           y = "m3/d")+
      scale_x_date( date_labels = "%b-%Y")+
      guides(colour=guide_legend(ncol=3)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "top", 
            legend.title = element_blank(),
            legend.text = element_text(size=8))
    
    
    cat_bbox <- st_as_sfc(sf::st_bbox(tm_cat_id, crs =2193)) %>% 
      st_buffer(., 6000)
    
    tm_catid <- tm_shape(cat_bbox)+
      tm_polygons(alpha = 0,
                  border.alpha = 0,
                  legend.show = F)+
      tm_shape(tm_cat_id)+
      tm_polygons(col = "grey60",
                  border.alpha = 0.7,
                  lwd = 0.5,
                  border.col ="black"
      )+
      #tm_shape(sw_sites_pts)+
      #tm_dots(col = "red", size = 0.08, alpha =0.6)+
      #tm_text("Site.Name", col = "black", ymod = 0.5, xmod =0.5, size = 0.4)+
      tm_shape(rua_streams)+
      tm_lines(col = "blue", lwd = 0.2)+
      tm_shape(tmap_site)+
      tm_dots(col = "red", size = 0.04, alpha =0.6)+
      tm_text("Site", col = "black", auto.placement = TRUE, size = 0.35)+
      tm_shape(Rua_domain_new)+
      tm_polygons("Id",
                  border.col ="red",
                  border.alpha = 0.6,
                  alpha = 0.0,
                  legend.show = F
      )
    
    rm(tmap_site)
    
    tm_inset <- 
      tm_shape(Rua_domain_new)+
      tm_polygons("Id",
                  border.col ="red",
                  border.alpha = 0.6,
                  alpha = 0.0,
                  legend.show = F
      )+
      tm_shape(topnet_rua_cat)+
      tm_polygons(border.col ="grey",
                  border.alpha = 0.4,
                  alpha = 0.0, lwd = 0.1)+
      tm_shape(tm_cat_id)+
      tm_polygons(col = "grey40",
                  border.alpha = 0.8, 
                  border.col ="black",
                  lwd = 1)
    
    
  } else if (count_measu ==  1 & count_measu_catno$group =="continuous_flow" ) {
    # measuredflow_continuous <- measuredflow_catno %>% 
    #   dplyr::filter(group == "continuous_flow")
    
    measuredflow_continuous <- measuredflow_catno %>% 
      dplyr::filter(group == "continuous_flow")
    
    p_flow_runoff_compare <-
      
      ggplot(flow_runoff_catno, aes(Date, value, col = id))+
      #geom_line(size = 0.4, alpha =0.85)+
      geom_line(alpha =0.65)+
      geom_line(data = measuredflow_continuous, aes(x = Date , y = value, col = Site), alpha = 0.65)+
      #geom_point(data =measuredflow_gauging, aes(x = Date , y = value, col = Site), size = 1.2)+
      #scale_color_manual(values =c("tomato", "dodgerblue2"))+
      #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
      facet_wrap(~catno)+
      scale_y_log10()+
      theme_bw()+
      labs(x = "",
           y = "m3/d")+
      scale_x_date( date_labels = "%b-%Y")+
      guides(colour=guide_legend(ncol=3)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "top", 
            legend.title = element_blank(),
            legend.text = element_text(size=8))
    
    cat_bbox <- st_as_sfc(sf::st_bbox(tm_cat_id, crs =2193)) %>% 
      st_buffer(., 6000)
    
    tm_catid <- tm_shape(cat_bbox)+
      tm_polygons(alpha = 0,
                  border.alpha = 0,
                  legend.show = F)+
      tm_shape(tm_cat_id)+
      tm_polygons(col = "grey60",
                  border.alpha = 0.7,
                  lwd = 0.5,
                  border.col ="black"
      )+
      #tm_shape(sw_sites_pts)+
      #tm_dots(col = "red", size = 0.08, alpha =0.6)+
      #tm_text("Site.Name", col = "black", ymod = 0.5, xmod =0.5, size = 0.4)+
      tm_shape(rua_streams)+
      tm_lines(col = "blue", lwd = 0.2)+
      tm_shape(tmap_site)+
      tm_dots(col = "red", size = 0.04, alpha =0.6)+
      tm_text("Site", col = "black", xmod = 0.2, ymod = 0.3, size = 0.45)+
      tm_shape(Rua_domain_new)+
      tm_polygons("Id",
                  border.col ="red",
                  border.alpha = 0.6,
                  alpha = 0.0,
                  legend.show = F
      )
    
    rm(tmap_site)
    
    tm_inset <- 
      tm_shape(Rua_domain_new)+
      tm_polygons("Id",
                  border.col ="red",
                  border.alpha = 0.6,
                  alpha = 0.0,
                  legend.show = F
      )+
      tm_shape(topnet_rua_cat)+
      tm_polygons(border.col ="grey",
                  border.alpha = 0.4,
                  alpha = 0.0, lwd = 0.1)+
      tm_shape(tm_cat_id)+
      tm_polygons(col = "grey40",
                  border.alpha = 0.8, 
                  border.col ="black",
                  lwd = 1)
    
    
  } else {
     # measuredflow_continuous <- measuredflow_catno %>% 
    #   dplyr::filter(group == "continuous_flow")
    
    measuredflow_gauging <- measuredflow_catno %>% 
      dplyr::filter(group == "gauging")
    
    p_flow_runoff_compare <-
      
      ggplot(flow_runoff_catno, aes(Date, value, col = id))+
      #geom_line(size = 0.4, alpha =0.85)+
      geom_line(alpha =0.65)+
      #geom_line(data = measuredflow_continuous, aes(x = Date , y = value, col = Site))+
      geom_point(data =measuredflow_gauging, aes(x = Date , y = value, col = Site), size = 1.2)+
      #scale_color_manual(values =c("tomato", "dodgerblue2"))+
      #geom_point(data =site_gaugings,  colour = "red", alpha = 0.7)+
      facet_wrap(~catno)+
      scale_y_log10()+
      theme_bw()+
      labs(x = "",
           y = "m3/d")+
      scale_x_date( date_labels = "%b-%Y")+
      guides(colour=guide_legend(ncol=3)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "top", 
            legend.title = element_blank(),
            legend.text = element_text(size=8))
    
    cat_bbox <- st_as_sfc(sf::st_bbox(tm_cat_id, crs =2193)) %>% 
      st_buffer(., 6000)
    
    tm_catid <- tm_shape(cat_bbox)+
      tm_polygons(alpha = 0,
                  border.alpha = 0,
                  legend.show = F)+
      tm_shape(tm_cat_id)+
      tm_polygons(col = "grey60",
                  border.alpha = 0.7,
                  lwd = 0.5,
                  border.col ="black"
      )+
      #tm_shape(sw_sites_pts)+
      #tm_dots(col = "red", size = 0.08, alpha =0.6)+
      #tm_text("Site.Name", col = "black", ymod = 0.5, xmod =0.5, size = 0.4)+
      tm_shape(rua_streams)+
      tm_lines(col = "blue", lwd = 0.2)+
      tm_shape(tmap_site)+
      tm_dots(col = "red", size = 0.04, alpha =0.6)+
      tm_text("Site", col = "black", xmod = 0.2, ymod = 0.2, size = 0.45)+
      tm_shape(Rua_domain_new)+
      tm_polygons("Id",
                  border.col ="red",
                  border.alpha = 0.6,
                  alpha = 0.0,
                  legend.show = F
      )
    
    rm(tmap_site)
    
    tm_inset <- 
      tm_shape(Rua_domain_new)+
      tm_polygons("Id",
                  border.col ="red",
                  border.alpha = 0.6,
                  alpha = 0.0,
                  legend.show = F
      )+
      tm_shape(topnet_rua_cat)+
      tm_polygons(border.col ="grey",
                  border.alpha = 0.4,
                  alpha = 0.0, lwd = 0.1)+
      tm_shape(tm_cat_id)+
      tm_polygons(col = "grey40",
                  border.alpha = 0.8, 
                  border.col ="black",
                  lwd = 1)
    
    
  }

  
  p <- plot_grid(p_flow_runoff_compare, tableGrob(ts_perc_table), ncol =1, 
                 rel_heights = c(3, 1))
  
  tm_catid_g <- tmap_grob(tm_catid)
  tm_inset_g <- tmap_grob(tm_inset)
  
  tm_g <- ggdraw()+
    draw_plot(tm_catid_g) +
    draw_plot(tm_inset_g,
              width = 0.25, height = 0.25,
              x = 0.05, y = 0.60)
  
  
  plot_combined <- plot_grid(tm_g, p,  rel_widths = c(1,2))
  
  save_plot(paste0("plots/runoff_higherthan_HBRCflows/", catno_runoff_higherthan_hbrcflows[i], ".png", sep ="") , plot_combined, base_height = 8, base_aspect_ratio = 1.5)
  
  
}

