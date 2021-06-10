installPackages <- function(){
  
  # list of packages
  packages <- c('mapview', 'sf', 'ggplot2','dplyr','RColorBrewer')
  
  # only install if package is not installed yet
  install.packages(setdiff(packages, rownames(installed.packages())))
  
  # apply packages from the list
  invisible(lapply(packages, library, character.only = TRUE))
}

installPackages()

library(mapview) # to plot tiles in map
library(sf) # to read shapefile .shp
library(ggplot2) # to plot other graphs
library(dplyr) # for stats
library(RColorBrewer) # for color palette

# Source Data: OOKLA Shapefile and Malaysia Districts Shapefile
# "https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=1/2020-01-01_performance_mobile_tiles.zip"
# "https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=2/2020-04-01_performance_mobile_tiles.zip"
# "https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=3/2020-07-01_performance_mobile_tiles.zip"
# "https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=4/2020-10-01_performance_mobile_tiles.zip"
# "https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2021/quarter=1/2021-01-01_performance_mobile_tiles.zip"
# Districts, Malaysia 2013 shapefile "https://earthworks.stanford.edu/catalog/stanford-zd362bc5680"

# Parse OOKLA shapefile according to Malaysia Districts using QGIS software
# 1. Load OOKLA shapefile and Malaysia Districts vector layers
# 2. Vector->GeoprocessingTools->Clip [InputLayer:OOKLA, OverlayLayer:MalaysiaDistricts]
# 3. Vector->DataManagementTools->JoinAttributebyLocation [BaseLayer:#2, JoinLayer:MalaysiaDistricts, GeometricPredicate:Overlaps&Within]
# 4. Save the joined layer as shapefile

# read the joined layer
mobile_q1_20 <- read_sf("Joined_2020_q1/mobile_q1_2020_join.shp")
mobile_q2_20 <- read_sf("Joined_2020_q2/mobile_q2_2020_join.shp")
mobile_q3_20 <- read_sf("Joined_2020_q3/mobile_q3_2020_join.shp")
mobile_q4_20 <- read_sf("Joined_2020_q4/mobile_q4_2020_join.shp")
mobile_q1_21 <- read_sf("Joined_2021_q1/mobile_q1_2021_join.shp")
county <- read_sf("data/polbnda_mys.shp")

#######################################################################

# stats by states '20 q1
states_stats_q1_20 <- mobile_q1_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by states '20 q2
states_stats_q2_20 <- mobile_q2_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by states '20 q3
states_stats_q3_20 <- mobile_q3_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by states '20 q4
states_stats_q4_20 <- mobile_q4_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by states '21 q1
states_stats_q1_21 <- mobile_q1_21 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# combine states stats
states_stats_q1_20["Quarter"] = "'20 Q1"
states_stats_q2_20["Quarter"] = "'20 Q2"
states_stats_q3_20["Quarter"] = "'20 Q3"
states_stats_q4_20["Quarter"] = "'20 Q4"
states_stats_q1_21["Quarter"] = "'21 Q1"

states_stats <- rbind(states_stats_q1_20,states_stats_q2_20,states_stats_q3_20,
                      states_stats_q4_20,states_stats_q1_21)

rankFunction <- function(state_name,county_name,quarter,type,lvl,highlight){

  
  if(lvl == "State"){
    if(quarter == "2020 Q1"){
      rankdf <- states_stats_q1_20
    } else if(quarter == "Q2"){
      rankdf <- states_stats_q2_20
    } else if(quarter == "Q3"){
      rankdf <- states_stats_q3_20
    } else if(quarter == "Q4"){
      rankdf <- states_stats_q4_20
    } else if(quarter == "2021 Q1"){
      rankdf <- states_stats_q1_21
    }
    
    if(highlight==TRUE){
      rankdf <- rankdf %>% mutate( ToHighlight = ifelse( nam == state_name, "yes", "no" ))
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean DL Speed (Mbps)",title="Mean DL Speed for different Quarter")
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean UL Speed (Mbps)",title="Mean UL Speed for different Quarter")
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_lat_ms_wt),y=mean_lat_ms_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Latency (Mbps)",title="Mean Latency for different Quarter")
      }
    }
    else{
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=mean_dl_mbps_wt)) +
          scale_fill_continuous(low = "darkblue", high = "green")+
          labs(x="",y="Mean DL Speed (Mbps)",title="Mean DL Speed for different Quarter")
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=mean_ul_mbps_wt)) +
          scale_fill_continuous(low = "darkblue", high = "green")+
          labs(x="",y="Mean UL Speed (Mbps)",title="Mean UL Speed for different Quarter")
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_lat_ms_wt),y=mean_lat_ms_wt,fill=mean_lat_ms_wt)) +
          scale_fill_continuous(low = "darkblue", high = "green")+
          labs(x="",y="Latency (Mbps)",title="Mean Latency for different Quarter")
      }
    }
  }
  
  else{
    if(quarter == "2020 Q1"){
      rankdf <- county_stats_q1_20
    } else if(quarter == "Q2"){
      rankdf <- county_stats_q2_20
    } else if(quarter == "Q3"){
      rankdf <- county_stats_q3_20
    } else if(quarter == "Q4"){
      rankdf <- county_stats_q4_20
    } else if(quarter == "2021 Q1"){
      rankdf <- county_stats_q1_21
    }
    
    if(highlight==TRUE){
      rankdf <- rankdf[rankdf['nam']==state_name,] %>% mutate( ToHighlight = ifelse( laa == county_name, "yes", "no" ))
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean DL Speed (Mbps)",title="Mean DL Speed for different Quarter")
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean UL Speed (Mbps)",title="Mean UL Speed for different Quarter")
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_lat_ms_wt),y=mean_lat_ms_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Latency (Mbps)",title="Mean Latency for different Quarter")
      }
    }
    else{
      rankdf <- rankdf[rankdf['nam']==state_name,]
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=mean_dl_mbps_wt)) +
          scale_fill_continuous(low = "darkblue", high = "green")+
          labs(x="",y="Mean DL Speed (Mbps)",title="Mean DL Speed for different Quarter")
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=mean_ul_mbps_wt)) +
          scale_fill_continuous(low = "darkblue", high = "green")+
          labs(x="",y="Mean UL Speed (Mbps)",title="Mean UL Speed for different Quarter")
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_lat_ms_wt),y=mean_lat_ms_wt,fill=mean_lat_ms_wt)) +
          scale_fill_continuous(low = "darkblue", high = "green")+
          labs(x="",y="Latency (Mbps)",title="Mean Latency for different Quarter")
      }
    }
  }
  rankplot <- rankplot +
    geom_bar(position="dodge2",stat="identity") + 
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    coord_flip()
  return(rankplot)
}

rankp<-rankFunction("KEDAH","KOTA SETAR","Q2","Download","County",TRUE)

rank <- mobile_q1_20 %>% mutate( ToHighlight = ifelse( nam == "JOHOR", "yes", "no" ) )
# plot DL Speed stats by states
ggplot(rank, aes(x=reorder(nam, avg_d_kbps),y=avg_d_kbps,fill=avg_d_kbps)) +
  scale_fill_continuous(low = "darkblue", high = "green")+
  geom_bar(position="dodge2",stat="identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x="",y="Mean DL Speed (Mbps)",title="Mean DL Speed for different Quarter")+
  coord_flip()

#######################################################################

# stats by county '20 q1
county_stats_q1_20 <- mobile_q1_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam,laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q2
county_stats_q2_20 <- mobile_q2_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam,laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q3
county_stats_q3_20 <- mobile_q3_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam,laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q4
county_stats_q4_20 <- mobile_q4_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam,laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '21 q1
county_stats_q1_21 <- mobile_q1_21 %>%
  st_set_geometry(NULL) %>%
  group_by(nam,laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# combine county stats
county_stats_q1_20["Quarter"] = "'20 Q1"
county_stats_q2_20["Quarter"] = "'20 Q2"
county_stats_q3_20["Quarter"] = "'20 Q3"
county_stats_q4_20["Quarter"] = "'20 Q4"
county_stats_q1_21["Quarter"] = "'21 Q1"

county_stats <- rbind(county_stats_q1_20,county_stats_q2_20,county_stats_q3_20
                      ,county_stats_q4_20,county_stats_q1_21)

# plot DL speed stats by county eg KL
county_name = "KUALA LUMPUR"
county_stats %>%
  filter(laa==county_name) %>%
  ggplot(aes(x=laa,y=mean_dl_mbps_wt,fill=Quarter)) + 
  geom_bar(position="dodge2",stat="identity") + 
  #theme(axis.text.x=element_text(angle=270, hjust=1),axis.text.y=element_text(angle=180, hjust=1)) +
  labs(x="",y="Mean DL Speed (Mbps)",title="Mean DL Speed for different Quarter") + scale_fill_manual( values = c("grey","grey","tomato","grey","grey"), guide = FALSE )
  #coord_flip()


library("reshape2")
mdf <- melt(states_stats[c('nam','Quarter','mean_dl_mbps_wt')], id.vars=c("nam","Quarter"), value.name="value")
ggplot(data=mdf, aes(x=Quarter, y=value, group = nam, colour = nam)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white")

#######################################################################

# mapping DL speed by state e.g johor
state_name = "JOHOR"
state_tiles <- mobile_q1_21[mobile_q1_21$nam == state_name,]
state_polygon <- county[county$nam == state_name,]
mapview(state_tiles["avg_d_kbps"],legend=T, at=seq(0,150000,10000),layer.name="avg_d_kbps", map.types = "OpenStreetMap", label="avg_d_kbps") +
  mapview(state_polygon, alpha.regions = 0.1, alpha = 1)

# mapping DL speed by county e.g kl
county_name = "KUALA LUMPUR"
county_tiles <- mobile_q1_21[mobile_q1_21$laa == county_name,]
county_polygon <- county[county$laa == county_name,] 
mapview(county_tiles["avg_d_kbps"],legend=T,at=seq(0,150000,10000),layer.name="avg_d_kbps", map.types = "OpenStreetMap", label="avg_d_kbps") +
  mapview(county_polygon, alpha.regions = 0.1, alpha = 1) 

#######################################################################

# plot mean DL speed using ggplot by county

county_stats_sf <- county %>%
  select(laa) %>%
  left_join(county_stats_q1_21 %>% mutate(laa = as.character(laa)), by = c("laa")) %>%
  mutate(mean_dl_mbps_wt = case_when(tests < 50 ~ NA_real_,
                                     TRUE ~ mean_dl_mbps_wt)) %>% # at least 50 tests
  mutate(dl_cat = cut(mean_dl_mbps_wt, c(0,10,20,30,40,50), ordered_result = TRUE))


ggplot() +
  geom_sf(data = county_stats_sf, aes(fill = dl_cat), color = "gray20", lwd = 0.1) +
  #geom_sf_text(data = ky_places, aes(label = NAME), color = "black", size = 3) +
  theme_void() +
  scale_fill_manual(values = brewer.pal(n = 6, name = "Accent"),  
                    na.value = "gray80", 
                    labels = c("0 to 10", "10.1 to 20", "20.1 to 30", "30.1 to 40", "40.1 to 50", "Insufficient data"), 
                    name = "Mean download speed (Mbps)", 
                    guide = guide_legend(direction = "horizontal", title.position = "top", nrow = 1, label.position = "bottom", keyheight = 0.5, keywidth = 5)) +
  theme(text = element_text(color = "gray25"), legend.position = "top")
