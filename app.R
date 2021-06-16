library(mapview) # to plot tiles in map
library(leaflet)
library(leafpop)
library(sf) # to read shapefile .shp
library(ggplot2) # to plot other graphs
library(plyr) # for dlply function
library(dplyr) # for stats
library(RColorBrewer) # for color palette
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(viridis) # Baha - added 11/6/2021
library(DT) # Baha - added 13/6/2021
library(reshape2)
library(highcharter)
library(plotly)
library(tools)

# Baha - modified 7/6/2021 - comment out read_sf, load input from local database
# mobile_q1_20 <- read_sf("Joined_2020_q1/mobile_q1_2020_join.shp")
# mobile_q2_20 <- read_sf("Joined_2020_q2/mobile_q2_2020_join.shp")
# mobile_q3_20 <- read_sf("Joined_2020_q3/mobile_q3_2020_join.shp")
# mobile_q4_20 <- read_sf("Joined_2020_q4/mobile_q4_2020_join.shp")
# mobile_q1_21 <- read_sf("Joined_2021_q1/mobile_q1_2021_join.shp")
# county <- read_sf("data/polbnda_mys.shp")
load('myProjectEnvironment_v2.RData')

# Baha - modified 11/6/2021 - added ul, dl, lat rank for state and country stats
# stats by states '20 q1
states_stats_q1_20 <- mobile_q1_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
states_stats_q1_20$rank_dl <- rank(-states_stats_q1_20$mean_dl_mbps_wt)
states_stats_q1_20$rank_ul <- rank(-states_stats_q1_20$mean_ul_mbps_wt)
states_stats_q1_20$rank_lat <- rank(states_stats_q1_20$mean_lat_ms_wt)

# stats by states '20 q2
states_stats_q2_20 <- mobile_q2_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
states_stats_q2_20$rank_dl <- rank(-states_stats_q2_20$mean_dl_mbps_wt)
states_stats_q2_20$rank_ul <- rank(-states_stats_q2_20$mean_ul_mbps_wt)
states_stats_q2_20$rank_lat <- rank(states_stats_q2_20$mean_lat_ms_wt)

# stats by states '20 q3
states_stats_q3_20 <- mobile_q3_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
states_stats_q3_20$rank_dl <- rank(-states_stats_q3_20$mean_dl_mbps_wt)
states_stats_q3_20$rank_ul <- rank(-states_stats_q3_20$mean_ul_mbps_wt)
states_stats_q3_20$rank_lat <- rank(states_stats_q3_20$mean_lat_ms_wt)

# stats by states '20 q4
states_stats_q4_20 <- mobile_q4_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
states_stats_q4_20$rank_dl <- rank(-states_stats_q4_20$mean_dl_mbps_wt)
states_stats_q4_20$rank_ul <- rank(-states_stats_q4_20$mean_ul_mbps_wt)
states_stats_q4_20$rank_lat <- rank(states_stats_q4_20$mean_lat_ms_wt)

# stats by states '21 q1
states_stats_q1_21 <- mobile_q1_21 %>%
  st_set_geometry(NULL) %>%
  group_by(nam) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
states_stats_q1_21$rank_dl <- rank(-states_stats_q1_21$mean_dl_mbps_wt)
states_stats_q1_21$rank_ul <- rank(-states_stats_q1_21$mean_ul_mbps_wt)
states_stats_q1_21$rank_lat <- rank(states_stats_q1_21$mean_lat_ms_wt)

# combine states stats
states_stats_q1_20["Quarter"] = "'20 Q1"
states_stats_q2_20["Quarter"] = "'20 Q2"
states_stats_q3_20["Quarter"] = "'20 Q3"
states_stats_q4_20["Quarter"] = "'20 Q4"
states_stats_q1_21["Quarter"] = "'21 Q1"

states_stats <- rbind(states_stats_q1_20,states_stats_q2_20,states_stats_q3_20,
                      states_stats_q4_20,states_stats_q1_21)


# stats by county '20 q1
county_stats_q1_20 <- mobile_q1_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
county_stats_q1_20$rank_dl <- rank(-county_stats_q1_20$mean_dl_mbps_wt)
county_stats_q1_20$rank_ul <- rank(-county_stats_q1_20$mean_ul_mbps_wt)
county_stats_q1_20$rank_lat <- rank(county_stats_q1_20$mean_lat_ms_wt)

# stats by county '20 q2
county_stats_q2_20 <- mobile_q2_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
county_stats_q2_20$rank_dl <- rank(-county_stats_q2_20$mean_dl_mbps_wt)
county_stats_q2_20$rank_ul <- rank(-county_stats_q2_20$mean_ul_mbps_wt)
county_stats_q2_20$rank_lat <- rank(county_stats_q2_20$mean_lat_ms_wt)

# stats by county '20 q3
county_stats_q3_20 <- mobile_q3_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
county_stats_q3_20$rank_dl <- rank(-county_stats_q3_20$mean_dl_mbps_wt)
county_stats_q3_20$rank_ul <- rank(-county_stats_q3_20$mean_ul_mbps_wt)
county_stats_q3_20$rank_lat <- rank(county_stats_q3_20$mean_lat_ms_wt)

# stats by county '20 q4
county_stats_q4_20 <- mobile_q4_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
county_stats_q4_20$rank_dl <- rank(-county_stats_q4_20$mean_dl_mbps_wt)
county_stats_q4_20$rank_ul <- rank(-county_stats_q4_20$mean_ul_mbps_wt)
county_stats_q4_20$rank_lat <- rank(county_stats_q4_20$mean_lat_ms_wt)

# stats by county '21 q1
county_stats_q1_21 <- mobile_q1_21 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))
county_stats_q1_21$rank_dl <- rank(-county_stats_q1_21$mean_dl_mbps_wt)
county_stats_q1_21$rank_ul <- rank(-county_stats_q1_21$mean_ul_mbps_wt)
county_stats_q1_21$rank_lat <- rank(county_stats_q1_21$mean_lat_ms_wt)

# combine county stats
county_stats_q1_20["Quarter"] = "'20 Q1"
county_stats_q2_20["Quarter"] = "'20 Q2"
county_stats_q3_20["Quarter"] = "'20 Q3"
county_stats_q4_20["Quarter"] = "'20 Q4"
county_stats_q1_21["Quarter"] = "'21 Q1"

county_stats <- rbind(county_stats_q1_20,county_stats_q2_20,county_stats_q3_20
                      ,county_stats_q4_20,county_stats_q1_21)


county_state <- mobile_q1_20 %>%
  as.data.frame()%>%
  select(c('nam','laa')) %>%
  unique() %>%
  .[order(.['nam'],.['laa']),] %>%
  dlply(.(nam))


# Baha - added 11/6/2021 - helper function to calculate ave, max, min in map
national_stat <- function(quarter,type){
    if(quarter=="2020 Q1"){
      n_stats <- states_stats_q1_20
    } else if(quarter=="Q2"){
      n_stats <- states_stats_q2_20
    } else if(quarter=="Q3"){
      n_stats <- states_stats_q3_20
    } else if(quarter=="Q4"){
      n_stats <- states_stats_q4_20
    } else if(quarter=="2021 Q1"){
      n_stats <- states_stats_q1_21
    }

    if(type == "Download"){
      national_stat <- n_stats %>%
        summarise(ave_n = round(mean(mean_dl_mbps_wt),digits=2),
                  min_n = round(min(mean_dl_mbps_wt),digits=2),
                  max_n = round(max(mean_dl_mbps_wt),digits=2))
    } else if(type == "Upload"){
      national_stat <- n_stats %>%
        summarise(ave_n = round(mean(mean_ul_mbps_wt),digits=2),
                  min_n = round(min(mean_ul_mbps_wt),digits=2),
                  max_n = round(max(mean_ul_mbps_wt),digits=2))
    } else if(type == "Latency"){
      national_stat <- n_stats %>%
        summarise(ave_n = round(mean(mean_lat_ms_wt),digits=2),
                  min_n = round(max(mean_lat_ms_wt),digits=2),
                  max_n = round(min(mean_lat_ms_wt),digits=2))
    }

    return(national_stat)
}


# Baha - added 11/6/2021 - helper function to show ave, max, min, rank values in map
get_rank <- function(state,quarter,type){
  if(quarter=="2020 Q1"){
    n_stats <- states_stats_q1_20
  } else if(quarter=="Q2"){
    n_stats <- states_stats_q2_20
  } else if(quarter=="Q3"){
    n_stats <- states_stats_q3_20
  } else if(quarter=="Q4"){
    n_stats <- states_stats_q4_20
  } else if(quarter=="2021 Q1"){
    n_stats <- states_stats_q1_21
  }
  
  if(type == "Download")
  {
    data <- n_stats %>%
      filter(rank_dl == min(rank_dl)) %>%
      summarise(first.rank = nam)
    data$last <- n_stats %>%
      filter(rank_dl == max(rank_dl)) %>%
      summarise(rank = nam)
    data$target_rank <- n_stats %>%
      filter(nam == state) %>%
      summarise(rank = rank_dl)
    data$target_value <- n_stats %>%
      filter(nam == state) %>%
      summarise(value = round(mean_dl_mbps_wt,digits=2))
    data$target_state <- state
    data$unit <- " Mbps"
    data$p_direction <- " Maximum "
    data$n_direction <- " Minimum "
  }else if(type == "Upload"){
    data <- n_stats %>%
      filter(rank_ul == min(rank_ul)) %>%
      summarise(first.rank = nam)
    data$last <- n_stats %>%
      filter(rank_ul == max(rank_ul)) %>%
      summarise(rank = nam)
    data$target_rank <- n_stats %>%
      filter(nam == state) %>%
      summarise(rank = rank_ul)
    data$target_value <- n_stats %>%
      filter(nam == state) %>%
      summarise(value = round(mean_ul_mbps_wt,digits=2))
    data$target_state <- state
    data$unit <- " Mbps"
    data$p_direction <- " Maximum "
    data$n_direction <- " Minimum "
  } else if(type == "Latency"){
    data <- n_stats %>%
      filter(rank_lat == min(rank_lat)) %>%
      summarise(first.rank = nam)
    data$last <- n_stats %>%
      filter(rank_lat == max(rank_lat)) %>%
      summarise(rank = nam)
    data$target_rank <- n_stats %>%
      filter(nam == state) %>%
      summarise(rank = rank_lat)
    data$target_value <- n_stats %>%
      filter(nam == state) %>%
      summarise(value = round(mean_lat_ms_wt,digits=2))
    data$target_state <- state
    data$unit <- " ms"
    data$p_direction <- " Minimum "
    data$n_direction <- " Maximum "
  }
  return(data)
}

# Baha - modified 7/6/2021 - mapfunction based on user input
mapfunction <- function(state_name,county_name,quarter,type,map_type){
  if(map_type == "State"){
    if(quarter == "2020 Q1"){
      tiles <- mobile_q1_20[mobile_q1_20$nam == state_name,]
      polygon <- county[county$nam == state_name,]
    } else if(quarter == "Q2"){
      tiles <- mobile_q2_20[mobile_q2_20$nam == state_name,]
      polygon <- county[county$nam == state_name,]
    } else if(quarter == "Q3"){
      tiles <- mobile_q3_20[mobile_q3_20$nam == state_name,]
      polygon <- county[county$nam == state_name,]
    } else if(quarter == "Q4"){
      tiles <- mobile_q4_20[mobile_q4_20$nam == state_name,]
      polygon <- county[county$nam == state_name,]
    } else if(quarter == "2021 Q1"){
      tiles <- mobile_q1_21[mobile_q1_21$nam == state_name,]
      polygon <- county[county$nam == state_name,]
    }
  } else if(map_type == "District"){
    if(quarter == "2020 Q1"){
      tiles <- mobile_q1_20[mobile_q1_20$laa == county_name,]
      polygon <- county[county$laa == county_name,]
    } else if(quarter == "Q2"){
      tiles <- mobile_q2_20[mobile_q2_20$laa == county_name,]
      polygon <- county[county$laa == county_name,]
    } else if(quarter == "Q3"){
      tiles <- mobile_q3_20[mobile_q3_20$laa == county_name,]
      polygon <- county[county$laa == county_name,]
    } else if(quarter == "Q4"){
      tiles <- mobile_q4_20[mobile_q4_20$laa == county_name,]
      polygon <- county[county$laa == county_name,]
    } else if(quarter == "2021 Q1"){
      tiles <- mobile_q1_21[mobile_q1_21$laa == county_name,]
      polygon <- county[county$laa == county_name,]
    }
  }
  
  if(type == "Download"){
    show_map <- mapview(polygon,alpha.regions=0.1,alpha=1,layer.name=NULL,legend=F,label=FALSE,popup=FALSE,map.types="OpenStreetMap") +
      mapview(tiles[c("avg_d_mbps","avg_u_mbps","avg_lat_ms","tests","devices")],zcol="avg_d_mbps",legend=T,at=seq(100,0,-5),layer.name="Average Download (Mbps)")
  } else if(type == "Upload"){
    show_map <- mapview(polygon,alpha.regions=0.1,alpha=1,layer.name=NULL,legend=F,label=FALSE,popup=FALSE,map.types="OpenStreetMap") +
      mapview(tiles[c("avg_u_mbps","avg_d_mbps","avg_lat_ms","tests","devices")],zcol="avg_u_mbps",legend=T,at=seq(50,0,-5),layer.name="Average Upload (Mbps)")
  } else if(type == "Latency"){
    show_map <- mapview(polygon,alpha.regions=0.1,alpha=1,layer.name=NULL,legend=F,label=FALSE,popup=FALSE,map.types="OpenStreetMap") + 
      #Baha - modified 11/6/2021 - reverse viridis color palette in map for latency
      mapview(tiles[c("avg_lat_ms","avg_d_mbps","avg_u_mbps","tests","devices")],zcol="avg_lat_ms",legend=T,at=seq(0,100,5),col.regions=viridis(20,direction=-1),layer.name="Average Latency (ms)")
  }
  
  return(show_map)
}

rankFunction <- function(state_name,county_name,quarter,type,lvl,highlight){
  
  
  if(lvl == "State"){
    if(quarter == "2020 Q1"){
      rankdf <- states_stats_q1_20
      quarter_name <- "Q1 2020"
    } else if(quarter == "Q2"){
      rankdf <- states_stats_q2_20
      quarter_name <- "Q2 2020"
    } else if(quarter == "Q3"){
      rankdf <- states_stats_q3_20
      quarter_name <- "Q3 2020"
    } else if(quarter == "Q4"){
      rankdf <- states_stats_q4_20
      quarter_name <- "Q4 2020"
    } else if(quarter == "2021 Q1"){
      rankdf <- states_stats_q1_21
      quarter_name <- "Q1 2021"
    }
    
    if(highlight==TRUE){
      rankdf <- rankdf %>% mutate( ToHighlight = ifelse( nam == state_name, "yes", "no" ))
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean DL Speed (Mbps)",title=paste("Mean Download Speed for All States",quarter_name))+ 
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean UL Speed (Mbps)",title=paste("Mean Upload Speed for All States",quarter_name))+ 
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Latency (ms)",title=paste("Mean Latency for All States",quarter_name))+ 
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
    else{
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=mean_dl_mbps_wt)) +
          scale_fill_viridis()+
          labs(x="",y="Mean DL Speed (Mbps)",title=paste("Mean Download Speed for All States",quarter_name))+ 
          labs(fill = "Latency (ms)")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=mean_ul_mbps_wt)) +
          scale_fill_viridis()+
          labs(x="",y="Mean UL Speed (Mbps)",title=paste("Mean Upload Speed for All States",quarter_name))+ 
          labs(fill = "Latency (ms)")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=mean_lat_ms_wt)) +
          scale_fill_viridis(trans = 'reverse')+
          labs(x="",y="Latency (ms)",title=paste("Mean Latency for All States",quarter_name))+ 
          labs(fill = "Latency (ms)")+
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
  }
  
  else{
    if(quarter == "2020 Q1"){
      rankdf <- county_stats_q1_20
      quarter_name <- "Q1 2020"
    } else if(quarter == "Q2"){
      rankdf <- county_stats_q2_20
      quarter_name <- "Q2 2020"
    } else if(quarter == "Q3"){
      rankdf <- county_stats_q3_20
      quarter_name <- "Q3 2020"
    } else if(quarter == "Q4"){
      rankdf <- county_stats_q4_20
      quarter_name <- "Q4 2020"
    } else if(quarter == "2021 Q1"){
      rankdf <- county_stats_q1_21
      quarter_name <- "Q1 2021"
    }
    
    if(highlight==TRUE){
      rankdf <- rankdf[rankdf['nam']==state_name,] %>% mutate( ToHighlight = ifelse( laa == county_name, "yes", "no" ))
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean DL Speed (Mbps)",title=paste("Mean Download Speed for",toTitleCase(state_name),quarter_name))+ 
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Mean UL Speed (Mbps)",title=paste("Mean Upload Speed for",toTitleCase(state_name),quarter_name))+ 
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Latency"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Latency (ms)",title=paste("Mean Latency for",toTitleCase(state_name),quarter_name))+ 
          theme(legend.position = "none")+
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
    else{
      rankdf <- rankdf[rankdf['nam']==state_name,]
      if(type == "Download"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_dl_mbps_wt),y=mean_dl_mbps_wt,fill=mean_dl_mbps_wt)) +
          scale_fill_viridis()+
          labs(x="",y="Mean DL Speed (Mbps)",title=paste("Mean Download Speed for",toTitleCase(state_name),quarter_name))+ 
          labs(fill = "Latency (ms)")+
          theme(plot.title = element_text(hjust = 0.5))
      } else if(type == "Upload"){
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, mean_ul_mbps_wt),y=mean_ul_mbps_wt,fill=mean_ul_mbps_wt)) +
          scale_fill_viridis()+
          labs(x="",y="Mean UL Speed (Mbps)",title=paste("Mean Upload Speed for",toTitleCase(state_name),quarter_name))+ 
          labs(fill = "Latency (ms)")+
          theme(plot.title = element_text(hjust = 0.5))
      } else{
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=mean_lat_ms_wt)) +
          scale_fill_viridis(trans = 'reverse')+
          labs(x="",y="Latency (ms)",title=paste("Mean Latency for",toTitleCase(state_name),quarter_name))+ 
          labs(fill = "Latency (ms)")+
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
  }
  rankplot <- rankplot +
    geom_bar(position="dodge2",stat="identity") + 
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    coord_flip()
  return(rankplot)
}

#bar_count <- function(lvl, state){
#  if(lvl =="State"){
#    return(count(unique(states_stats['nam'])))
#  }
#  else{
#    return(count(unique(county_stats[county_stats['nam']==state,'laa'])))
#  }
#}


line_plot_function <- function(lvl, list1, state, type){
  
  if(type == "Download"){
    if(lvl == "State"){
      melt_states_stats <- melt(states_stats[c('nam','Quarter','mean_dl_mbps_wt')], id.vars=c("nam","Quarter"), value.name='Download')
      names_option <- melt_states_stats[melt_states_stats$nam %in% list1,]
      lineplot <- names_option %>% 
        hchart('line', hcaes(x = Quarter, y = Download, group = nam))%>%
        hc_size(height = 600) %>%
        hc_title(text = 'Download speed (Mbps)')
      return(lineplot)
    }
    
    else{
      melt_district_stats <- melt(county_stats[county_stats['nam']==state, c('laa','Quarter','mean_dl_mbps_wt')], id.vars=c("laa","Quarter"), value.name='Download')
      names_option <- melt_district_stats[melt_district_stats$laa %in% list1,]
      lineplot <- names_option %>% 
        hchart('line', hcaes(x = Quarter, y = Download, group = laa))%>%
        hc_size(height = 600) %>%
        hc_title(text = 'Download speed (Mbps)')
      return(lineplot)
    }
  }
  else if(type == "Upload"){
    if(lvl == "State"){
      melt_states_stats <- melt(states_stats[c('nam','Quarter','mean_ul_mbps_wt')], id.vars=c("nam","Quarter"), value.name='Upload')
      names_option <- melt_states_stats[melt_states_stats$nam %in% list1,]
      lineplot <- names_option %>% 
        hchart('line', hcaes(x = Quarter, y = Upload, group = nam))%>%
        hc_size(height = 600) %>%
        hc_title(text = 'Upload speed (Mbps)')
      return(lineplot)
    }
    
    else{
      melt_district_stats <- melt(county_stats[county_stats['nam']==state, c('laa','Quarter','mean_ul_mbps_wt')], id.vars=c("laa","Quarter"), value.name='Upload')
      names_option <- melt_district_stats[melt_district_stats$laa %in% list1,]
      lineplot <- names_option %>% 
        hchart('line', hcaes(x = Quarter, y = Upload, group = laa))%>%
        hc_size(height = 600) %>%
        hc_title(text = 'Upload speed (Mbps)')
      return(lineplot)
    }
  }
  else{
    if(lvl == "State"){
      melt_states_stats <- melt(states_stats[c('nam','Quarter','mean_lat_ms_wt')], id.vars=c("nam","Quarter"), value.name='Latency')
      names_option <- melt_states_stats[melt_states_stats$nam %in% list1,]
      lineplot <- names_option %>% 
        hchart('line', hcaes(x = Quarter, y = Latency, group = nam))%>%
        hc_size(height = 600) %>%
        hc_title(text = 'Latency trend (ms)')
      return(lineplot)
    }
    
    else{
      melt_district_stats <- melt(county_stats[county_stats['nam']==state, c('laa','Quarter','mean_lat_ms_wt')], id.vars=c("laa","Quarter"), value.name='Latency')
      names_option <- melt_district_stats[melt_district_stats$laa %in% list1,]
      lineplot <- names_option %>% 
        hchart('line', hcaes(x = Quarter, y = Latency, group = laa))%>%
        hc_size(height = 600) %>%
        hc_title(text = 'Latency trend (ms)')
      return(lineplot)
    }
  }
}


# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  theme = shinytheme("sandstone"),
  navbarPage(
    "Mobile Network Analysis",
    
    tabPanel("Map",
             sidebarPanel(width = 2,
                          # Baha - modified 7/6/2021 - add input to select map level
                          radioGroupButtons("level_select1", "Map View:",   
                                      choices = c("State", "District"), 
                                      selected = c("District"),
                                      justified = TRUE),
                          pickerInput("state_select1", "State:",   
                                      choices = names(county_state), 
                                      selected = "KEDAH",
                                      options = list(
                                        `live-search` = TRUE)),
                          pickerInput("county_select1", "District:",   
                                      choices = pull(county_state[['KEDAH']],'laa'), 
                                      selected = "BALING",
                                      options = list(
                                        `live-search` = TRUE)),
                          pickerInput("type_select1", "Type:",   
                                      choices = c("Download", "Upload", "Latency"), 
                                      selected = c("Download")),
                          shinyWidgets::sliderTextInput("quarter_select1", 
                                                        label = "Quarter:", 
                                                        choices = c("2020 Q1", "Q2", "Q3", "Q4", "2021 Q1"),
                                                        grid = TRUE)
             ),
             # Baha - modified 11/6/2021 - add panel to show ave, min, max values
             mainPanel(width = 10,
                       div(class="inner",tags$head(includeCSS("styles1.css")),
                       leafletOutput("mapplot", height= 700),
                      
                       absolutePanel(id = "controls", class = "panel panel-default",
                                    top = 200, left = 350, width = 250, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    
                                    span(tags$i(h6("National level statistics are calculated using weighted mean, by averaging each tile values to the number of conducted tests.")), style="color:#045a8d"),
                                    h2(textOutput("mean_national"), align = "right"),
                                    h6(textOutput("mean_national_desc"), align = "right"),
                                    h4(textOutput("max_national"), align = "right"),
                                    h6(textOutput("max_national_desc"), align = "right"),
                                    h4(textOutput("min_national"), align = "right"),
                                    h6(textOutput("min_national_desc"), align = "right"),
                                    h2(textOutput("target_state"), align = "right"),
                                    h6(textOutput("target_state_desc"), align = "right"),
                                    h4(textOutput("mean_target_state"), align = "right"),
                                    h6(textOutput("mean_target_state_desc"), align = "right"),
                                    h4(textOutput("mean_target_district"), align = "right"),
                                    h6(textOutput("mean_target_district_desc"), align = "right"),
                                    ),

                       ),
            )
    ),
    tabPanel("Ranking",
             sidebarPanel(width = 2,
                          radioGroupButtons("level_select2", "Level:",   
                                            choices = c("State", "District"), 
                                            selected = c("District"),
                                            justified = TRUE),
                          pickerInput("state_select2", "State:",   
                                      choices = names(county_state), 
                                      selected = "KEDAH",
                                      options = list(
                                        `live-search` = TRUE)),
                          pickerInput("county_select2", "District:",   
                                      choices = pull(county_state[['KEDAH']],'laa'), 
                                      selected = "BALING",
                                      options = list(
                                        `live-search` = TRUE)),
                          pickerInput("type_select2", "Type:",   
                                      choices = c("Download", "Upload", "Latency"), 
                                      selected = c("Download")),
                          shinyWidgets::sliderTextInput("quarter_select2", 
                                                        label = "Quarter:", 
                                                        choices = c("2020 Q1", "Q2", "Q3", "Q4", "2021 Q1"),
                                                        # Baha - modified 11/6/2021 - add animation
                                                        animate=animationOptions(interval = 3000, loop = FALSE)),
                          materialSwitch("highlight2",
                            label = "Highlight selection", 
                            value = TRUE, 
                            right = TRUE,
                            status = "success"
                          )
             ),
             mainPanel(width = 10,
                       plotlyOutput("ranking_plot", height = "600px")
             )         
    ),
    
    tabPanel("Trend",
             sidebarPanel(width = 2,
                          radioGroupButtons("level_select3", "Level:",   
                                            choices = c("State", "District"), 
                                            selected = c("District"),
                                            justified = TRUE),
                          pickerInput("state_select3", "State:",   
                                      choices = names(county_state), 
                                      selected = "KEDAH",
                                      options = list(
                                        `live-search` = TRUE)),
                          pickerInput("type_select3", "Type:",   
                                      choices = c("Download", "Upload", "Latency"), 
                                      selected = c("Download")),                        
                          pickerInput(
                            inputId = "line_select3", label = "Show :",
                            choices = pull(county_state[['KEDAH']],'laa'), 
                            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0}/{1} Selected"),
                            multiple = TRUE
                          )
             ),
             mainPanel(width = 10,
                       highchartOutput("line_plot", width = "100%", height = "100%")
             )  
    ),
    
    # Baha - added 13/6/2021 - added data search tab
    tabPanel("Data Search",
             radioGroupButtons("datatable_select", "Select Data to view:",   
                               choices = c("State", "District"), 
                               selected = c("State")),

             DT::dataTableOutput("data_table")
    ),
    
    navbarMenu("Info",
               tabPanel("User Manual",
                        # Baha - modified 13/6/2021 - add information in user manual tab
                        tags$div(tags$img(src="Word_Cloud.png",width="50%",height="50%"),align="center"),
                        tags$br(),tags$br(),tags$b("Mobile Network Performance Speed Coverage Map"),
                        " is an interactive network speed check service check service which 
                        providing network speed comparison (ranking) among places, displaying 
                        speed and coverage on specified location, as well as quarterly network 
                        speed change trend throughout a year, based on user input.",tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),tags$div(tags$img(src="Quarterly_Trend.png",width="35%",height="35%"),align="left"),tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),tags$div(tags$img(src="Geo_Ranking.png",width="35%",height="35%"),align="right"),tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),tags$div(tags$img(src="Connectivity_Map.png",width="35%",height="35%"),align="left"),tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),tags$div(tags$img(src="Speed_Info.png",width="35%",height="35%"),align="right"),tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),
                        ),
               tabPanel("About",
                        # Baha - modified 13/6/2021 - add information in about tab
                        tags$div(tags$img(src="Picture_About.png",width="80%",height="80%"),align="center"),
                        tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),
                        tags$div(tags$img(src="About_Desc.png", width="80%", height="80%"),align="center"),
                        tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),
                        tags$div(tags$img(src="Data.png",width="10%",height="10%"),align="center"),tags$br(),
                        tags$div(tags$a(href="https://github.com/teamookla/ookla-open-data",tags$img(src="ookla-for-good.svg",width="10%",height="10%")),HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'),
                        tags$a(href="https://earthworks.stanford.edu/catalog/stanford-zd362bc5680", tags$img(src="stanford-libraries.svg",width="10%",height="10%")),align="center"),
                        tags$br(),tags$br(),
                        tags$div(tags$b("____________________________________________________________________________"),align="center"),
                        tags$br(),tags$br(),
                        )
               
               
    )
  )
)

server <- function(input, output, session) {
  #synchronizing inputs across 3 tabs
  observe({
    x <- input$level_select1
    updateRadioGroupButtons(session, "level_select2", selected = x)
    updateRadioGroupButtons(session, "level_select3", selected = x)
  })
  observe({
    x <- input$level_select2
    updateRadioGroupButtons(session, "level_select1", selected = x)
    updateRadioGroupButtons(session, "level_select3", selected = x)
  })
  observe({
    x <- input$level_select3
    updateRadioGroupButtons(session, "level_select1", selected = x)
    updateRadioGroupButtons(session, "level_select2", selected = x)
    if(x == "District"){
      updatePickerInput(session, "line_select3",
                        choices = county_state[[input$state_select3]][,'laa'], 
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                       `count-selected-text` = "{0}/{1} Selected")
      )
    }
    else{
      updatePickerInput(session, "line_select3",
                        choices = names(county_state), 
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                       `count-selected-text` = "{0}/{1} Selected")
      )
    }
  })
  
  observe({
    x <- input$state_select1
    updatePickerInput(session, "state_select2", selected = x)
    updatePickerInput(session, "state_select3", selected = x)
    updatePickerInput(session, "county_select1", choices = pull(county_state[[x]],'laa'))
  })
  observe({
    x <- input$state_select2
    updatePickerInput(session, "state_select1", selected = x)
    updatePickerInput(session, "state_select3", selected = x)
    updatePickerInput(session, "county_select2", choices = pull(county_state[[x]],'laa'))
  })
  observe({
    x <- input$state_select3
    updatePickerInput(session, "state_select1", selected = x)
    updatePickerInput(session, "state_select2", selected = x)
    updatePickerInput(session, "county_select3", choices = pull(county_state[[x]],'laa'))
  })
  
  observe({
    x <- input$county_select1
    updatePickerInput(session, "county_select2", selected = x)
  })
  observe({
    x <- input$county_select2
    updatePickerInput(session, "county_select1", selected = x)
  })
  observe({
    x <- input$type_select1
    updatePickerInput(session, "type_select2", selected = x)
    updatePickerInput(session, "type_select3", selected = x)
  })
  observe({
    x <- input$type_select2
    updatePickerInput(session, "type_select1", selected = x)
    updatePickerInput(session, "type_select3", selected = x)
  })
  observe({
    x <- input$type_select3
    updatePickerInput(session, "type_select1", selected = x)
    updatePickerInput(session, "type_select2", selected = x)
  })
  observe({
    x <- input$quarter_select1
    updateSliderTextInput(session, "quarter_select2", selected = x)
  })
  observe({
    x <- input$quarter_select2
    updateSliderTextInput(session, "quarter_select1", selected = x)
  })

  
  # Baha - modified 7/6/2021 - additional input to new mapfunction
  output$mapplot <- renderLeaflet({
    mapfunction(input$state_select1,input$county_select1,input$quarter_select1,input$type_select1,input$level_select1)@map
  })
  
  # Baha - added 11/6/2021 - for floating panel in map tab
  output$mean_national <- renderText({
    n_stat <- national_stat(input$quarter_select1,input$type_select1)
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_stat$ave_n,n_rank$unit)
  })
  output$mean_national_desc <- renderText({
    paste0("National Average Overall")
  })
  output$max_national <- renderText({
    n_stat <- national_stat(input$quarter_select1,input$type_select1)
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_stat$max_n,n_rank$unit)
  })
  output$max_national_desc <- renderText({
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_rank$p_direction,"National Average from ",n_rank$first.rank)
  })
  output$min_national <- renderText({
    n_stat <- national_stat(input$quarter_select1,input$type_select1)
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_stat$min_n,n_rank$unit)
  })
  output$min_national_desc <- renderText({
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_rank$n_direction,"National Average from ",n_rank$last$rank)
  })
  output$target_state <- renderText({
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_rank$target_rank)
  })
  output$target_state_desc <- renderText({
    paste0("Rank of ",input$state_select1," in the Country")
  })
  output$mean_target_state <- renderText({
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    paste0(n_rank$target_value,n_rank$unit)
  })
  output$mean_target_state_desc <- renderText({
    paste0("Average Value for ",input$state_select1)
  })
  output$mean_target_district <- renderText({
    n_rank <- get_rank(input$state_select1,input$quarter_select1,input$type_select1)
    if(input$quarter_select1 == "2020 Q1"){
      target <- county_stats_q1_20 %>% filter(laa==input$county_select1)
    }else if(input$quarter_select1 == "Q2"){
      target <- county_stats_q2_20 %>% filter(laa==input$county_select1)
    }else if(input$quarter_select1 == "Q3"){
      target <- county_stats_q3_20 %>% filter(laa==input$county_select1)
    }else if(input$quarter_select1 == "Q4"){
      target <- county_stats_q4_20 %>% filter(laa==input$county_select1)
    }else if(input$quarter_select1 == "2021 Q1"){
      target <- county_stats_q1_21 %>% filter(laa==input$county_select1)
    }
    
    if(input$type_select1=="Download"){
      paste0(round(target$mean_dl_mbps_wt,digits=2),n_rank$unit)
    }else if(input$type_select1=="Upload"){
      paste0(round(target$mean_ul_mbps_wt,digits=2),n_rank$unit)
    }else if(input$type_select1=="Latency"){
      paste0(round(target$mean_lat_ms_wt,digits=2),n_rank$unit)
    }
  })
  
  
  
  output$mean_target_district_desc <- renderText({
    paste0("Average Value for ",input$county_select1)
  })
  
  # Baha - added 13/6/2021 - for table in data search tab
  output$data_table <- DT::renderDataTable(DT::datatable(
    if(input$datatable_select == "State"){
      data <- states_stats
    }else if(input$datatable_select == "District"){
      data <- county_stats
    }
    ))
  
  #PlotHeight = reactive(
  #  return(20*bar_count(input$level_select2, input$state_select2))
  #)
  
  
  output$ranking_plot <- renderPlotly({
    ggplotly(rankFunction(input$state_select2,input$county_select2,input$quarter_select2,input$type_select2,input$level_select2,input$highlight2),
             tooltip = c("y"))
  })
  
  output$line_plot <- renderHighchart({
    validate(need( length(input$line_select3) > 0, "Select at least 1 State/District to see plot"))
    line_plot_function(input$level_select3, input$line_select3, input$state_select3, input$type_select3)
  })
}

shinyApp(ui = ui, server = server)