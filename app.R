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




# Baha - modified 7/6/2021 - comment out read_sf, load input from local database
# mobile_q1_20 <- read_sf("Joined_2020_q1/mobile_q1_2020_join.shp")
# mobile_q2_20 <- read_sf("Joined_2020_q2/mobile_q2_2020_join.shp")
# mobile_q3_20 <- read_sf("Joined_2020_q3/mobile_q3_2020_join.shp")
# mobile_q4_20 <- read_sf("Joined_2020_q4/mobile_q4_2020_join.shp")
# mobile_q1_21 <- read_sf("Joined_2021_q1/mobile_q1_2021_join.shp")
# county <- read_sf("data/polbnda_mys.shp")
load('myProjectEnvironment.RData')

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


# stats by county '20 q1
county_stats_q1_20 <- mobile_q1_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q2
county_stats_q2_20 <- mobile_q2_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q3
county_stats_q3_20 <- mobile_q3_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q4
county_stats_q4_20 <- mobile_q4_20 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '21 q1
county_stats_q1_21 <- mobile_q1_21 %>%
  st_set_geometry(NULL) %>%
  group_by(nam, laa) %>%
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


county_state <- mobile_q1_20 %>%
  as.data.frame()%>%
  select(c('nam','laa')) %>%
  unique() %>%
  .[order(.['nam'],.['laa']),] %>%
  dlply(.(nam))

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
      mapview(tiles[c("avg_lat_ms","avg_d_mbps","avg_u_mbps","tests","devices")],zcol="avg_lat_ms",legend=T,at=seq(0,100,5),layer.name="Average Latency (ms)")
  }
  
  return(show_map)
}

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
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Latency (ms)",title="Mean Latency for different Quarter")
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
        rankplot <- ggplot(rankdf, aes(x=reorder(nam, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=mean_lat_ms_wt)) +
          scale_fill_continuous(high = "darkblue", low = "green")+
          labs(x="",y="Latency (ms)",title="Mean Latency for different Quarter")
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
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=ToHighlight)) +
          scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )+
          labs(x="",y="Latency (ms)",title="Mean Latency for different Quarter")
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
        rankplot <- ggplot(rankdf, aes(x=reorder(laa, -mean_lat_ms_wt),y=mean_lat_ms_wt,fill=mean_lat_ms_wt)) +
          scale_fill_continuous(high = "darkblue", low = "green")+
          labs(x="",y="Latency (ms)",title="Mean Latency for different Quarter")
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

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  theme = shinytheme("sandstone"),
  navbarPage(
    "Mobile Network Analysis",
    
    tabPanel("Map",
             sidebarPanel(width = 2,
                          # Baha - modified 7/6/2021 - add input to select map level
                          selectInput("level_select1", "Map View:",   
                                      choices = c("State", "District"), 
                                      selected = c("District"),
                                      multiple = FALSE),
                          selectInput("state_select1", "State:",   
                                      choices = names(county_state), 
                                      selected = "KEDAH",
                                      multiple = FALSE),
                          selectInput("county_select1", "District:",   
                                      choices = pull(county_state[['KEDAH']],'laa'), 
                                      selected = "LANGKAWI",
                                      multiple = FALSE),
                          selectInput("type_select1", "Type:",   
                                      choices = c("Download", "Upload", "Latency"), 
                                      selected = c("Download"),
                                      multiple = FALSE),
                          shinyWidgets::sliderTextInput("quarter_select1", 
                                                        label = "Quarter:", 
                                                        choices = c("2020 Q1", "Q2", "Q3", "Q4", "2021 Q1"),
                                                        grid = TRUE)
             ),
             mainPanel(width = 10,
                       leafletOutput("mapplot", height= 700)
             )
    ),
    tabPanel("Ranking",
             sidebarPanel(width = 2,
                          selectInput("level_select2", "Level:",   
                                      choices = c("State", "District"), 
                                      selected = c("District"),
                                      multiple = FALSE),
                          selectInput("state_select2", "State:",   
                                      choices = names(county_state), 
                                      selected = c("KEDAH"),
                                      multiple = FALSE),
                          selectInput("county_select2", "District:",   
                                      choices = c("County1", "County2", "County3", "County4"), 
                                      selected = c("County1"),
                                      multiple = FALSE),
                          selectInput("type_select2", "Type:",   
                                      choices = c("Download", "Upload", "Latency"), 
                                      selected = c("Download"),
                                      multiple = FALSE),
                          shinyWidgets::sliderTextInput("quarter_select2", 
                                                        label = "Quarter:", 
                                                        choices = c("2020 Q1", "Q2", "Q3", "Q4", "2021 Q1")),
                          checkboxInput("highlight2",
                                        "Highlight selection",
                                        value = TRUE)
             ),
             mainPanel(width = 10,
                       plotOutput("ranking_plot")
             )         
    ),
    
    tabPanel("Trend",
             sidebarPanel(width = 2,
                          selectInput("level_select3", "Level:",   
                                      choices = c("State", "District"), 
                                      selected = c("District"),
                                      multiple = FALSE),
                          selectInput("state_select3", "State:",   
                                      choices = names(county_state),  
                                      selected = c("KEDAH"),
                                      multiple = FALSE),
                          selectInput("county_select3", "District:",   
                                      choices = c("County1", "County2", "County3", "County4"), 
                                      selected = c("County1"),
                                      multiple = FALSE),
                          selectInput("type_select3", "Type:",   
                                      choices = c("Download", "Upload", "Latency"), 
                                      selected = c("Download"),
                                      multiple = FALSE),
                          shinyWidgets::sliderTextInput("quarter_select3", 
                                                        label = "Quarter:", 
                                                        choices = c("2020 Q1", "Q2", "Q3", "Q4", "2021 Q1"),
                                                        grid = TRUE,
                                                        hide_min_max = TRUE),
                          checkboxInput("highlight3",
                                        "Highlight selection",
                                        value = TRUE)
             ),
             mainPanel(width = 10,
                       "insert render plot"
             )  
    ),
    navbarMenu("Info",
               tabPanel("User Manual"),
               tabPanel("About")
    )
  )
  
)

server <- function(input, output, session) {
  #synchronizing inputs across 3 tabs
  observe({
    x <- input$level_select1
    updateSelectInput(session, "level_select2", selected = x)
    updateSelectInput(session, "level_select3", selected = x)
  })
  observe({
    x <- input$level_select2
    updateSelectInput(session, "level_select1", selected = x)
    updateSelectInput(session, "level_select3", selected = x)
  })
  observe({
    x <- input$level_select3
    updateSelectInput(session, "level_select1", selected = x)
    updateSelectInput(session, "level_select2", selected = x)
  })
  
  observe({
    x <- input$state_select1
    updateSelectInput(session, "state_select2", selected = x)
    updateSelectInput(session, "state_select3", selected = x)
    updateSelectInput(session, "county_select1", choices = pull(county_state[[x]],'laa'))
  })
  observe({
    x <- input$state_select2
    updateSelectInput(session, "state_select1", selected = x)
    updateSelectInput(session, "state_select3", selected = x)
    updateSelectInput(session, "county_select2", choices = pull(county_state[[x]],'laa'))
  })
  observe({
    x <- input$state_select3
    updateSelectInput(session, "state_select1", selected = x)
    updateSelectInput(session, "state_select2", selected = x)
    updateSelectInput(session, "county_select3", choices = pull(county_state[[x]],'laa'))
  })
  
  observe({
    x <- input$county_select1
    updateSelectInput(session, "county_select2", selected = x)
    updateSelectInput(session, "county_select3", selected = x)
  })
  observe({
    x <- input$county_select2
    updateSelectInput(session, "county_select1", selected = x)
    updateSelectInput(session, "county_select3", selected = x)
  })
  observe({
    x <- input$county_select3
    updateSelectInput(session, "county_select1", selected = x)
    updateSelectInput(session, "county_select2", selected = x)
  })
  observe({
    x <- input$type_select1
    updateSelectInput(session, "type_select2", selected = x)
    updateSelectInput(session, "type_select3", selected = x)
  })
  observe({
    x <- input$type_select2
    updateSelectInput(session, "type_select1", selected = x)
    updateSelectInput(session, "type_select3", selected = x)
  })
  observe({
    x <- input$type_select3
    updateSelectInput(session, "type_select1", selected = x)
    updateSelectInput(session, "type_select2", selected = x)
  })
  observe({
    x <- input$quarter_select1
    updateSliderTextInput(session, "quarter_select2", selected = x)
    updateSliderTextInput(session, "quarter_select3", selected = x)
  })
  observe({
    x <- input$quarter_select2
    updateSliderTextInput(session, "quarter_select1", selected = x)
    updateSliderTextInput(session, "quarter_select3", selected = x)
  })
  observe({
    x <- input$quarter_select3
    updateSliderTextInput(session, "quarter_select1", selected = x)
    updateSliderTextInput(session, "quarter_select2", selected = x)
  })
  observe({
    x <- input$highlight2
    updateCheckboxInput(session, "highlight3", value = x)
  })
  observe({
    x <- input$highlight3
    updateCheckboxInput(session, "highlight2", value = x)
  })
  
  # Baha - modified 7/6/2021 - additional input to new mapfunction
  output$mapplot <- renderLeaflet({
    mapfunction(input$state_select1,input$county_select1,input$quarter_select1,input$type_select1,input$level_select1)@map
  })
  
  output$ranking_plot <- renderPlot(
    rankFunction(input$state_select2,input$county_select2,input$quarter_select2,input$type_select2,input$level_select2,input$highlight2)
  )
}

shinyApp(ui = ui, server = server)