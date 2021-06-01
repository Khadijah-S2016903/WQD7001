library(mapview) # to plot tiles in map
library(sf) # to read shapefile .shp
library(ggplot2) # to plot other graphs
library(dplyr) # for stats
library(RColorBrewer) # for color palette
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(leaflet)


mobile_q1_20 <- read_sf("Joined_2020_q1/mobile_q1_2020_join.shp")
mobile_q2_20 <- read_sf("Joined_2020_q2/mobile_q2_2020_join.shp")
mobile_q3_20 <- read_sf("Joined_2020_q3/mobile_q3_2020_join.shp")
mobile_q4_20 <- read_sf("Joined_2020_q4/mobile_q4_2020_join.shp")
mobile_q1_21 <- read_sf("Joined_2021_q1/mobile_q1_2021_join.shp")
county <- read_sf("data/polbnda_mys.shp")

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
  group_by(laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q2
county_stats_q2_20 <- mobile_q2_20 %>%
  st_set_geometry(NULL) %>%
  group_by(laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q3
county_stats_q3_20 <- mobile_q3_20 %>%
  st_set_geometry(NULL) %>%
  group_by(laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '20 q4
county_stats_q4_20 <- mobile_q4_20 %>%
  st_set_geometry(NULL) %>%
  group_by(laa) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests))

# stats by county '21 q1
county_stats_q1_21 <- mobile_q1_21 %>%
  st_set_geometry(NULL) %>%
  group_by(laa) %>%
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


county_state <- county %>%
  as.data.frame()%>%
  select(c('nam','laa')) %>%
  unique() %>%
  .[order(.['nam'],.['laa']),] %>%
  dlply(.(nam))


mapfunction <- function(state_name){
  state_tiles <- mobile_q1_21[mobile_q1_21$nam == state_name,]
  state_polygon <- county[county$nam == state_name,]
  return(mapview(state_tiles["avg_d_kbps"],legend=T, at=seq(150000,0,-10000),layer.name="Average download (kbps)", map.types = "OpenStreetMap", zcol="avg_d_kbps") +
           mapview(state_polygon, alpha.regions = 0.1, alpha = 1))
}

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  theme = shinytheme("sandstone"),
  navbarPage(
    "Mobile Network Analysis",
    
    tabPanel("Map",
              sidebarPanel(width = 2,
                          selectInput("state_select1", "State:",   
                                      choices = names(county_state), 
                                      selected = "JOHOR",
                                      multiple = FALSE),
                          selectInput("county_select1", "County:",   
                                      choices = pull(county_state[['JOHOR']],'laa'), 
                                      selected = "MUAR",
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
                         selectInput("state_select2", "State:",   
                                     choices = names(county_state), 
                                     selected = c("JOHOR"),
                                     multiple = FALSE),
                         selectInput("county_select2", "County:",   
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
                       "insert render plot"
             )         
    ),
    
    tabPanel("Trend",
             sidebarPanel(width = 2,
                          selectInput("state_select3", "State:",   
                                      choices = names(county_state),  
                                      selected = c("JOHOR"),
                                      multiple = FALSE),
                          selectInput("county_select3", "County:",   
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
                                        value = TRUE),
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
  
  
  output$mapplot <- renderLeaflet({
    mapfunction(input$state_select1)@map
  })
}

shinyApp(ui = ui, server = server)