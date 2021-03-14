library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(DT)
library(leaflet)


df <- read_excel("egrid2018_data_v2.xlsx", sheet=4)
df.2000 <- read_excel("eGRID2000_plant.xls", sheet=2)
df.2010 <- read_excel("eGRID2010_Data.xls", sheet=4)

sources <- c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL","OTHER")
renewable_sources <- c("HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL")
non_renewable_sources <- c("COAL","OIL","GAS","NUCLEAR","OTHER")

sources_percent <- list()
for(i in 1:9){sources_percent <- c(sources_percent,paste(sources[i],"_PERCENT",sep=""))}
unlist(sources_percent)

process_2018 <- function(df){
  # Renaming/dropping variables, arranging orders (2018)
  for(i in 6:14){names(df)[i] <- sources[i-5]}
  df$OTHER <- df[15] + df[16]
  df$TOTAL <- df[17] + df[18]
  df$OTHER <- df$OTHER$`Plant annual other fossil net generation (MWh)`
  df$TOTAL <- df$TOTAL$`Plant annual total nonrenewables net generation (MWh)`
  names(df)[17] <- "TOTAL_NON-RENEWABLE"
  names(df)[18] <- "TOTAL_RENEWABLE"
  
  for(i in 19:27){names(df)[i] <- sources_percent[i-18]}
  df$OTHER_PERCENT <- df[28] + df[29]
  df$OTHER_PERCENT <- df$OTHER_PERCENT$`Plant other fossil generation percent (resource mix)`
  names(df)[30] <- "TOTAL_NON-RENEWABLE_PERCENT"
  df$`TOTAL_NON-RENEWABLE_PERCENT` <- df$`TOTAL_NON-RENEWABLE_PERCENT`*100
  names(df)[31] <- "TOTAL_RENEWABLE_PERCENT"
  df$`TOTAL_RENEWABLE_PERCENT` <- df$`TOTAL_RENEWABLE_PERCENT`*100
  names(df)[32] <- "ORIS"
  
  df <- df[-c(15:16,28:29)]
  df <- df[,c(1:14,29,15:16,30,17:25,31,26:27,28)]
  return(df)
}

process_2000 <- function(df.2000){
  # Renaming/dropping variables, arranging orders (2000)
  names(df.2000)[2] <- "Plant state abbreviation"
  names(df.2000)[3] <- "Plant name"
  names(df.2000)[4] <- "Plant latitude"
  df.2000[4] <- as.numeric(df.2000$`Plant latitude`)
  names(df.2000)[5] <- "Plant longitude"
  df.2000[5] <- -as.numeric(df.2000$`Plant longitude`)
  names(df.2000)[6] <- "TOTAL"
  for(i in 7:16){names(df.2000)[i] <- sources[i-6]}
  names(df.2000)[17] <- "TOTAL_NON-RENEWABLE"
  names(df.2000)[18] <- "TOTAL_RENEWABLE"
  
  for(i in 19:27){names(df.2000)[i] <- sources_percent[i-18]}
  names(df.2000)[28] <-"OTHER_PERCENT"
  names(df.2000)[29] <- "TOTAL_NON-RENEWABLE_PERCENT"
  names(df.2000)[30] <- "TOTAL_RENEWABLE_PERCENT"
  names(df.2000)[31] <- "ORIS"
  df.2000[31] <- df.2000$ORIS
  df.2000 <- df.2000[,c(1:5,7:18,6,19:31)]
  return(df.2000)
}

process_2010 <- function(df.2010){
  # Renaming/dropping variables, arranging orders (2010)
  sources <- c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL","OTHER")
  renewable_sources <- c("HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL")
  non_renewable_sources <- c("COAL","OIL","GAS","NUCLEAR","OTHER")
  names(df.2010)[6] <- "TOTAL"
  for(i in 7:15){names(df.2010)[i] <- sources[i-6]}
  df.2010$OTHER <- df.2010[16] + df.2010[17]
  df.2010$OTHER <- df.2010$OTHER$`Plant annual other fossil net generation (MWh)`
  names(df.2010)[18] <- "TOTAL_NON-RENEWABLE"
  names(df.2010)[19] <- "TOTAL_RENEWABLE"
  
  for(i in 20:28){names(df.2010)[i] <- sources_percent[i-19]}
  df.2010$OTHER_PERCENT <- df.2010[29] + df.2010[30]
  df.2010$OTHER_PERCENT <- df.2010$OTHER_PERCENT$`Plant other fossil generation percent (resource mix)`
  names(df.2010)[31] <- "TOTAL_NON-RENEWABLE_PERCENT"
  names(df.2010)[32] <- "TOTAL_RENEWABLE_PERCENT"
  names(df.2010)[33] <- "ORIS"
  
  df.2010 <- df.2010[-c(16:17,29:30)]
  df.2010 <- df.2010[,c(1:5,7:15,30,16:17,6,18:26,31,27:28,29)]
  return(df.2010)
}

df <- process_2018(df)
df.2000 <- process_2000(df.2000)
df.2010 <- process_2010(df.2010)


# Factor by conditions
f_state_abb <- levels(as.factor(df$`Plant state abbreviation`))
f_state <- state.name[match(f_state_abb,state.abb)]

# Default (Illinois)
df.illinois <- subset(df, df$`Plant state abbreviation`=='IL')
df.2000.illinois <- subset(df.2000, df.2000$`Plant state abbreviation`=='IL')
df.2010.illinois <- subset(df.2010, df.2010$`Plant state abbreviation`=='IL')

# Other state & year
df_list <- list()
df.2000_list <- list()
df.2010_list <- list()
for(i in 1:length(f_state_abb)){
  df_list <- c(df_list, list(subset(df, df$`Plant state abbreviation`==f_state_abb[i])))
  df.2000_list <- c(df.2000_list, list(subset(df.2000, df.2000$`Plant state abbreviation`==f_state_abb[i])))
  df.2010_list <- c(df.2010_list, list(subset(df.2010, df.2010$`Plant state abbreviation`==f_state_abb[i])))
}

color_code <- c("#1b9e77","#d95f02","#7570b3","#a6cee3","#1f78b4","#b2df8a","#994F00","#E1BE6A","#DC3220","#D35FB7")
pal <- colorFactor(color_code, domain = sources)


# Shiny Interface
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  
  # Sidebar start
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("About", tabName = "about"),
                     menuItem("Visualization", tabName = "visualization"),
                     menuItem("Comparison", tabName = "comparison"),
                     menuItem("Entire US", tabName = "us"),
                     menuItem("Added / Idled", tabName = "addidle")
                   )
  ),
  # Sidebar end
  
  # Body start
  dashboardBody(
    tabItems(
      # "About" Tab
      tabItem(tabName = "about",
              h2("About"),
              p("This project focuses on the visualization of data using the electrical power generation in the US dataset.
                Important information such as the name, geographic position, energy source, amount and percentage of the power plant are recorded.
                Based on the actual geographical positions recorded, the physical location of the power plant can be seen mapped for comparison purposes.
                The original data is available from",
                a("https://www.epa.gov/egrid/download-data"),
                ".",
                style = "font-family : sans-serif"),
              p("The app is written by Eakasorn Suraminitkul and uses the data from 2000, 2010 and 2018.",
                style = "font-family : sans-serif"),
      ),
      
      # "Visualization" Tab
      tabItem(tabName = "visualization",
                 h2("Power plants in Illinois (2018)"),
                 
                 fluidRow(
                   fluidRow(
                     column(1,
                            checkboxInput("c1", "Coal", FALSE)
                     ),
                     column(1,
                            checkboxInput("c2", "Oil", FALSE)
                     ),
                     column(1,
                            checkboxInput("c3", "Gas", FALSE)
                     ),
                     column(1,
                            checkboxInput("c4", "Nuclear", FALSE)
                     ),
                     column(1,
                            checkboxInput("c5", "Hydro", FALSE)
                     ),
                     column(1,
                            checkboxInput("c6", "Biomass", FALSE)
                     ),
                     column(1,
                            checkboxInput("c7", "Wind", FALSE)
                     ),
                     column(1,
                            checkboxInput("c8", "Solar", FALSE)
                     ),
                     column(1,
                            checkboxInput("c9", "Geothermal", FALSE)
                     ),
                     column(1,
                            checkboxInput("c10", "Other", FALSE)
                     )
                   ),
                   fluidRow(
                     column(1,
                            checkboxInput("c11", "Renewable", FALSE)
                     ),
                     column(1,
                            checkboxInput("c12", "Non-renewable", FALSE)
                     ),
                     column(1,
                          checkboxInput("c13", "All", TRUE)
                     ),
                     column(1,
                            actionButton("reset", "Reset")
                     ),
                     column(2,
                            h5("Click reset to display or reset view")
                     )
                   )
                 ),
                 
                fluidRow(
                  box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                      leafletOutput("leaf", height = 550)
                  )
                )

      ),
      
      # "Comparison" Tab
      tabItem(tabName = "comparison",
              h2("Power plants in the U.S. (2000, 2010, 2018)"),
              
              # Independent left checkbox
              column(6,
                fluidRow(
                  fluidRow(
                    column(1,
                           checkboxInput("l1", "Coal", FALSE)
                    ),
                    column(1,
                           checkboxInput("l2", "Oil", FALSE)
                    ),
                    column(1,
                           checkboxInput("l3", "Gas", FALSE)
                    ),
                    column(1,
                           checkboxInput("l4", "Nuclear", FALSE)
                    ),
                    column(1,
                           checkboxInput("l5", "Hydro", FALSE)
                    ),
                    column(1,
                           checkboxInput("l6", "Biomass", FALSE)
                    ),
                    column(1,
                           checkboxInput("l7", "Wind", FALSE)
                    ),
                    column(1,
                           checkboxInput("l8", "Solar", FALSE)
                    )
                  ),
                  fluidRow(
                    column(2,
                           checkboxInput("l9", "Geothermal", FALSE)
                    ),
                    column(1,
                           checkboxInput("l10", "Other", FALSE)
                    ),
                    column(2,
                           checkboxInput("l11", "Renewable", FALSE)
                    ),
                    column(2,
                           checkboxInput("l12", "Non-renewable", FALSE)
                    ),
                    column(1,
                           checkboxInput("l13", "All", TRUE)
                    ),
                    column(1,
                           actionButton("lreset", "Reset")
                    )
                    
                    
                  )
                )
              ),
              
              # Independent right checkbox
              column(6,
                     fluidRow(
                       fluidRow(
                         column(1,
                                checkboxInput("r1", "Coal", FALSE)
                         ),
                         column(1,
                                checkboxInput("r2", "Oil", FALSE)
                         ),
                         column(1,
                                checkboxInput("r3", "Gas", FALSE)
                         ),
                         column(1,
                                checkboxInput("r4", "Nuclear", FALSE)
                         ),
                         column(1,
                                checkboxInput("r5", "Hydro", FALSE)
                         ),
                         column(1,
                                checkboxInput("r6", "Biomass", FALSE)
                         ),
                         column(1,
                                checkboxInput("r7", "Wind", FALSE)
                         ),
                         column(1,
                                checkboxInput("r8", "Solar", FALSE)
                         )
                       ),
                       fluidRow(
                         column(2,
                                checkboxInput("r9", "Geothermal", FALSE)
                         ),
                         column(1,
                                checkboxInput("r10", "Other", FALSE)
                         ),
                         column(2,
                                checkboxInput("r11", "Renewable", FALSE)
                         ),
                         column(2,
                                checkboxInput("r12", "Non-renewable", FALSE)
                         ),
                         column(1,
                                checkboxInput("r13", "All", TRUE)
                         ),
                         column(1,
                                actionButton("rreset", "Reset")
                         )
                         
                         
                       )
                     )
              ),
              
              # Year and State
              fluidRow(
                column(6,
                  column(2,
                         selectInput("lyear", "Year",       
                                     c("2000","2010","2018"), selected="2000")
                  ),
                  column(2,
                         selectInput("lstate", "Select a state",       
                                     f_state, selected="Illinois")
                  ),
                  column(2,
                         selectInput("lmaptype", "Map type",       
                                     c("Default","1","2"), selected="Default")
                  ),
                  column(2,
                         h5("Click reset to display or reset view")
                  )
                ),
                column(6,
                  column(2,
                         selectInput("ryear", "Year",       
                                     c("2000","2010","2018"), selected="2018")
                  ),
                  column(2,
                         selectInput("rstate", "Select a state",       
                                     f_state, selected="Illinois")
                  ),
                  column(2,
                         selectInput("rmaptype", "Map type",       
                                     c("Default","1","2"), selected="Default")
                  )
                )
              ),
              
              # Left and right leaflet map
              fluidRow(
                column(6,
                  box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                      leafletOutput("lleaf", height = 500)
                  )
                ),
                column(6,
                       box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                           leafletOutput("rleaf", height = 500)
                       )
                )
              ),
              
              # Linked checkbox
              fluidRow(
                fluidRow(
                  column(1,
                         checkboxInput("m1", "Coal", FALSE)
                  ),
                  column(1,
                         checkboxInput("m2", "Oil", FALSE)
                  ),
                  column(1,
                         checkboxInput("m3", "Gas", FALSE)
                  ),
                  column(1,
                         checkboxInput("m4", "Nuclear", FALSE)
                  ),
                  column(1,
                         checkboxInput("m5", "Hydro", FALSE)
                  ),
                  column(1,
                         checkboxInput("m6", "Biomass", FALSE)
                  ),
                  column(1,
                         checkboxInput("m7", "Wind", FALSE)
                  ),
                  column(1,
                         checkboxInput("m8", "Solar", FALSE)
                  ),
                  column(1,
                         checkboxInput("m9", "Geothermal", FALSE)
                  ),
                  column(1,
                         checkboxInput("m10", "Other", FALSE)
                  )
                ),
                fluidRow(
                  column(1,
                         checkboxInput("m11", "Renewable", FALSE)
                  ),
                  column(1,
                         checkboxInput("m12", "Non-renewable", FALSE)
                  ),
                  column(1,
                         checkboxInput("m13", "All", TRUE)
                  )
                )
              )              
              
              
      ),
      
      # "Entire U.S." Tab
      tabItem(tabName = "us",
              h2("Power plants in the U.S. (2000, 2010, 2018)"),
              
              # Independent checkbox
              fluidRow(
               fluidRow(
                 column(1,
                        checkboxInput("us1", "Coal", FALSE)
                 ),
                 column(1,
                        checkboxInput("us2", "Oil", FALSE)
                 ),
                 column(1,
                        checkboxInput("us3", "Gas", FALSE)
                 ),
                 column(1,
                        checkboxInput("us4", "Nuclear", FALSE)
                 ),
                 column(1,
                        checkboxInput("us5", "Hydro", FALSE)
                 ),
                 column(1,
                        checkboxInput("us6", "Biomass", FALSE)
                 ),
                 column(1,
                        checkboxInput("us7", "Wind", FALSE)
                 ),
                 column(1,
                        checkboxInput("us8", "Solar", FALSE)
                 ),
                 column(1,
                        checkboxInput("us9", "Geothermal", FALSE)
                 ),
                 column(1,
                        checkboxInput("us10", "Other", FALSE)
                 )
               ),
               fluidRow(
                 column(1,
                        checkboxInput("us11", "Renewable", FALSE)
                 ),
                 column(1,
                        checkboxInput("us12", "Non-renewable", FALSE)
                 ),
                 column(1,
                        checkboxInput("us13", "All", FALSE)
                 ),
                 column(1,
                        actionButton("usreset", "Reset")
                 ),
                 column(1,
                        selectInput("usyear", "Year",       
                                    c("2000","2010","2018"), selected="2018")
                 ),
                 column(1,
                        selectInput("usstate", "Select a state",       
                                    c(f_state,"Entire US"), selected="Entire US")
                 ),
                 column(3,sliderInput("min_slider","Above (Min)",-3000000,32000000,0)),
                 column(3,sliderInput("max_slider","Below (Max)",-3000000,32000000,1000))
                 
               ),
               
               fluidRow(
                 box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                     leafletOutput("usleaf", height = 550)
                 )
               )
               
               
              )
              
      ),
      
      # "Added / Idled" Tab
      tabItem(tabName = "addidle",
              h2("Power plants added/idled over the years"),
              
              # Checkbox
              fluidRow(
                column(1,
                       selectInput("byear", "Year (Before)",       
                                   c("2000","2010","2018"), selected="2000")
                ),
                column(1,
                       selectInput("ayear", "Year (After)",       
                                   c("2000","2010","2018"), selected="2000")
                )
              ),
              
              fluidRow(
                box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                    leafletOutput("addleaf", height = 550)
                )
              )

      )
      
    )
  )
)


server <- function(input, output) {
  cc1  <- renderText({c1  = ifelse(input$c1, sources[1], "")})
  cc2  <- renderText({c2  = ifelse(input$c2, sources[2], "")})
  cc3  <- renderText({c3  = ifelse(input$c3, sources[3], "")})
  cc4  <- renderText({c4  = ifelse(input$c4, sources[4], "")})
  cc5  <- renderText({c5  = ifelse(input$c5, sources[5], "")})
  cc6  <- renderText({c6  = ifelse(input$c6, sources[6], "")})
  cc7  <- renderText({c7  = ifelse(input$c7, sources[7], "")})
  cc8  <- renderText({c8  = ifelse(input$c8, sources[8], "")})
  cc9  <- renderText({c9  = ifelse(input$c9, sources[9], "")})
  cc10 <- renderText({c10 = ifelse(input$c10, sources[10], "")})
  cc_list <- reactive(c(cc1(),cc2(),cc3(),cc4(),cc5(),cc6(),cc7(),cc8(),cc9(),cc10()))

  # Visualization tab leaflet
  output$leaf <- renderLeaflet({
    # Initial set to Illinois state
    map <- leaflet() %>%
           addTiles() %>%
           setView(lng = -89.3, lat = 40.2, zoom = 6)
    # ----------------------------------------------------
    # [COAL,OIL,GAS,NUCLEAR,HYDRO,BIOMASS,WIND,SOLAR,GEOTHERMAL,OTHER]
    #   6              9                   12                    15 
    
    temp_range <- max(subset(df$TOTAL,is.na(df$TOTAL)==FALSE)) - min(subset(df$TOTAL,is.na(df$TOTAL)==FALSE))
    
    if(input$c13==TRUE) {
      cc_list <- reactive(c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL","OTHER"))
      for(i in 6:15){
        if(cc_list()[i-5]!=""){
          temp_df <- subset(df.illinois[,c(3:5,i,29:30)], df.illinois[i]!=as.double(0) | is.na(df.illinois[i]))
          map <- addCircleMarkers(map, lng = temp_df$`Plant longitude`, lat = temp_df$`Plant latitude`,
                                  popup = paste(temp_df$`Plant name`,"Capacity:",temp_df[[4]],"Renewable:",temp_df[[6]],"% Non-renewable:",temp_df[[5]],"%"),
                                  color = color_code[i-5], radius = (temp_df[[4]]/temp_range)*30 +5 , fill = FALSE) 
        }
      }
    }
    else{
      # Renewable ["HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL"]
      if(input$c11==TRUE){
        cc_list <- reactive(c(cc1(),cc2(),cc3(),cc4(),"HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL",cc10()))
        for(i in 10:14){
          if(cc_list()[i-5]!=""){
            temp_df <- subset(df.illinois[,c(3:5,i,29:30)], df.illinois[i]!=as.double(0) | is.na(df.illinois[i]))
            map <- addCircleMarkers(map, lng = temp_df$`Plant longitude`, lat = temp_df$`Plant latitude`,
                                    popup = paste(temp_df$`Plant name`,"Capacity:",temp_df[[4]],"Renewable:",temp_df[[6]],"% Non-renewable:",temp_df[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df[[4]]/temp_range)*30 +5 , fill = FALSE) 
          }
        }
      }

      # Non-renewable ["COAL","OIL","GAS","NUCLEAR","OTHER"]
      if(input$c12==TRUE){
        cc_list <- reactive(c("COAL","OIL","GAS","NUCLEAR",cc5(),cc6(),cc7(),cc8(),cc9(),"OTHER"))
        for(i in 6:9){
          if(cc_list()[i-5]!=""){
            temp_df <- subset(df.illinois[,c(3:5,i,29:30)], df.illinois[i]!=as.double(0) | is.na(df.illinois[i]))
            map <- addCircleMarkers(map, lng = temp_df$`Plant longitude`, lat = temp_df$`Plant latitude`,
                                    popup = paste(temp_df$`Plant name`,"Capacity:",temp_df[[4]],"Renewable:",temp_df[[6]],"% Non-renewable:",temp_df[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df[[4]]/temp_range)*30 +5 , fill = FALSE) 
          }
        }
            
        if(cc_list()[10]!=""){
          temp_df <- subset(df.illinois[,c(3:5,15,29,30)], df.illinois[15]!=as.double(0) | is.na(df.illinois[15]))
          map <- addCircleMarkers(map, lng = temp_df$`Plant longitude`, lat = temp_df$`Plant latitude`,
                                  popup = paste(temp_df$`Plant name`,"Capacity:",temp_df[[4]],"Renewable:",temp_df[[6]],"% Non-renewable:",temp_df[[5]],"%"),
                                  color = color_code[10], radius = (temp_df[[4]]/temp_range)*30 +5 , fill = FALSE) 
        }
      }
      
      else{
        for(i in 1:10){
          if(cc_list()[i]!=""){
            temp_df <- subset(df.illinois[,c(3:5,i+5,29,30)], df.illinois[i+5]!=as.double(0) | is.na(df.illinois[i+5]))
            map <- addCircleMarkers(map, lng = temp_df$`Plant longitude`, lat = temp_df$`Plant latitude`,
                                    popup = paste(temp_df$`Plant name`,"Capacity:",temp_df[[4]],"Renewable:",temp_df[[6]],"% Non-renewable:",temp_df[[5]],"%"),
                                    color = color_code[i], radius = (temp_df[[4]]/temp_range)*30 +5 , fill = FALSE) 
          }
        }
      }
      
    }
    map <- addLegend(map,"bottomright", colors = color_code, labels = sources,
              title = "Status",
              opacity = 1)
    map
    if(input$reset){
      map1 <-reactive(setView(map, lng = -89.3, lat = 40.2, zoom = 6))
      map1()
    }
  })
  
  ccl1  <- renderText({c1  = ifelse(input$l1, sources[1], "")})
  ccl2  <- renderText({c2  = ifelse(input$l2, sources[2], "")})
  ccl3  <- renderText({c3  = ifelse(input$l3, sources[3], "")})
  ccl4  <- renderText({c4  = ifelse(input$l4, sources[4], "")})
  ccl5  <- renderText({c5  = ifelse(input$l5, sources[5], "")})
  ccl6  <- renderText({c6  = ifelse(input$l6, sources[6], "")})
  ccl7  <- renderText({c7  = ifelse(input$l7, sources[7], "")})
  ccl8  <- renderText({c8  = ifelse(input$l8, sources[8], "")})
  ccl9  <- renderText({c9  = ifelse(input$l9, sources[9], "")})
  ccl10 <- renderText({c10 = ifelse(input$l10, sources[10], "")})
  cc_llist <- reactive(c(ccl1(),ccl2(),ccl3(),ccl4(),ccl5(),ccl6(),ccl7(),ccl8(),ccl9(),ccl10()))
  
  ccr1  <- renderText({c1  = ifelse(input$r1, sources[1], "")})
  ccr2  <- renderText({c2  = ifelse(input$r2, sources[2], "")})
  ccr3  <- renderText({c3  = ifelse(input$r3, sources[3], "")})
  ccr4  <- renderText({c4  = ifelse(input$r4, sources[4], "")})
  ccr5  <- renderText({c5  = ifelse(input$r5, sources[5], "")})
  ccr6  <- renderText({c6  = ifelse(input$r6, sources[6], "")})
  ccr7  <- renderText({c7  = ifelse(input$r7, sources[7], "")})
  ccr8  <- renderText({c8  = ifelse(input$r8, sources[8], "")})
  ccr9  <- renderText({c9  = ifelse(input$r9, sources[9], "")})
  ccr10 <- renderText({c10 = ifelse(input$r10, sources[10], "")})
  cc_rlist <- reactive(c(ccr1(),ccr2(),ccr3(),ccr4(),ccr5(),ccr6(),ccr7(),ccr8(),ccr9(),ccr10()))
  
  ccm1  <- renderText({c1  = ifelse(input$m1, sources[1], "")})
  ccm2  <- renderText({c2  = ifelse(input$m2, sources[2], "")})
  ccm3  <- renderText({c3  = ifelse(input$m3, sources[3], "")})
  ccm4  <- renderText({c4  = ifelse(input$m4, sources[4], "")})
  ccm5  <- renderText({c5  = ifelse(input$m5, sources[5], "")})
  ccm6  <- renderText({c6  = ifelse(input$m6, sources[6], "")})
  ccm7  <- renderText({c7  = ifelse(input$m7, sources[7], "")})
  ccm8  <- renderText({c8  = ifelse(input$m8, sources[8], "")})
  ccm9  <- renderText({c9  = ifelse(input$m9, sources[9], "")})
  ccm10 <- renderText({c10 = ifelse(input$m10, sources[10], "")})
  cc_mlist <- reactive(c(ccm1(),ccm2(),ccm3(),ccm4(),ccm5(),ccm6(),ccm7(),ccm8(),ccm9(),ccm10()))
  
  # Comparison tab left leaflet
  output$lleaf <- renderLeaflet({
    # Initial set to Illinois state
    map <- leaflet() %>%
           addTiles() %>%
           setView(lng = -89.3, lat = 40.2, zoom = 6)
    # ----------------------------------------------------
    # [COAL,OIL,GAS,NUCLEAR,HYDRO,BIOMASS,WIND,SOLAR,GEOTHERMAL,OTHER]
    #   6              9                   12                    15 
    
    
    temp_df_list.l <- reactive(switch(input$lyear,
                                       "2000" = df.2000_list,
                                       "2010" = df.2010_list,
                                       "2018" = df_list))
    temp_df.l <- reactive({
        temp_df_list.l()[[match(input$lstate,f_state)]]
    })
    
    temp_range.l <- reactive(max(subset(temp_df.l()$TOTAL,is.na(temp_df.l()$TOTAL)==FALSE)) - min(subset(temp_df.l()$TOTAL,is.na(temp_df.l()$TOTAL)==FALSE)))
    
    if(input$l13==TRUE | input$m13==TRUE) {
      cc_llist <- reactive(c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL","OTHER"))
      for(i in 6:15){
        if(cc_llist()[i-5]!=""){
          temp_df.l1 <- reactive(subset(temp_df.l()[,c(3:5,i,29,30)], temp_df.l()[i]!=as.double(0) | is.na(temp_df.l()[i])))
          map <- addCircleMarkers(map, lng = temp_df.l1()$`Plant longitude`, lat = temp_df.l1()$`Plant latitude`,
                                  popup = paste(temp_df.l1()$`Plant name`,"Capacity:",temp_df.l1()[[4]],"Renewable:",temp_df.l1()[[6]],"% Non-renewable:",temp_df.l1()[[5]],"%"),
                                  color = color_code[i-5], radius = (temp_df.l1()[[4]]/temp_range.l())*30 +5 , fill = FALSE) 
        }
      }
    }
    else{
      # Renewable ["HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL"]
      if(input$l11==TRUE | input$m11==TRUE){
        cc_llist <- reactive(c(ccl1(),ccl2(),ccl3(),ccl4(),"HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL",ccl10()))
        for(i in 10:14){
          if(cc_llist()[i-5]!=""){
            temp_df.l1 <- reactive(subset(temp_df.l()[,c(3:5,i,29,30)], temp_df.l()[i]!=as.double(0) | is.na(temp_df.l()[i])))
            map <- addCircleMarkers(map, lng = temp_df.l1()$`Plant longitude`, lat = temp_df.l1()$`Plant latitude`,
                                    popup = paste(temp_df.l1()$`Plant name`,"Capacity:",temp_df.l1()[[4]],"Renewable:",temp_df.l1()[[6]],"% Non-renewable:",temp_df.l1()[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df.l1()[[4]]/temp_range.l())*30 +5 , fill = FALSE) 
          }     
        }
      }

      # Non-renewable ["COAL","OIL","GAS","NUCLEAR","OTHER"]
      if(input$l12==TRUE | input$m12==TRUE){
        cc_llist <- reactive(c("COAL","OIL","GAS","NUCLEAR",ccl5(),ccl6(),ccl7(),ccl8(),ccl9(),"OTHER"))
        for(i in 6:9){
          if(cc_llist()[i-5]!=""){
            temp_df.l1 <- reactive(subset(temp_df.l()[,c(3:5,i,29,30)], temp_df.l()[i]!=as.double(0) | is.na(temp_df.l()[i])))
            map <- addCircleMarkers(map, lng = temp_df.l1()$`Plant longitude`, lat = temp_df.l1()$`Plant latitude`,
                                    popup = paste(temp_df.l1()$`Plant name`,"Capacity:",temp_df.l1()[[4]],"Renewable:",temp_df.l1()[[6]],"% Non-renewable:",temp_df.l1()[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df.l1()[[4]]/temp_range.l())*30 +5 , fill = FALSE) 
          }
        }
        if(cc_llist()[10]!=""){
          temp_df.l1 <- reactive(subset(temp_df.l()[,c(3:5,15,29,30)], temp_df.l()[15]!=as.double(0) | is.na(temp_df.l()[15])))
          map <- addCircleMarkers(map, lng = temp_df.l1()$`Plant longitude`, lat = temp_df.l1()$`Plant latitude`,
                                  popup = paste(temp_df.l1()$`Plant name`,"Capacity:",temp_df.l1()[[4]],"Renewable:",temp_df.l1()[[6]],"% Non-renewable:",temp_df.l1()[[5]],"%"),
                                  color = color_code[10], radius = (temp_df.l1()[[4]]/temp_range.l())*30 +5 , fill = FALSE) 
          
        }
      }

      else{
        for(i in 1:10){
          if(cc_llist()[i]!="" | cc_mlist()[i]!=""){
            temp_df.l1 <- reactive(subset(temp_df.l()[,c(3:5,i+5,29,30)], temp_df.l()[i+5]!=as.double(0) | is.na(temp_df.l()[i+5])))
            map <- addCircleMarkers(map, lng = temp_df.l1()$`Plant longitude`, lat = temp_df.l1()$`Plant latitude`,
                                    popup = paste(temp_df.l1()$`Plant name`,"Capacity:",temp_df.l1()[[4]],"Renewable:",temp_df.l1()[[6]],"% Non-renewable:",temp_df.l1()[[5]],"%"),
                                    color = color_code[i], radius = (temp_df.l1()[[4]]/temp_range.l())*30 +5 , fill = FALSE) 
            
          }
        }
      }

    }
    map <- addLegend(map,"bottomright", colors = color_code, labels = sources,
                     title = "Status",
                     opacity = 1)
    map
    if(input$lreset){
      map1 <-reactive(setView(map, lng = -89.3, lat = 40.2, zoom = 6))
      map1()
    }
    map2 <- reactive(switch (input$lmaptype,
      "Default" = addTiles(map),
      "1" = addProviderTiles(map, providers$Stamen.Toner),
      "2" = addProviderTiles(map,providers$Esri.NatGeoWorldMap)
      )
    )
    map2()
  })
  
  # Comparison tab right leaflet
  output$rleaf <- renderLeaflet({
    # Initial set to Illinois state
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -89.3, lat = 40.2, zoom = 6)
    # ----------------------------------------------------
    # [COAL,OIL,GAS,NUCLEAR,HYDRO,BIOMASS,WIND,SOLAR,GEOTHERMAL,OTHER]
    #   6              9                   12                    15 
    
    
    temp_df_list.r <- reactive(switch(input$ryear,
                                      "2000" = df.2000_list,
                                      "2010" = df.2010_list,
                                      "2018" = df_list))
    temp_df.r <- reactive({
      temp_df_list.r()[[match(input$rstate,f_state)]]
    })
    
    temp_range.r <- reactive(max(subset(temp_df.r()$TOTAL,is.na(temp_df.r()$TOTAL)==FALSE)) - min(subset(temp_df.r()$TOTAL,is.na(temp_df.r()$TOTAL)==FALSE)))
    
    if(input$r13==TRUE | input$m13==TRUE) {
      cc_rlist <- reactive(c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL","OTHER"))
      for(i in 6:15){
        if(cc_rlist()[i-5]!=""){
          temp_df.r1 <- reactive(subset(temp_df.r()[,c(3:5,i,29,30)], temp_df.r()[i]!=as.double(0) | is.na(temp_df.r()[i])))
          map <- addCircleMarkers(map, lng = temp_df.r1()$`Plant longitude`, lat = temp_df.r1()$`Plant latitude`,
                                  popup = paste(temp_df.r1()$`Plant name`,"Capacity:",temp_df.r1()[[4]],"Renewable:",temp_df.r1()[[6]],"% Non-renewable:",temp_df.r1()[[5]],"%"),
                                  color = color_code[i-5], radius = (temp_df.r1()[[4]]/temp_range.r())*30 +5 , fill = FALSE) 
        }
      }
    }
    else{
      # Renewable ["HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL"]
      if(input$r11==TRUE | input$m11==TRUE){
        cc_rlist <- reactive(c(ccr1(),ccr2(),ccr3(),ccr4(),"HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL",ccr10()))
        for(i in 10:14){
          if(cc_rlist()[i-5]!=""){
            temp_df.r1 <- reactive(subset(temp_df.r()[,c(3:5,i,29,30)], temp_df.r()[i]!=as.double(0) | is.na(temp_df.r()[i])))
            map <- addCircleMarkers(map, lng = temp_df.r1()$`Plant longitude`, lat = temp_df.r1()$`Plant latitude`,
                                    popup = paste(temp_df.r1()$`Plant name`,"Capacity:",temp_df.r1()[[4]],"Renewable:",temp_df.r1()[[6]],"% Non-renewable:",temp_df.r1()[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df.r1()[[4]]/temp_range.r())*30 +5 , fill = FALSE) 
          }
        }
      }
      
      # Non-renewable ["COAL","OIL","GAS","NUCLEAR","OTHER"]
      if(input$r12==TRUE | input$m12==TRUE){
        cc_rlist <- reactive(c("COAL","OIL","GAS","NUCLEAR",ccr5(),ccr6(),ccr7(),ccr8(),ccr9(),"OTHER"))
        for(i in 6:9){
          if(cc_rlist()[i-5]!=""){
            temp_df.r1 <- reactive(subset(temp_df.r()[,c(3:5,i,29,30)], temp_df.r()[i]!=as.double(0) | is.na(temp_df.r()[i])))
            map <- addCircleMarkers(map, lng = temp_df.r1()$`Plant longitude`, lat = temp_df.r1()$`Plant latitude`,
                                    popup = paste(temp_df.r1()$`Plant name`,"Capacity:",temp_df.r1()[[4]],"Renewable:",temp_df.r1()[[6]],"% Non-renewable:",temp_df.r1()[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df.r1()[[4]]/temp_range.r())*30 +5 , fill = FALSE) 
          }
        }
        if(cc_rlist()[10]!=""){
          temp_df.r1 <- reactive(subset(temp_df.r()[,c(3:5,15,29,30)], temp_df.r()[15]!=as.double(0) | is.na(temp_df.r()[15])))
          map <- addCircleMarkers(map, lng = temp_df.r1()$`Plant longitude`, lat = temp_df.r1()$`Plant latitude`,
                                  popup = paste(temp_df.r1()$`Plant name`,"Capacity:",temp_df.r1()[[4]],"Renewable:",temp_df.r1()[[6]],"% Non-renewable:",temp_df.r1()[[5]],"%"),
                                  color = color_code[10], radius = (temp_df.r1()[[4]]/temp_range.r())*30 +5 , fill = FALSE) 
        }
      }
      
      else{
        for(i in 1:10){
          if(cc_rlist()[i]!="" | cc_mlist()[i]!=""){
            temp_df.r1 <- reactive(subset(temp_df.r()[,c(3:5,i+5,29,30)], temp_df.r()[i+5]!=as.double(0) | is.na(temp_df.r()[i+5])))
            map <- addCircleMarkers(map, lng = temp_df.r1()$`Plant longitude`, lat = temp_df.r1()$`Plant latitude`,
                                    popup = paste(temp_df.r1()$`Plant name`,"Capacity:",temp_df.r1()[[4]],"Renewable:",temp_df.r1()[[6]],"% Non-renewable:",temp_df.r1()[[5]],"%"),
                                    color = color_code[i], radius = (temp_df.r1()[[4]]/temp_range.r())*30 +5 , fill = FALSE) 
          }
        }
      }
      
    }
    map <- addLegend(map,"bottomright", colors = color_code, labels = sources,
                     title = "Status",
                     opacity = 1)
    map
    if(input$rreset){
      map1 <-reactive(setView(map, lng = -89.3, lat = 40.2, zoom = 6))
      map1()
    }
    map2 <- reactive(switch (input$rmaptype,
                             "Default" = addTiles(map),
                             "1" = addProviderTiles(map, providers$Stamen.Toner),
                             "2" = addProviderTiles(map,providers$Esri.NatGeoWorldMap)
    )
    )
    map2()
  })  
  
  # Entire US tab leaflet
  ccus1  <- renderText({c1  = ifelse(input$us1, sources[1], "")})
  ccus2  <- renderText({c2  = ifelse(input$us2, sources[2], "")})
  ccus3  <- renderText({c3  = ifelse(input$us3, sources[3], "")})
  ccus4  <- renderText({c4  = ifelse(input$us4, sources[4], "")})
  ccus5  <- renderText({c5  = ifelse(input$us5, sources[5], "")})
  ccus6  <- renderText({c6  = ifelse(input$us6, sources[6], "")})
  ccus7  <- renderText({c7  = ifelse(input$us7, sources[7], "")})
  ccus8  <- renderText({c8  = ifelse(input$us8, sources[8], "")})
  ccus9  <- renderText({c9  = ifelse(input$us9, sources[9], "")})
  ccus10 <- renderText({c10 = ifelse(input$us10, sources[10], "")})
  cc_uslist <- reactive(c(ccus1(),ccus2(),ccus3(),ccus4(),ccus5(),ccus6(),ccus7(),ccus8(),ccus9(),ccus10()))
  
  output$usleaf <- renderLeaflet({
    # Initial set to Illinois state
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -101.2, lat = 39.3, zoom = 5)
    # ----------------------------------------------------
    # [COAL,OIL,GAS,NUCLEAR,HYDRO,BIOMASS,WIND,SOLAR,GEOTHERMAL,OTHER]
    #   6              9                   12                    15 
    

    temp_df_list.us <- reactive(switch(input$usyear,
                                    "2000" = df.2000_list,
                                    "2010" = df.2010_list,
                                    "2018" = df_list))
    temp_df.us <- reactive({
      if(input$usstate=="Entire US"){
        switch(input$usyear,
            "2000" = df.2000,
            "2010" = df.2010,
            "2018" = df)
      }
      else{
      temp_df_list.us()[[match(input$usstate,f_state)]]}
      })
    
    temp_range.us <- reactive(max(subset(temp_df.us()$TOTAL,is.na(temp_df.us()$TOTAL)==FALSE)) - min(subset(temp_df.us()$TOTAL,is.na(temp_df.us()$TOTAL)==FALSE)))

    if(input$us13==TRUE) {
      cc_uslist <- reactive(c("COAL","OIL","GAS","NUCLEAR","HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL","OTHER"))
      for(i in 6:15){
        if(cc_uslist()[i-5]!=""){
          temp_df.us1 <- reactive(subset(temp_df.us()[,c(3:5,i,29,30)], temp_df.us()[i]!=as.double(0) | is.na(temp_df.us()[i])))
          temp_df.us2 <- reactive(subset(temp_df.us1(), temp_df.us1()[4]>=input$min_slider & temp_df.us1()[4]<=input$max_slider))
          map <- addCircleMarkers(map, lng = temp_df.us2()$`Plant longitude`, lat = temp_df.us2()$`Plant latitude`,
                                  popup = paste(temp_df.us2()$`Plant name`,"Capacity:",temp_df.us2()[[4]],"Renewable:",temp_df.us2()[[6]],"% Non-renewable:",temp_df.us2()[[5]],"%"),
                                  color = color_code[i-5], radius = (temp_df.us2()[[4]]/temp_range.us())*30 +5 , fill = FALSE) 
        }
      }
    }
    else{
      # Renewable ["HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL"]
      if(input$us11==TRUE){
        cc_uslist <- reactive(c(ccus1(),ccus2(),ccus3(),ccus4(),"HYDRO","BIOMASS","WIND","SOLAR","GEOTHERMAL",ccus10()))
        for(i in 10:14){
          if(cc_uslist()[i-5]!=""){
            temp_df.us1 <- reactive(subset(temp_df.us()[,c(3:5,i,29,30)], temp_df.us()[i]!=as.double(0) | is.na(temp_df.us()[i])))
            temp_df.us2 <- reactive(subset(temp_df.us1(), temp_df.us1()[4]>=input$min_slider & temp_df.us1()[4]<=input$max_slider))
            map <- addCircleMarkers(map, lng = temp_df.us2()$`Plant longitude`, lat = temp_df.us2()$`Plant latitude`,
                                    popup = paste(temp_df.us2()$`Plant name`,"Capacity:",temp_df.us2()[[4]],"Renewable:",temp_df.us2()[[6]],"% Non-renewable:",temp_df.us2()[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df.us2()[[4]]/temp_range.us())*30 +5 , fill = FALSE) 
          }
        }
      }
      
      # Non-renewable ["COAL","OIL","GAS","NUCLEAR","OTHER"]
      if(input$us12==TRUE){
        cc_uslist <- reactive(c("COAL","OIL","GAS","NUCLEAR",ccus5(),ccus6(),ccus7(),ccus8(),ccus9(),"OTHER"))
        for(i in 6:9){
          if(cc_uslist()[i-5]!=""){
            temp_df.us1 <- reactive(subset(temp_df.us()[,c(3:5,i,29,30)], temp_df.us()[i]!=as.double(0) | is.na(temp_df.us()[i])))
            temp_df.us2 <- reactive(subset(temp_df.us1(), temp_df.us1()[4]>=input$min_slider & temp_df.us1()[4]<=input$max_slider))
            map <- addCircleMarkers(map, lng = temp_df.us2()$`Plant longitude`, lat = temp_df.us2()$`Plant latitude`,
                                    popup = paste(temp_df.us2()$`Plant name`,"Capacity:",temp_df.us2()[[4]],"Renewable:",temp_df.us2()[[6]],"% Non-renewable:",temp_df.us2()[[5]],"%"),
                                    color = color_code[i-5], radius = (temp_df.us2()[[4]]/temp_range.us())*30 +5 , fill = FALSE) 
          }
        }
        if(cc_uslist()[10]!=""){
          temp_df.us1 <- reactive(subset(temp_df.us()[,c(3:5,15,29,30)], temp_df.us()[15]!=as.double(0) | is.na(temp_df.us()[15])))
          temp_df.us2 <- reactive(subset(temp_df.us1(), temp_df.us1()[4]>=input$min_slider & temp_df.us1()[4]<=input$max_slider))
          map <- addCircleMarkers(map, lng = temp_df.us2()$`Plant longitude`, lat = temp_df.us2()$`Plant latitude`,
                                  popup = paste(temp_df.us2()$`Plant name`,"Capacity:",temp_df.us2()[[4]],"Renewable:",temp_df.us2()[[6]],"% Non-renewable:",temp_df.us2()[[5]],"%"),
                                  color = color_code[10], radius = (temp_df.us2()[[4]]/temp_range.us())*30 +5 , fill = FALSE) 
          
        }
      }
      
      else{
        for(i in 1:10){
          if(cc_uslist()[i]!=""){
            temp_df.us1 <- reactive(subset(temp_df.us()[,c(3:5,i+5,29,30)], temp_df.us()[i+5]!=as.double(0) | is.na(temp_df.us()[i+5])))
            temp_df.us2 <- reactive(subset(temp_df.us1(), temp_df.us1()[4]>=input$min_slider & temp_df.us1()[4]<=input$max_slider))
            map <- addCircleMarkers(map, lng = temp_df.us2()$`Plant longitude`, lat = temp_df.us2()$`Plant latitude`,
                                    popup = paste(temp_df.us2()$`Plant name`,"Capacity:",temp_df.us2()[[4]],"Renewable:",temp_df.us2()[[6]],"% Non-renewable:",temp_df.us2()[[5]],"%"),
                                    color = color_code[i], radius = (temp_df.us2()[[4]]/temp_range.us())*30 +5 , fill = FALSE) 
          }
        }

        
      }
      
    }
    map <- addLegend(map,"bottomright", colors = color_code, labels = sources,
                     title = "Status",
                     opacity = 1)
    map
    if(input$usreset){
      map1 <-reactive(setView(map, lng = -89.3, lat = 40.2, zoom = 5))
      map1()
    }
  })
  
  # Added/Idled tab leaflet
  output$addleaf <- renderLeaflet({
    if(input$byear==input$ayear){
      temp_df.before <- reactive(switch(input$byear,
                                        "2000" = df.2000,
                                        "2010" = df.2010,
                                        "2018" = df))
      map <- reactive(leaflet(temp_df.before()) %>%
             addTiles() %>%
             setView(lng = -101.2, lat = 39.3, zoom = 5) %>%
             addCircleMarkers(lng = ~`Plant longitude`, lat = ~`Plant latitude`, popup = ~`Plant name`,
                              color = "grey", radius = 2, fill = TRUE) %>%
             addLegend("bottomright", colors = c("grey"), labels = c("Idled"),
                       title = "Status",
                       opacity = 1)
             )
      map()
    }
    else{
      temp_df.before <- reactive(switch(input$byear,
                                         "2000" = df.2000,
                                         "2010" = df.2010,
                                         "2018" = df))
      temp_df.after <- reactive(switch(input$ayear,
                                        "2000" = df.2000,
                                        "2010" = df.2010,
                                        "2018" = df))
      temp_df.same <- reactive(
        subset(temp_df.before()[,c(3:5,31)],temp_df.before()$ORIS %in% temp_df.after()$ORIS == TRUE)
      )
      temp_df.diff <- reactive(
        subset(temp_df.after()[,c(3:5,31)],temp_df.after()$ORIS %in% temp_df.before()$ORIS == FALSE)
      )
      map <- reactive(leaflet(temp_df.same(),temp_df.diff()) %>%
             addTiles() %>%
             setView(map, lng = -101.2, lat = 39.3, zoom = 5) %>%
             addCircleMarkers(lng = temp_df.same()$`Plant longitude`, lat = temp_df.same()$`Plant latitude`, popup = temp_df.same()$`Plant name`,
                              color = "grey", radius = 1, fill = TRUE) %>%
             addCircleMarkers(lng = temp_df.diff()$`Plant longitude`, lat = temp_df.diff()$`Plant latitude`, popup = temp_df.diff()$`Plant name`,
                              color = "red", radius = 1, fill = TRUE) %>%
             addLegend("bottomright", colors = c("grey","red"), labels = c("Idled","Added"),
                        title = "Status",
                        opacity = 1)
             )
      map()
    
    }

    
  })
  
  
}

shinyApp(ui = ui, server = server)