  library(shiny)
  library(shinyMobile)
  library(shinyWidgets)
  library(bs4Dash)
  library(tidyverse)
  library(shinycssloaders)
  library(highcharter)
  library(apexcharter)
  
  
  # import data
  cv_ev_agg=read.csv("00_data/map_data.csv")
  cv_ev_prdct<-read.csv("00_data/prediction_data.csv")
  cv_ev_data_out <- cv_ev_agg %>% select(State, County, Year, Total_Chargers, Vehicles, VehiclesChevrolet, VehiclesChevrolet_pct)%>%rename(Total_Vehicles=Vehicles, Chevrolet_Vehicles=VehiclesChevrolet,Chevrolet_MarketShare=VehiclesChevrolet_pct)
  
  shinyApp(
    ui = f7Page(tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
    ),
      title = "Tab layout",
    options = list(
      theme = c("ios", "md", "auto", "aurora"),
      dark = TRUE,
      filled = TRUE,
      color = "#009688",
      touch = list(
        tapHold = TRUE,
        tapHoldDelay = 750,
        iosTouchRipple = FALSE
      )),
      f7TabLayout(
        tags$head(
          tags$script(
            "$(function(){
               $('#tapHold').on('taphold', function () {
                 app.dialog.alert('Tap hold fired!');
               });
             });
             "
          )
        ),
        panels = tagList(
          f7Panel(title = "Menu", side = "left", theme = "light", "Left One", effect = "cover")        ),
        navbar = f7Navbar(
          title = "Electric Vehicle App",
          hairline = FALSE,
          shadow = TRUE,
          leftPanel = TRUE,
          rightPanel = FALSE
        ),
        f7Tabs(
          
          animated = FALSE,
          swipeable = TRUE,
          f7Tab(
            tabName = "Predict Chevrolet",
            icon = f7Icon("car_fill"),
            active = TRUE,
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              f7Card(
                title = "Chevy Vehicle Prediction",
                f7Picker(
                  inputId = "state_select_predict",
                  label = "Select State",
                  value = "WA",
                  placeholder = "Select State",
                  choices = unique(cv_ev_agg$State),
                  scrollToInput="TRUE"                ),
                f7Picker(
                  inputId = "county_select_predict",
                  value = "King",
                  label = "Select County",
                  choices = c("Please pick a state first!")
                ),
                box(status = "primary", "_"),
            
                box(
                  status = "primary"
                  ,solidHeader = FALSE 
                  ,collapsible = FALSE 
                  ,plotOutput("predictLineChart", height = "300px")
                ),
                

                footer = tagList(
                
                )
              )
            
          )),
          f7Tab(
            tabName = "Predict Station",
            icon = f7Icon("bolt_circle_fill"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Charging Station Prediction",
              f7Picker(
                inputId = "state_select_predict2",
                label = "Select State",
                value = "WA",
                placeholder = "Select State",
                choices = unique(cv_ev_agg$State)
              ),
              f7Picker(
                inputId = "county_select_predict2",
                value = "King",
                label = "Select County",
                choices = c("Please pick a state first!")
              ),
              box(status = "primary", "_"),
              
              box(
                status = "primary"
                ,solidHeader = FALSE 
                ,collapsible = FALSE 
                ,plotOutput("predictLineChart2", height = "300px")
              ),
              
              
              footer = tagList(
                
              )
            )
            
          )),
          f7Tab(
            tabName = "About",
            icon = f7Icon("info_circle_fill"),
            active = FALSE,
            f7Card(title = "ABOUT",
                   "This Mobile App was created by the Highway 51 Revisited team in 2021 as part of the Capstone Project for Northwestern University’s School of Professional Studies (SPS) Master of Science in Data Science (MSDS) program. For the purposes of this project, the Highway 51 Revisited team adopted the role of an independent data consulting firm contracted by Chevy Motors to analyze relationships between Electric Vehicle (EV) Charging Infrastructure and Chevy’s position in the EV marketplace.
This Mobile App uses Time Series Forecasting to project the number of registered Chevy EVs and EV charging stations on a county-by-county basis through 2025 for those US states sharing EV registration data on atlasevhub.com.
The Highway 51 Revisited team is Farzad Falsafi, Douglas Hickey, Andy Holst, Rahul Lingam, and Swapna Vaidya.")
            )
       
        
      )
    )),
    server = function(input, output) {

      

      
      observeEvent(input$state_select_predict, {
        updateF7Picker(
        inputId = "county_select_predict",
        value = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select_predict,"County"]%>%unique())[1],
        choices = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select_predict,"County"]%>%unique())
        )
        })
     
      observeEvent(input$state_select_predict2, {
        updateF7Picker(
          inputId = "county_select_predict2",
          value = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select_predict2,"County"]%>%unique())[1],
          choices = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select_predict2,"County"]%>%unique())
        )
      })
      
      selected_state=reactive({
        input$state_select_predict
      })
      
      selected_county=reactive({
        input$county_select_predict
      })
      
      selected_state2=reactive({
        input$state_select_predict2
      })
      
      selected_county2=reactive({
        input$county_select_predict2
      })
      
      reactive_df3=reactive({
        cv_ev_agg%>%filter(County==selected_county(),State==selected_state())
      })
      
      reactive_df4=reactive({
        cv_ev_prdct%>%filter(County==selected_county(),State==selected_state(),Year>=2020)
      })
   
      reactive_df5=reactive({
        cv_ev_agg%>%filter(County==selected_county2(),State==selected_state2())
      })
      
      reactive_df6=reactive({
        cv_ev_prdct%>%filter(County==selected_county2(),State==selected_state2(),Year>=2020)
      })
      
      output$predictLineChart<-renderPlot({
        ggplot() +
        geom_line(data=reactive_df3(), aes(x = Year, y = VehiclesChevrolet,color = "Actual"))+
        geom_line(data=reactive_df4(), aes(x = Year, y =  fcst,color = "Forecasted"),linetype='dotted')+
        scale_color_manual(labels = c("Actual", "Forecasted"), values = c("blue", "red"))+
        labs(y="Chevrolet Vehicle Count", x = "Year", title = "Chevrolet Vehicle Count Prediction",color = "")+
          theme(legend.position="bottom",plot.background = element_rect(fill = "gray"))
        
      })
      
      output$predictLineChart2<-renderPlot({
        ggplot() +
          geom_line(data=reactive_df5(), aes(x = Year, y = VehiclesChevrolet,color = "Actual"))+
          geom_line(data=reactive_df6(), aes(x = Year, y =  fcst,color = "Forecasted"),linetype='dotted')+
          scale_color_manual(labels = c("Actual", "Forecasted"), values = c("blue", "red"))+
          labs(y="Charging Station Count", x = "Year", title = "Total Charging Station Count Prediction",color = "")+
          theme(legend.position="bottom",plot.background = element_rect(fill = "gray"))
        
      })


      
    }
  )


