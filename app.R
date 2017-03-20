library(shiny)
library(leaflet)
library(googlesheets)
library(RColorBrewer)

#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()

#===== REMEMBER maual depolyment with rsconnect::deployApp() ============== 

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "80%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                uiOutput("SelectCat"),
                checkboxInput("legend", "Show legend", TRUE), 
                textOutput("Selected"),
                tableOutput("table")
  )
)

#https://docs.google.com/spreadsheets/d/11to6blwYrHWau_QK7yQL9bs_fb4ywEC7NmeP-AoE288/edit?usp=sharing



server <- function(input, output, session) {

 # strPalName<-c("Red","Green","Blue","Black","Orange")  #'RdYlBu' #"PRGn"
  strPalName<-brewer.pal(7,"Dark2")
  
  # How do I give permission ot the shiny io server to read my google sheet?
  # =========================================================================
  #gdatatitle <-gs_key("11to6blwYrHWau_QK7yQL9bs_fb4ywEC7NmeP-AoE288")
  gdatatitle <-gs_key("15CDB7c3f59rAdWYPaQekZg1_uRAkv90z2W_tZRUP4o4")
                        
  #gdatatitle <- gs_title("R_test")
  df<-gs_read(gdatatitle)
  df2<-df[,c("Name","Lat","Lon","Category")]
  df$Link<-ifelse(is.na(df$Link),"",df$Link)
  df$HtmlText<-ifelse(df$Link=="",
                      paste0("<b>",df$Name,"</br><a>",df$LinkText,"</a></b>"),
                      paste0("<b>",df$Name,"</br><a target='_blank' href='",df$Link,"'>",df$LinkText,"</a></b>")) 
  
  df_r<-reactive({
    selected<-input$category
    if(is.null(selected)){selected<-"ALL"}
    if(selected!="ALL"){
      df[df$Category==selected,]
      #return(df)
    }else{
      df
    }
  })  
  
  
      #addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% 
  #https://leaflet-extras.github.io/leaflet-providers/preview/
  
  output$mymap <- renderLeaflet({
    leaflet({df_r()}) %>% 
      addProviderTiles("Esri.WorldTopoMap", options = providerTileOptions(noWrap = TRUE)) %>% 
      
      #addTiles() %>%
      #setView(lng = 12, lat = 56, zoom = 6) # %>%
      fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = {df_r()})
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <-colorFactor(strPalName, df$Category)
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Category
      )
    }
  })
  
  observeEvent(input$selected, { # update the map markers and view on location selectInput changes
    
  })
  
  
  observe({
    pal<-colorFactor(strPalName, df$Category)
    leafletProxy("mymap",data={df_r()}) %>%
      addCircleMarkers(~Lon, ~Lat, radius=5,popup=~HtmlText, 
                       stroke=TRUE, fillOpacity = 0.9, color = ~pal(Category))
  })
  
  observeEvent(input$MAPID_click, {
    ClickVar<-input$MAPID_click
    addPopups("mymap", lng=ClickVar$lng, lat=ClickVar$lat, popup="Here I am")
  })

  output$SelectCat <- renderUI({
    tagList(
      selectInput("category", "Category:", choices =c("ALL", df$Category), selected="ALL")
      )})
  #}]

  
}


shinyApp(ui, server)