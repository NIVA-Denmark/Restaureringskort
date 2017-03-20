library(shiny)
library(leaflet)
library(googlesheets)
library(RColorBrewer)

#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()

#===== REMEMBER maual depolyment with rsconnect::deployApp() ============== 

#DEL 1: Her er User Interface, ui:
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "80%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                uiOutput("SelectCat"),
                uiOutput("SelectKeywords2"),
                checkboxInput("legend", "Show legend", TRUE), 
                textOutput("Selected"),
                textOutput("Selected8"),
                tableOutput("table")
  )
)

#https://docs.google.com/spreadsheets/d/15CDB7c3f59rAdWYPaQekZg1_uRAkv90z2W_tZRUP4o4/edit?usp=sharing - oprindeligt
#https://docs.google.com/spreadsheets/d/1iIn5-NFevKpSrqUMCn1_CY5yFPaYNcinQuvhVk7G0Lw/edit?usp=sharing - R_DATA - Korrekt

# DEL 2: Her er server-scriptet, server:
server <- function(input, output, session) {
  
  # strPalName<-c("Red","Green","Blue","Black","Orange")  #'RdYlBu' #"PRGn"
  # brewer.pal (nr af farver i brug,"Type af palette"). muligheder kan ses her: https://www.stat.ubc.ca/~jenny/STAT545A/block14_colors.html
  # display.brewer: farvelpallette i brug vises under 'Plots' til højre
  display.brewer.pal(3, "Set1")
  strPalName<-brewer.pal(3,"Set1")
  
  
  # How do I give permission ot the shiny io server to read my google sheet?
  # =========================================================================
  gdatatitle <-gs_key("1iIn5-NFevKpSrqUMCn1_CY5yFPaYNcinQuvhVk7G0Lw")
  #gdatatitle <- gs_title("R_test")
  df<-gs_read(gdatatitle)
  df2<-df[,c("Name","Projekt","Madeby","Lat","Lon","Category","Keywords2","Område","Beskrivelse")] #disse kolonner henter jeg ind og bruger senere, bla. til søgeboks
  df$Link<-ifelse(is.na(df$Link),"",df$Link)
  df$HtmlText<-ifelse(df$Link=="",
                      #Her har jeg lavet min tekst til pop-up-boksene
                      paste0("<b>",df$Name,"</br></b>",df$Projekt,"</br><br>Udført for:    ",df$Madeby,"<br>Projektdesign:  ",df$Projektdesign,"<br>Entreprenør:  ",df$Entreprenør,"</br>Omkostninger:  ",df$Omkostninger,"</br>Finansiering: ",df$Finansiering,"</br>Område: ",df$Område,"</br></br><i>",df$Beskrivelse,"</i></br><a>",df$LinkText,"</a>"),
                      paste0("<b>",df$Name,"</br></b>",df$Projekt,"</br><br>Udført for:    ",df$Madeby,"<br>Projektdesign:   ",df$Projektdesign,"<br>Entreprenør:   ",df$Entreprenør,"</br>Omkostninger:  ",df$Omkostninger,"</br>Finansiering: ",df$Finansiering,"</br>Område: ",df$Område,"</br></br><i>",df$Beskrivelse,"</i></br><a target='_blank' href='",df$Link,"'>",df$LinkText,"</a>")) 
  
  df_r<-reactive({
    selected<-input$category
    
    selected8<-input$Keywords2
    if(is.null(selected)){selected<-"ALL"}
    if(is.null(selected8)){selected8<-"ALL"}
    
    if(selected!="ALL"){
      df1<-df[df$Category==selected,]
      #return(df)
    }else{
      df1<-df
    }
    
    if(selected8!="ALL"){
      df1[df1$Keywords2==selected8,]
      #return(df)
    }else{
      df1
    }
    
    
  }) 
  
  # Jeg prøver at få søgebox 2 til at reagere her: Jeg har midlertidigt disabled den for at sægeboks 1 virker
  
  #df_r<-reactive({
  # Selectize<-input$Keywords2
  # if(is.null(Selectize)){Selectize<-"ALL"}
  # if(Selectize!="ALL"){
  #   df[df$Keywords2==Selectize,]
  #return(df)
  # }else{
  #    df
  #   }
  # }) 
  
  output$mymap <- renderLeaflet({
    leaflet({df_r()}) %>% 
      addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(noWrap = TRUE)) %>% 
      #Herover specificeret korttypen: eg. Esri.WorldStreetMap eller Stamen.TonerLite
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
  # er denne vigtig? Jeg tror ikke den ændrede noget
  observeEvent(input$selected8, { # update the map markers and view on location selectInput changes
    
  }) 
  
  
  # Stroke angiver kant, som er sort, 1 pixel, ikke-gennemsnigtig. Alt med fill foran er fyldet. 
  observe({
    pal<-colorFactor(strPalName, df$Category)
    leafletProxy("mymap",data={df_r()}) %>%
      # her specificerer jeg min marker: cirkel m. sort kant, farver efter Kategori
      addCircleMarkers(~Lon, ~Lat, radius=5,popup=~HtmlText, 
                       stroke=TRUE, color="black",weight=1,opacity=1,fillOpacity = 1, fillColor = ~pal(Category))
  })
  
  observeEvent(input$MAPID_click, {
    ClickVar<-input$MAPID_click
    addPopups("mymap", lng=ClickVar$lng, lat=ClickVar$lat, popup="Here I am")
  })
  
  output$SelectCat <- renderUI({
    tagList(
      selectInput("category", "Vælg søer, vandløb eller marint:", choices =c("ALL", df$Category), selected="ALL")
    )})
  
  #HER HAR JEG LAVET EN SØGEBOKS VERSION 2 - bare som test med 1 ord per række
  
  output$SelectKeywords2 <- renderUI({
    tagList(
      selectInput("Keywords2", "Søgeord", choices =c(df$Keywords2), selected=NULL,multiple = TRUE)
    )})
  # output$SelectKeywords2 <- renderUI({
  #tagList(
  #selectizeInput("Keywords2", "Søg", choices =c(df$Keywords2), selected=NULL, multiple = TRUE,
  #   options = list(placeholder = 'Vælg søgeord'))
  #)})
  
}

#Her samler jeg Ui og server til en ShinyApp

shinyApp(ui, server)
