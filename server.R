library(shiny)
library(leaflet)
library(googlesheets)
library(RColorBrewer)

shinyServer(function(input, output,session) {
  #gdatatitle <-gs_key("1UwT-mpX00bcncnZa-xC9yb-aDfcfR0X4bTU60-wDahY")
  
  gdatatitle <-gs_key("1iIn5-NFevKpSrqUMCn1_CY5yFPaYNcinQuvhVk7G0Lw")
  df<- gs_read(gdatatitle)
  browser()
  names(df)[names(df)=="Udarbejdet for"]<-"UdarbejdetFor"
  names(df)[names(df)=="Størrelse på restaureret område"]<-"Omraade"
  names(df)[names(df)=="Entreprenør"]<-"Entreprenoer"
  names(df)[names(df)=="Navn"]<-"Name"
  names(df)[names(df)=="Kategori"]<-"Category"
  
  
  df$HtmlText<-paste0("<b>",df$Name,"</b><br/>",df$Projekt,"<br/>")
  df$HtmlText<-ifelse(is.na(df$UdarbejdetFor),df$HtmlText,paste0(df$HtmlText,"<br/>Udført for:    ",df$UdarbejdetFor))
  df$HtmlText<-ifelse(is.na(df$Projektdesign),df$HtmlText,paste0(df$HtmlText,"<br/>Projektdesign:  ",df$Projektdesign))
  df$HtmlText<-ifelse(is.na(df$Entreprenoer),df$HtmlText,paste0(df$HtmlText,"<br/>Entreprenør: ",df$Entreprenoer))
  df$HtmlText<-ifelse(is.na(df$Omkostninger),df$HtmlText,paste0(df$HtmlText,"<br/>Omkostninger: ",df$Omkostninger))
  df$HtmlText<-ifelse(is.na(df$Finansiering),df$HtmlText,paste0(df$HtmlText,"<br/>Finansiering: ",df$Finansiering))
  df$HtmlText<-ifelse(is.na(df$Omraade),df$HtmlText,paste0(df$HtmlText,"<br/>Område: ",df$Omraade))
  df$HtmlText<-ifelse(is.na(df$Beskrivelse),df$HtmlText,paste0(df$HtmlText,"<br/><br/><i>",df$Beskrivelse,"</i>"))
  df$HtmlText<-ifelse(is.na(df$Link),df$HtmlText,paste0(df$HtmlText,"<br/><br/><a target='_blank' href='",df$Link,"'>",df$LinkText,"</a>"))
  
  selected<-reactive({
    if(is.null(input$category)){
      unique(df_r()$Category)
    }else{
      if(input$category=="ALL"){
        unique(df_r()$Category)
      }else{
        input$category
      }
    }
  })
  
  cat_list<-reactive({
    c("ALL",sort(unique(df_r()$Category)))
  })
  
  
  kw_list<-reactive({
    df1<-df
    if(!is.null(input$category)){
      if(!input$category=="ALL"){
        df1<-df1[df1$Category == input$category,]
      }
    }
    sort(unique(df1$Keywords2))
  })
  
  
  df_r<-reactive({
    df1<-df
    if(!is.null(input$category)){
       if(!input$category=="ALL"){
         df1<-df1[df1$Category == input$category,]
       }
    }
    
    if(length(input$keyword)>0){
     df1<-df1[df1$Keywords2 %in% input$keyword,]
    }
    df1
    })
  
  df_t<-reactive({
    df_r()[,c("ID","Name","Lat","Lon","Category")]
  })
  
  output$table <- renderTable(df_t())
  output$table2 <- renderTable(selected())
  
  strPalName<-brewer.pal(3,"Set1")
  
  #pal<-colorFactor(strPalName, df$Category)
  output$mymap <- renderLeaflet({
    pal<-colorFactor(strPalName, df$Category)
    leaflet({df_r()}) %>% 
      addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(noWrap = TRUE)) %>% 
    addCircleMarkers(~Lon, ~Lat, radius=5,popup=~HtmlText,label=~Name,
                     stroke=TRUE, color="black",weight=1,opacity=1,fillOpacity = 1,fillColor=~pal(Category))
    
  })  
  output$SelectCat <- renderUI({
    tagList(
      selectInput("category", "Vælg søer, vandløb eller marint:", choices=c("ALL",sort(unique(df$Category))), selected="ALL")
    )})
  
  output$SelectKeywd <- renderUI({
    tagList(
      selectInput("keyword", "Søgeord:", choices=kw_list(),multiple=TRUE) #,width='400px'
      #choices=c(sort(unique(df_r()$Keywords2)))
    )})
  
})
