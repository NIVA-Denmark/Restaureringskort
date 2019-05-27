

library(shiny)
library(leaflet)

shinyUI(
  fluidPage(title="Kort over akvatiske restaurerings projekter",
    titlePanel(title=div(
      a(href="https://niva-denmark.dk/",
        img(src="NIVA-Denmark-150.png"))
      ,"Kort over akvatiske restaurerings projekter"
      )),
    
    column(9,
        leafletOutput("mymap",height = 800)
      ),
    column(3,
      wellPanel(
          uiOutput("SelectCat"),
        uiOutput("SelectKeywd"),
       tableOutput("table")
      )
      
  )
))

# 
# shinyUI(bootstrapPage(title="Kort over akvatiske restaurerings projekter",
#                       tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#                       
#                       leafletOutput("mymap", width = "80%", height = "100%"),
#                       
#                       absolutePanel(top = 0, right = 0, width = "20%", height = "100%",
#                                     uiOutput("SelectCat"),
#                                     uiOutput("SelectKeywd"),
#                                     #tableOutput("table2"),
#                                     tableOutput("table")