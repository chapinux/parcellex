#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)



### data loading and setup###############################s
path_to_vignettes <-  "/media/chap/HashDaiDai/bidouille_Anatol/all_ids_patches/"
path_to_data <-  "~/dev/parcellex/data60obs_MSEMAER2.csv"
path_to_parcelsSHP <- "~/dev/parcellex/rpg_2017_T31TFM.geojson" 

yy <- read.csv(path_to_data)
yy <- yy[,-1]
yy$deltaTP <- abs(yy$truth-yy$pred)

parcelles <-  read_sf(path_to_parcelsSHP,stringsAsFactors = F)
#uniquement les parcelles concernées par les données du dataset
parcelles <-  parcelles %>%  filter(ID_PARCEL %in% yy$ID_PARCEL)  %>% select(ID_PARCEL,CODE_CULTU,geometry)


codecultID <- data.frame(parcelles$ID_PARCEL, parcelles$CODE_CULTU) 
names(codecultID) <- c("ID_PARCEL", "CodeCulture")
yy <- left_join(yy,codecultID)


# outlier removal
yy <- yy %>% filter(slope >= 0 & elevation < 1.14e23)

#central data objects

Xrange = c(min(yy$truth),max(yy$truth))
Yrange = c(min(yy$pred), max(yy$pred))

fulldata <- yy

fulldata$MAE <-  round(fulldata$MAE, 4)
fulldata$MSE <-  round(fulldata$MSE, 4)
fulldata$Rsquare <-  round(fulldata$Rsquare, 4)
fulldata$deltaTP <-  round(fulldata$deltaTP, 4)



currentdata <- yy
rm(yy)
#############################################



paramSliderFactory <-  function(feature){
feature <-fulldata[feature]
minfeature <-  round(min(feature, na.rm=T), digits = 2)
maxFeature <-  round(max(feature, na.rm=T), digits = 2)
stepFeature <-  floor(quantile(feature, seq(0, 1, 0.1), na.rm = T))
stepFeature <- round(abs(stepFeature[6]-stepFeature[7]), digits = 2)
paramSliderFeature <-  c(mini <- minfeature, maxi <- maxFeature, step <- stepFeature)
return(paramSliderFeature)
}



sliSlopeparam <- paramSliderFactory("slope") 
sliElevationparam <- paramSliderFactory("elevation") 
sliSurfaceparam <- paramSliderFactory("surface") 
sliPerimeterparam <- paramSliderFactory("perimeter") 
sliNbpixparam <- paramSliderFactory("nbpix") 
sliDeltaTPparam <- paramSliderFactory("deltaTP") 
sliMSEparam <-  paramSliderFactory("MSE")
sliMAEparam <- paramSliderFactory("MAE")
sliR2param <-  paramSliderFactory("Rsquare")
 
IDs <-  fulldata$ID_PARCEL %>% unique()

currentFeatureName <-  "surface"


ui <- fluidPage(
  
  
  
  
   fluidRow(
     column(3, div(style = "height:30%"),
            sliderInput('nbparcels', "Number of parcels",
                        min=100, max=4770, value=100, step=100),
            sliderInput('sliSlope', "Slope",
                        min=sliSlopeparam[1], max=sliSlopeparam[2], value=sliSlopeparam[1:2], step=sliSlopeparam[3]),
            sliderInput('sliElevation', "Elevation",
                        min=sliElevationparam[1], max=sliElevationparam[2], value=sliElevationparam[1:2], step=sliElevationparam[3]),
            sliderInput('sliPerimeter', "Perimeter",
                        min=sliPerimeterparam[1], max=sliPerimeterparam[2], value=sliPerimeterparam[1:2], step=sliPerimeterparam[3]),
            sliderInput('sliSurface', "Surface",
                        min=sliSurfaceparam[1], max=sliSurfaceparam[2], value=sliSurfaceparam[1:2], step=sliSurfaceparam[3]),
            sliderInput('sliNbpix', "Nbpix",
                        min=sliNbpixparam[1], max=sliNbpixparam[2], value=sliNbpixparam[1:2], step=sliNbpixparam[3]),
            sliderInput('sliDeltaTP', "Delta Truth - Pred",
                        min=sliDeltaTPparam[1], max=sliDeltaTPparam[2], value=sliDeltaTPparam[1:2], step=0.1),
            sliderInput('sliMSE', "MSE",
                        min=sliMSEparam[1], max=sliMSEparam[2], value=sliMSEparam[1:2], step=0.01),
            sliderInput('sliMAE', "MAE",
                        min=sliMAEparam[1], max=sliMAEparam[2], value=sliMAEparam[1:2], step=0.01),
            sliderInput('sliR2', "R square",
                        min=sliR2param[1], max=sliR2param[2], value=sliR2param[1:2], step=0.01)
            ),
     
     column(9, 
            fluidRow(
            plotlyOutput(outputId = 'plotPrincipal', height = "450px") 
            ),
            fluidRow(
              column(2,
                     selectizeInput( inputId =  "IDselector",
                        selected=NULL,
                       label = h3("Parcel selector"),
                       choices = NULL,options = list(maxItems = 1,placeholder = 'ID_PARCEL')),
                      actionButton("clearIDbutton", "Clear current parcel")
                     ),
              column(3,
                     plotOutput('plotSHP', height = "300px")
              ),
              column(3, 
                     imageOutput("vignette", height="300px")
              ),
              column(2,
                     tableOutput('infoClick')
              ),
              column(2,
                     radioButtons('featureColor', "color",choices = c("surface", "perimeter",  "elevation", "slope", "nbpix", "codeculture", "deltaTP", "MSE", "MAE", "Rsquare"), selected = "surface")
                     )
            )#fluidrow
     )#colonne
     
   
    ),

  fluidRow(
    plotlyOutput(outputId = 'plotTimeSerie', height = "200px")
    )#fluidrow
 )#fluidpage
  
  
  
 




server <- function(input, output,session) {
   
  updateSelectizeInput(session, inputId = 'IDselector', selected = character(0), choices = IDs, server = TRUE)
  
  observeEvent(input$clearIDbutton,
               {
                 updateSelectizeInput(session, inputId = 'IDselector', selected = character(0), choices = IDs, server = TRUE)
                 currentdata <<- subsetdf()
                 
                 
                 
                  })
  
  subsetdf <- reactive({
  
    if(input$IDselector %in% IDs ){
     
      filtereddf <- fulldata %>%  filter(ID_PARCEL == input$IDselector)
    }
      else{
   
    selectedParcelsID <-  sample(parcelles$ID_PARCEL, size = input$nbparcels)
    filtereddf <-  fulldata %>%  filter(ID_PARCEL %in% selectedParcelsID) %>% 
                                 filter(between(slope, input$sliSlope[1],input$sliSlope[2] )) %>%
                                 filter(between(perimeter, input$sliPerimeter[1],input$sliPerimeter[2] )) %>%
                               filter(between(surface, input$sliSurface[1],input$sliSurface[2] )) %>%
                                filter(between(elevation, input$sliElevation[1],input$sliElevation[2])) %>%
                              filter(between(nbpix, input$sliNbpix[1],input$sliNbpix[2])) %>%
                               filter(between(deltaTP, input$sliDeltaTP[1],input$sliDeltaTP[2] )) %>% 
                                filter(between(MSE, input$sliMSE[1],input$sliMSE[2] )) %>% 
                                filter(between(MAE, input$sliMAE[1],input$sliMAE[2] )) %>% 
                                filter(between(Rsquare, input$sliR2[1],input$sliR2[2] )) 
      
    }  
    
        return(filtereddf)
  })
  
  
  output$plotTimeSerie <- renderPlotly({
    evd <- event_data("plotly_click")
    req(evd)
    serie <-  fulldata %>% filter(ID_PARCEL==evd$customdata)
    dates <- c(1:nrow(serie))
    plt2 <- plot_ly(data=serie, x=~dates)
    plt2 <- plt2 %>% add_trace(y = ~truth, name = 'truth',mode = 'lines+markers',symbols=c('hexagon2'))
    plt2 <- plt2 %>% add_trace(y = ~pred, name = 'pred',mode = 'lines+markers', symbols=c('x') )
    plt2
    
    })
  
  
  
  
  output$plotPrincipal <-  renderPlotly({

    currentdata <<- subsetdf()
     currentdata$colorValue <-  switch(input$featureColor,
           "surface" = currentdata$surface,
           "elevation" = currentdata$elevation,
           "slope"= currentdata$slope,
           "perimeter" = currentdata$perimeter,  
            "nbpix"= currentdata$nbpix, 
           "codeculture"= currentdata$CodeCulture, 
           "deltaTP"= currentdata$deltaTP,
           "MSE"=currentdata$MSE,
           "MAE"=currentdata$MAE,
           "Rsquare"= currentdata$Rsquare
           )
  
    highlightableCurrendata <- highlight_key(currentdata, ~ID_PARCEL)
    
    
  plt1 <- plot_ly(data=highlightableCurrendata, x=~truth, y=~pred,
                   type='scatter',
                    mode = 'markers',
                  customdata=~ID_PARCEL,
                    #group=~ CodeCulture,
                    #name=~CodeCulture,
                 text = ~paste("<br><b>ID",ID_PARCEL
                               #   "</b><br>surface:",surface,
                               #   "<br>perimeter:",perimeter,
                               #   "<br>slope:", slope,
                               #   "<br>elevation:", elevation,
                               #   "<br>nbpix:", nbpix,
                               # "<br>CodeCulture:", CodeCulture  
                               ),
                    color=~colorValue
) %>%  layout(title = paste0('Truth/Pred colored by ', input$featureColor),
                yaxis = list(hoverformat = '.2f', range=Yrange),
                xaxis = list(hoverformat = '.2f', range= Xrange),
              legend = list(orientation = 'h', x=0.5,xanchor='center', y=-0.2)
              ) %>% event_register("plotly_hover" ) %>% 
    highlight(on = "plotly_click", off = "plotly_doubleclick") 
  
      
  })  
  

  output$plotSHP <- renderPlot({
    evd <- event_data("plotly_click")
    req(evd)
    idxparcelle <- which(parcelles$ID_PARCEL== evd$customdata)
    parcelleGeom <- parcelles[idxparcelle,"geometry"]
    par(mar=c(0,0,0,0))
    plot(parcelleGeom, border = "orange", bg="#dddddd", lwd=1.5)
    })
  
  
  output$infoClick <- renderTable({
    evd <- event_data("plotly_click")
    req(evd)
    ligne <-  currentdata %>% filter(ID_PARCEL == evd$customdata & truth==evd$x & pred==evd$y)
    nomsligne <- names(ligne)
    colonne <- data.frame(nomsligne, t(ligne))
    names(colonne) <- c("name", "value")
    return(colonne)
    
  })
  
  
  output$vignette <- renderImage({
    evd <- event_data("plotly_click")
    req(evd)
    fifiname <-  paste0(as.character(evd$customdata), ".png")
    fullname <-  paste0(path_to_vignettes, fifiname)
    # cat(fullname)
    list(src = fullname,
         width = 300,
         height = 300,
         alt = "parcelle vignette")
  }, deleteFile = FALSE)
    
  # output$id_by_data <- renderPrint({
  #   evd <-  event_data("plotly_hover")
  #   req(evd)
  # print(evd$customdata)
  # })
  
}


shinyApp(ui, server)



