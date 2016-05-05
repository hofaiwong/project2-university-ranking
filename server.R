library(shiny)
library(leaflet)
library(googleVis)
source('./helpers.R')

## server.R ##
shinyServer(function(input, output){
  
  mapIcon <- makeIcon(
    iconUrl = "http://www.map.boun.edu.tr/css/img/bina.png",
    iconWidth = 20, iconHeight = 20
    # iconAnchorX = 22, iconAnchorY = 94
    # shadowUrl = "http://leafletjs.com/docs/images/leaf-shadow.png",
    # shadowWidth = 50, shadowHeight = 64,
    # shadowAnchorX = 4, shadowAnchorY = 62
  )
  
  #Tab1: World map with country stats
  output$country.map <- renderGvis({
    args <- if (input$selectedStat == 1) { #Top
      switch(input$sourceCountry,
             '1' = list(df.2015.country$top_shanghai, 'top_shanghai', "{colors: [\'green\', \'yellow\', \'red\']}"),
             '3'= list(df.2015.country$top_cwur, 'top_cwur', "{colors: [\'green\', \'yellow\', \'red\']}"),
             '2' = list(df.2015.country$top_times, 'top_times', "{colors: [\'green\', \'yellow\', \'red\']}")
      )} else if (input$selectedStat == 2) { #Median
        switch(input$sourceCountry,
               '1' = list(df.2015.country$median_shanghai, 'median_shanghai', "{colors: [\'green\', \'yellow\', \'red\']}"),
               '3' = list(df.2015.country$median_cwur, 'median_cwur', "{colors: [\'green\', \'yellow\', \'red\']}"),
               '2' = list(df.2015.country$median_times, 'median_times', "{colors: [\'green\', \'yellow\', \'red\']}")
      )} else if (input$selectedStat == 3) { #Count
        switch(input$sourceCountry,
               '1' = list(df.2015.country$count_shanghai, 'count_shanghai', "{colors: [\'yellow\', \'green\']}"),
               '3' = list(df.2015.country$count_cwur, 'count_cwur', "{colors: [\'yellow\', \'green\']}"),
               '2' = list(df.2015.country$count_times, 'count_times', "{colors: [\'yellow\', \'green\']}")
        )}

    world_map <- function(var, stat, color) {
      df = df.2015.country[!is.na(var),]
      gvisGeoChart(data = df, 
                   locationvar = "country", 
                   colorvar = stat,
                   options=list(
                     projection="kavrayskiy-vii",
                     colorAxis=color
                   )
      )}
    
    do.call(world_map, args)
    
  })
  
  #Tab1: World map with country stats - Data table
  output$country.table <- renderDataTable({
    df.2015.country
  })
  
  #Tab2: World map with universities
  output$unimap <- renderLeaflet({
    # args2 <- switch(input$sourceCountry,
    #                 '1' = list(df.2015.uni$rank_shanghai, 'top_shanghai', "{colors: [\'green\', \'yellow\', \'red\']}"),
    #                 '3'= list(df.2015.uni$rank_cwur, 'top_cwur', "{colors: [\'green\', \'yellow\', \'red\']}"),
    #                 '2' = list(df.2015.uni$rank_times, 'top_times', "{colors: [\'green\', \'yellow\', \'red\']}")
    # )
    # 
    # unimap <- function(var, stat, color) {
      unimap<-leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
        addMarkers(data = df.2015.uni[,c('lon','lat')], 
                   icon = mapIcon,
                   clusterOptions = markerClusterOptions())
      unimap
    # }
    # 
    # do.call(uni_map, args2)
    
  })
  
  
  #Tab2: World map with universities - Data table
  output$uni.table <- renderDataTable({
    df.2015.uni[,c('new_name','country','rank_shanghai','rank_times','rank_cwur')]
  })  
  
  #Tab3: Bar chart for school comparison
  output$compare.shanghai <- renderGvis({
      df = shanghaiData %>%
        filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
        select(., new_name, alumni, award, hici, ns, pub, pcp, total_score)
      yvar = c("alumni", "award", "hici", "ns", "pub", "pcp", "total_score")
      args <- list(df, yvar)
      do.call(compuni, args)
  })
  
  output$compare.times <- renderGvis({
    df = timesData %>%
      filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
      select(., new_name, teaching, international, research, citations, income, total_score)
    yvar = c('teaching','international','research','citations','income','total_score')
    args <- list(df, yvar)
    do.call(compuni, args)
  })
  
  output$compare.cwur <- renderGvis({
    df = cwur %>%
      filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
      select(., new_name, quality_of_education,alumni_employment,quality_of_faculty,publications,influence,citations,broad_impact,patents,total_score)
    yvar = c('quality_of_education','alumni_employment','quality_of_faculty','publications','influence','citations','broad_impact','patents')
    args <- list(df, yvar)
    do.call(compuni, args)
  })
  
})