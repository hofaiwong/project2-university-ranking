library(shiny)
library(leaflet)
library(googleVis)
library(htmltools)
source('./helpers.R')

## server.R ##
shinyServer(function(input, output){
  
  #Tab1: World map with country stats
  output$country.map <- renderGvis({

    args = if (input$sourceCountry == 1) { #Shanghai
      switch(input$selectedStat_shanghai,
             '1' = list(df.2015.country$top_shanghai, 'top_shanghai', "{colors: [\'green\', \'yellow\', \'red\']}"),
             '2' = list(df.2015.country$median_shanghai, 'median_shanghai', "{colors: [\'green\', \'yellow\', \'red\']}"),
             '3' = list(df.2015.country$count_shanghai, 'count_shanghai', "{colors: [\'orange\', \'yellow\', \'green\']}"),
             '4' = list(df.2015.country$total_score_shanghai, 'total_score_shanghai', "{colors: [\'red\', \'yellow\', \'green\']}"),
             '5' = list(df.2015.country$alumni, 'alumni', "{colors: [\'red\', \'yellow\', \'green\']}"),
             '6' = list(df.2015.country$award, 'award', "{colors: [\'red\', \'yellow\', \'green\']}"),
             '7' = list(df.2015.country$hici, 'hici', "{colors: [\'red\', \'yellow\', \'green\']}"),
             '8' = list(df.2015.country$ns, 'ns', "{colors: [\'red\', \'yellow\', \'green\']}"),
             '9' = list(df.2015.country$pub, 'pub', "{colors: [\'red\', \'yellow\', \'green\']}"),
             '10' = list(df.2015.country$pcp, 'pcp', "{colors: [\'red\', \'yellow\', \'green\']}")
             
             )} else if (input$sourceCountry == 2) { #Times
               switch(input$selectedStat_times,
                      '1' = list(df.2015.country$top_times, 'top_times', "{colors: [\'green\', \'yellow\', \'red\']}"),
                      '2' = list(df.2015.country$median_times, 'median_times', "{colors: [\'green\', \'yellow\', \'red\']}"),
                      '3' = list(df.2015.country$count_times, 'count_times', "{colors: [\'orange\', \'yellow\', \'green\']}"),
                      '4' = list(df.2015.country$total_score_times, 'total_score_times', "{colors: [\'red\', \'yellow\', \'green\']}"),
                      '5' = list(df.2015.country$teaching, 'teaching', "{colors: [\'red\', \'yellow\', \'green\']}"),
                      '6' = list(df.2015.country$international, 'international', "{colors: [\'red\', \'yellow\', \'green\']}"),
                      '7' = list(df.2015.country$research, 'research', "{colors: [\'red\', \'yellow\', \'green\']}"),
                      '8' = list(df.2015.country$citations_times, 'citations_times', "{colors: [\'red\', \'yellow\', \'green\']}"),
                      '9' = list(df.2015.country$income, 'income', "{colors: [\'red\', \'yellow\', \'green\']}")
                      
               )} else if (input$sourceCountry == 3) { #CWUR
                 switch(input$selectedStat_cwur,
                        '1' = list(df.2015.country$top_cwur, 'top_cwur', "{colors: [\'green\', \'yellow\', \'red\']}"),
                        '2' = list(df.2015.country$median_cwur, 'median_cwur', "{colors: [\'green\', \'yellow\', \'red\']}"),
                        '3' = list(df.2015.country$count_cwur, 'count_cwur', "{colors: [\'orange\', \'yellow\', \'green\']}"),
                        '4' = list(df.2015.country$total_score_cwur, 'total_score_cwur', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '5' = list(df.2015.country$quality_of_education, 'quality_of_education', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '6' = list(df.2015.country$alumni_employment, 'alumni_employment', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '7' = list(df.2015.country$quality_of_faculty, 'quality_of_faculty', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '8' = list(df.2015.country$publications, 'publications', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '9' = list(df.2015.country$influence, 'influence', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '10' = list(df.2015.country$citations_cwur, 'citations_cwur', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '11' = list(df.2015.country$broad_impact, 'broad_impact', "{colors: [\'red\', \'yellow\', \'green\']}"),
                        '12' = list(df.2015.country$patents, 'patents', "{colors: [\'red\', \'yellow\', \'green\']}")
                 )}
    
    world_map <- function(var, stat, color) {
      df = df.2015.country[!is.na(var),]
      gvisGeoChart(data = df, 
                   locationvar = "country", 
                   colorvar = stat,
                   options=list(
                     projection="kavrayskiy-vii",
                     colorAxis=color,
                     width='100%',
                     height='100%',
                     keepAspectRatio = TRUE
                   )
      )}
    
    do.call(world_map, args)
    
  })
  
  #Tab1: World map with country stats - Data table
  output$country.table <- renderDataTable({
    df.2015.country[,c(1,4,3,2,7,6,5,10,9,8)]
  })
  
  #Tab2: World map with universities
  output$unimap <- renderLeaflet({

      unimap<-leaflet(df.2015.uni) %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(~lon, ~lat,
                         radius = 4, stroke = F, fillOpacity = 0.5,
                         popup = ~paste(sep = "<br/>",new_name,
                     "Shanghai rank:",rank_shanghai,
                     "Times rank:",rank_times,
                     "CWUR rank:",rank_cwur))
      unimap
    
  })
  
  
  #Tab2: World map with universities - Data table
  output$uni.table <- renderDataTable({
    df.2015.uni[,c('new_name','country','rank_shanghai','rank_times','rank_cwur')]
  })  
  
  #Tab3: Bar chart for school comparison
  output$compare.shanghai <- renderGvis({
      df = shanghaiData %>%
        filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
        select(., new_name, alumni, award, hici, ns, pub, pcp, total_score) %>%
        arrange(., desc(new_name))
      yvar = c("alumni", "award", "hici", "ns", "pub", "pcp", "total_score")
      args <- list(df, yvar)
      do.call(compuni, args)
  })
  
  output$compare.times <- renderGvis({
    df = timesData %>%
      filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
      select(., new_name, teaching, international, research, citations, income, total_score) %>%
      arrange(., desc(new_name))
    yvar = c('teaching','international','research','citations','income','total_score')
    args <- list(df, yvar)
    do.call(compuni, args)
  })
  
  output$compare.cwur <- renderGvis({
    df = cwur %>%
      filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
      select(., new_name, quality_of_education,alumni_employment,quality_of_faculty,publications,influence,citations,broad_impact,patents,total_score) %>%
      arrange(., desc(new_name))
    yvar = c('quality_of_education','alumni_employment','quality_of_faculty','publications','influence','citations','broad_impact','patents')
    args <- list(df, yvar)
    do.call(compuni, args)
  })
  
})