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
  
#Kaggle data only has uni name. Geocode sometimes gives incorrect lon/lat (e.g. Drexel) so not displaying this entire section  
  # unimapdata = reactive ({
  #   if (input$sourceUni==1) { #Shanghai
  #     df = df.2015.uni %>%
  #       filter(rank_shanghai>=input$s.sh.rank[1],
  #              rank_shanghai<=input$s.sh.rank[2],
  #              alumni>=input$s.sh.alumni[1],
  #              alumni<=input$s.sh.alumni[2],
  #              award>=input$s.sh.award[1],
  #              award<=input$s.sh.award[2],
  #              hici>=input$s.sh.hici[1],
  #              hici<=input$s.sh.hici[2],
  #              ns>=input$s.sh.ns[1],
  #              ns<=input$s.sh.ns[2],
  #              pub>=input$s.sh.pub[1],
  #              pub<=input$s.sh.pub[2],
  #              pcp>=input$s.sh.pcp[1],
  #              pcp<=input$s.sh.pcp[2]
  #       )
  #   } else if (input$sourceUni==2) { #Times
  #     df = df.2015.uni %>%
  #       filter(rank_times>=input$s.t.rank[1],
  #              rank_times<=input$s.t.rank[2],
  #              teaching>=input$s.t.teaching[1],
  #              teaching<=input$s.t.teaching[2],
  #              international>=input$s.t.international[1],
  #              international<=input$s.t.international[2],
  #              research>=input$s.t.research[1],
  #              research<=input$s.t.research[2],
  #              citations_times>=input$s.t.citations_times[1],
  #              citations_times<=input$s.t.citations_times[2],
  #              income>=input$s.t.income[1],
  #              income<=input$s.t.income[2]
  #       )
  #   }
  #   
  # })
  # 
  # #Tab2: World map with universities
  # output$unimap <- renderLeaflet({
  #   unimap<-leaflet(data = unimapdata()) %>%
  #     setView(lng=0, lat=0, zoom=2) %>%
  #     addProviderTiles("Esri.WorldStreetMap") %>%
  #     addCircleMarkers(~lon, ~lat,
  #                      radius = 4, stroke = F, fillOpacity = 0.5,
  #                      popup = ~paste(sep = "<br/>",new_name,
  #                                     "Shanghai rank:",rank_shanghai,
  #                                     "Times rank:",rank_times,
  #                                     "CWUR rank:",rank_cwur))
  #     unimap
  # })
  # 
  # observe({
  #   leafletProxy("unimap", data = unimapdata()) %>%
  #     clearMarkers() %>%
  #       addCircleMarkers(~lon, ~lat,
  #                        radius = 4, stroke = F, fillOpacity = 0.5,
  #                        popup = ~paste(sep = "<br/>",new_name,
  #                                       "Shanghai rank:",rank_shanghai,
  #                                       "Times rank:",rank_times,
  #                                       "CWUR rank:",rank_cwur))
  # })
  # 
  # #Tab2: World map with universities - Data table
  # output$uni.table <- renderDataTable({
  #   df.2015.uni[,c('new_name','country','rank_shanghai','rank_times','rank_cwur')]
  # })  

  
  #Scoring criteria details for one university
  output$bar.shanghai <- renderGvis({
    df = rankings %>%
      filter(., new_name == input$selectUni) %>%
      select(., new_name, alumni, award, hici, ns, pub, pcp)
    yvar = c("alumni", "award", "hici", "ns", "pub", "pcp")
    args <- list(df, yvar)
    do.call(baruni, args)
  })  
  
  output$bar.times <- renderGvis({
    df = rankings %>%
      filter(., new_name == input$selectUni) %>%
      select(., new_name, teaching, international, research, citations_times, income)
    yvar = c('teaching','international','research','citations_times','income')
    args <- list(df, yvar)
    do.call(baruni, args)
  })  
  
  output$bar.cwur <- renderGvis({
    df = rankings %>%
      filter(., new_name == input$selectUni) %>%
      select(., new_name, quality_of_education,alumni_employment,quality_of_faculty,publications,influence,citations_cwur,broad_impact,patents)
    yvar = c('quality_of_education','alumni_employment','quality_of_faculty','publications','influence','citations_cwur','broad_impact','patents')
    args <- list(df, yvar)
    do.call(baruni, args)
  })  
  
  
  
    
  # #DELETE: Bar chart for school comparison
  # output$compare.shanghai <- renderGvis({
  #     df = shanghaiData %>%
  #       filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
  #       select(., new_name, alumni, award, hici, ns, pub, pcp) %>%
  #       arrange(., desc(new_name))
  #     yvar = c("alumni", "award", "hici", "ns", "pub", "pcp")
  #     args <- list(df, yvar)
  #     do.call(compuni, args)
  # })
  # 
  # output$compare.times <- renderGvis({
  #   df = timesData %>%
  #     filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
  #     select(., new_name, teaching, international, research, citations, income) %>%
  #     arrange(., desc(new_name))
  #   yvar = c('teaching','international','research','citations','income')
  #   args <- list(df, yvar)
  #   do.call(compuni, args)
  # })
  # 
  # output$compare.cwur <- renderGvis({
  #   df = cwur %>%
  #     filter(., year == 2015 & new_name %in% c(input$uni1,input$uni2,input$uni3)) %>%
  #     select(., new_name, quality_of_education,alumni_employment,quality_of_faculty,publications,influence,citations,broad_impact,patents) %>%
  #     arrange(., desc(new_name))
  #   yvar = c('quality_of_education','alumni_employment','quality_of_faculty','publications','influence','citations','broad_impact','patents')
  #   args <- list(df, yvar)
  #   do.call(compuni, args)
  # })
  
})