library(shiny)
library(leaflet)
library(googleVis)
library(htmltools)
source('./helpers.R')

## server.R ##
shinyServer(function(input, output){
  
  country = reactive({
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
  })
  
  #Tab1: World map with country stats
  output$country.map <- renderGvis({
    do.call(world_map, country())
  })
  
  
  #Tab1: Selected stat bar chart by country
  output$country.bar <- renderGvis({
    
    #Draw bar chart for countries by metric
    barcountry <- function(var, stat, color) {
      
      df = if (grepl('top',stat) | grepl('median',stat)) {
        df.2015.country[order(var),c('country',stat)]
      } else {
        df.2015.country[order(var, decreasing = TRUE),c('country',stat)]
      }
      
      df = df[complete.cases(df),]
      
      gvisBarChart(data = df, 
                   xvar = "country", 
                   yvar = stat,
                   options=list(legend="{position: 'top'}",
                                height=900,
                                align='top',
                                fontSize=8)
      )}
    
    do.call(barcountry, country())
    
  })
  
  
  
  #Data: Table for Shanghai stats for selected country
  output$selectcountry.shanghai.table <- renderDataTable({
    df = df.2015.country %>%
      filter(country %in% input$selectCountry) %>%
      select(c(1,4,7,10,29:36))
    
    df.t = t(df[,2:ncol(df)])
    colnames(df.t) = df$country
    cbind(Metric = rownames(df.t), df.t)
  })
  
  #Data: Table for Times stats for selected country
  output$selectcountry.times.table <- renderDataTable({
    df = df.2015.country %>%
      filter(country %in% input$selectCountry) %>%
      select(c(1,3,6,9,22:28))
    
    df.t = t(df[,2:ncol(df)])
    colnames(df.t) = df$country
    cbind(Metric = rownames(df.t), df.t)
  })
  
  #Data: Table for CWUR stats for selected country
  output$selectcountry.cwur.table <- renderDataTable({
    df = df.2015.country %>%
      filter(country %in% input$selectCountry) %>%
      select(c(1,2,5,8,11:21))
    
    df.t = t(df[,2:ncol(df)])
    colnames(df.t) = df$country
    cbind(Metric = rownames(df.t), df.t)
  })
  
  
  #Ranks for one university
  output$unirank.table <- renderDataTable({
    df = rankings %>%
      filter(new_name==input$selectUni) %>%
      select(country, rank_shanghai, rank_times, rank_cwur)
    colnames(df) = c('Country','Shanhai Rank','Times Rank','CWUR Rank')
    df
  }, options = list(searching=F, paging=F, bInfo=F))
  
  #Scoring criteria details for one university
  output$bar.shanghai <- renderGvis({
    df = rankings %>%
      filter(., new_name == input$selectUni) %>%
      select(., new_name, alumni, award, hici, ns, pub, pcp)
    colnames(df) = c('University','Alumni','Award','HiCi','NS','PUB','PCP')
    yvar = c('Alumni','Award','HiCi','NS','PUB','PCP')
    args <- list(df, yvar)
    do.call(baruni, args)
  })  
  
  output$bar.times <- renderGvis({
    df = rankings %>%
      filter(., new_name == input$selectUni) %>%
      select(., new_name, teaching, international, research, citations_times, income)
    colnames(df) = c('University','Teaching','International','Research','Citations','Income')
    yvar = c('Teaching','International','Research','Citations','Income')
    args <- list(df, yvar)
    do.call(baruni, args)
  })  
  
  output$bar.cwur <- renderGvis({
    df = rankings %>%
      filter(., new_name == input$selectUni) %>%
      select(., new_name, quality_of_education,alumni_employment,quality_of_faculty,publications,influence,citations_cwur,broad_impact,patents)
    colnames(df) = c('University','Education','Alumni','Faculty','Publications','Influence','Citations','Broad Impact','Patents')
    yvar = c('Education','Alumni','Faculty','Publications','Influence','Citations','Broad Impact','Patents')
    args <- list(df, yvar)
    do.call(baruni, args)
  })  
  
  
  
  
  
  #Tab1: World map with country stats - Data table
  output$country.table <- renderDataTable({
    df.2015.country[,c(1,4,3,2,7,6,5,10,9,8)]
  })
  
  
})