library(plotly)
library(shiny)
library(leaflet)
library(googleVis)
library(htmltools)
source('./helpers.R')

## server.R ##
shinyServer(function(input, output){
  
  #Scatter plot
  output$country.scatter = renderPlotly({

    df = rankings[,c(26,15,4,1,2)] %>%
      select(as.numeric(input$sourceScatterX), 
             as.numeric(input$sourceScatterY),
             University = new_name,
             country)
    #Only keep the 2 rankings selected for the scatter
    df = df[complete.cases(df),]
    #Only keep the country(ies) involved, or all by default
    if (!input$countryScatter=='All') {
      df = df[df$country %in% input$countryScatter,]
    }
    
    org.x = switch(input$sourceScatterX,
                 '1' = list(df$rank_shanghai,'Shanghai Ranking'),
                 '2' = list(df$rank_times,'Times Ranking'),
                 '3' = list(df$rank_cwur,'CWUR Ranking'))
    org.y = switch(input$sourceScatterY,
                   '1' = list(df$rank_shanghai,'Shanghai Ranking'),
                   '2' = list(df$rank_times,'Times Ranking'),
                   '3' = list(df$rank_cwur,'CWUR Ranking'))
    
    xaxis = list(title = org.x[[2]]) #x-axis title
    yaxis = list(title = org.y[[2]]) #y-axis title
    max=max(df[,1:2])
    
    #Draw scatterplot
    plot_ly(df, x = org.x[[1]], y = org.y[[1]],
            text = paste(University,
                         paste0(org.y[[2]],': ',org.y[[1]]),
                         paste0(org.x[[2]],': ',org.x[[1]]), sep='<br>'),
            mode = "markers",
            color = country,
            colors = 'Set1') %>%
            layout(xaxis = xaxis, 
                   yaxis = yaxis, 
                   title = paste(org.y[[2]],'by',org.x[[2]], sep=' ')) %>%
      add_trace(x = c(0,max), y = c(0,max), mode = "line", 
                showlegend=T, name='')
  })
  
  
  
  #Reactive for world map data by country
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
  
  
  
  ################
  ####Data Tab####
  ################
  
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
  
  
  #Data: Table for university metrics
  output$country.table <- renderDataTable({
    
  })
  
  
})