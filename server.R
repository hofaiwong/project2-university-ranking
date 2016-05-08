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
    
    #Y-axis sliders
    if (input$sourceScatterY==1) { #Shanghai
      df = rankings %>%
        filter(rank_shanghai>=input$y.sh.rank[1],
               rank_shanghai<=input$y.sh.rank[2],
               alumni>=input$y.sh.alumni[1],
               alumni<=input$y.sh.alumni[2],
               award>=input$y.sh.award[1],
               award<=input$y.sh.award[2],
               hici>=input$y.sh.hici[1],
               hici<=input$y.sh.hici[2],
               ns>=input$y.sh.ns[1],
               ns<=input$y.sh.ns[2],
               pub>=input$y.sh.pub[1],
               pub<=input$y.sh.pub[2],
               pcp>=input$y.sh.pcp[1],
               pcp<=input$y.sh.pcp[2]
        )
    } else if (input$sourceScatterY==2) { #Times
      df = rankings %>%
        filter(rank_times>=input$y.t.rank[1],
               rank_times<=input$y.t.rank[2],
               teaching>=input$y.t.teaching[1],
               teaching<=input$y.t.teaching[2],
               international>=input$y.t.international[1],
               international<=input$y.t.international[2],
               research>=input$y.t.research[1],
               research<=input$y.t.research[2],
               citations_times>=input$y.t.citations_times[1],
               citations_times<=input$y.t.citations_times[2],
               income>=input$y.t.income[1],
               income<=input$y.t.income[2]
        )
    } else if (input$sourceScatterY==3) {
      df = rankings %>%
        filter(rank_cwur>=input$y.c.rank[1],
               rank_cwur<=input$y.c.rank[2],
               quality_of_education>=input$y.c.education[1],
               quality_of_education<=input$y.c.education[2],
               alumni_employment>=input$y.c.alumni[1],
               alumni_employment<=input$y.c.alumni[2],
               quality_of_faculty>=input$y.c.faculty[1],
               quality_of_faculty<=input$y.c.faculty[2],
               publications>=input$y.c.pub[1],
               publications<=input$y.c.pub[2],
               influence>=input$y.c.influence[1],
               influence<=input$y.c.influence[2],
               citations_cwur>=input$y.c.citations[1],
               citations_cwur<=input$y.c.citations[2],
               broad_impact>=input$y.c.impact[1],
               broad_impact<=input$y.c.impact[2],
               patents>=input$y.c.patents[1],
               patents<=input$y.c.patents[2])
    }
    
    #X-axis sliders
    if (input$sourceScatterX==1) { #Shanghai
      df = df %>%
        filter(rank_shanghai>=input$x.sh.rank[1],
               rank_shanghai<=input$x.sh.rank[2],
               alumni>=input$x.sh.alumni[1],
               alumni<=input$x.sh.alumni[2],
               award>=input$x.sh.award[1],
               award<=input$x.sh.award[2],
               hici>=input$x.sh.hici[1],
               hici<=input$x.sh.hici[2],
               ns>=input$x.sh.ns[1],
               ns<=input$x.sh.ns[2],
               pub>=input$x.sh.pub[1],
               pub<=input$x.sh.pub[2],
               pcp>=input$x.sh.pcp[1],
               pcp<=input$x.sh.pcp[2]
        )
    } else if (input$sourceScatterX==2) { #Times
      df = df %>%
        filter(rank_times>=input$x.t.rank[1],
               rank_times<=input$x.t.rank[2],
               teaching>=input$x.t.teaching[1],
               teaching<=input$x.t.teaching[2],
               international>=input$x.t.international[1],
               international<=input$x.t.international[2],
               research>=input$x.t.research[1],
               research<=input$x.t.research[2],
               citations_times>=input$x.t.citations_times[1],
               citations_times<=input$x.t.citations_times[2],
               income>=input$x.t.income[1],
               income<=input$x.t.income[2]
        )
    } else if (input$sourceScatterX==3) {
      df = rankings %>%
        filter(rank_cwur>=input$x.c.rank[1],
               rank_cwur<=input$x.c.rank[2],
               quality_of_education>=input$x.c.education[1],
               quality_of_education<=input$x.c.education[2],
               alumni_employment>=input$x.c.alumni[1],
               alumni_employment<=input$x.c.alumni[2],
               quality_of_faculty>=input$x.c.faculty[1],
               quality_of_faculty<=input$x.c.faculty[2],
               publications>=input$x.c.pub[1],
               publications<=input$x.c.pub[2],
               influence>=input$x.c.influence[1],
               influence<=input$x.c.influence[2],
               citations_cwur>=input$x.c.citations[1],
               citations_cwur<=input$x.c.citations[2],
               broad_impact>=input$x.c.impact[1],
               broad_impact<=input$x.c.impact[2],
               patents>=input$x.c.patents[1],
               patents<=input$x.c.patents[2])
    }
    
    df = df %>%
      select(switch(input$sourceScatterX,
                    '1' = rank_shanghai,
                    '2' = rank_times,
                    '3' = rank_cwur),
             switch(input$sourceScatterY,
                    '1' = rank_shanghai,
                    '2' = rank_times,
                    '3' = rank_cwur),
             University = new_name, Country = country)
    
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
    
    
    if (input$sourceScatterY!=input$sourceScatterX) {
      max=max(df[,1:2], na.rm=T)
      
      #Draw scatterplot
      plot_ly(df, x = df[,1], y = df[,2],
              text = paste(University,
                           paste0(org.y[[2]],': ',org.y[[1]]),
                           paste0(org.x[[2]],': ',org.x[[1]]), sep='<br>'),
              mode = "markers",
              color = Country,
              colors = 'OrRd')
      layout(xaxis = xaxis,
             yaxis = yaxis,
             title = paste(org.y[[2]],'by',org.x[[2]], sep=' ')) %>%
        add_trace(x = c(0,max), y = c(0,max), mode = "line", color='black',
                  showlegend=F, name='')
    } else {
      max=max(df[,1], na.rm=T)
      
      #Draw scatterplot
      plot_ly(df, x = df[,1], y = df[,1],
              text = paste(University,
                           paste0(org.y[[2]],': ',org.y[[1]]),
                           paste0(org.x[[2]],': ',org.x[[1]]), sep='<br>'),
              mode = "markers",
              color = Country,
              colors = 'OrRd')
      layout(xaxis = xaxis,
             yaxis = yaxis,
             title = paste(org.y[[2]],'by',org.x[[2]], sep=' ')) %>%
        add_trace(x = c(0,max), y = c(0,max), mode = "line", color='black',
                  showlegend=F, name='')
    }
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