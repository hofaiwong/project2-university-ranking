library(shiny)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(shinythemes)

## ui.R ##
shinyUI(
  navbarPage(
    title = "World University Rankings",
    id = "nav",
    theme = shinytheme("united"),

    tabPanel("Countries",
  
              fluidRow(
                column(width = 3,
                       h4("Ranking organization"),
                    radioButtons("sourceCountry",
                                 "Select organization",
                                 choices = list("Shanghai Rankings" = 1,
                                                "Times World University Rankings" = 2,
                                                "Center for World University Rankings" = 3),
                                 selected = 1),
                    
                    conditionalPanel(
                      condition = "input.sourceCountry == 1", #Shanghai
                      h4("Shanghai ranking metrics"),
                      
                      radioButtons("selectedStat_shanghai",
                                   "Select metric",
                                   choices = list("Top rank" = 1, 
                                                  "Median rank" = 2,
                                                  "Count of ranked universities" = 3,
                                                  "Mean alumni score" = 5,
                                                  "Mean award score" = 6,
                                                  "Mean HiCi score" = 7,
                                                  "Mean N&S score" = 8,
                                                  "Mean PUB score" = 9,
                                                  "Mean PCP score" = 10),
                                   selected = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.sourceCountry == 2", #Times
                      h4("Times ranking metrics"),
                      
                      radioButtons("selectedStat_times",
                                   "Select metric",
                                   choices = list("Top rank" = 1, 
                                                  "Median rank" = 2,
                                                  "Count of ranked universities" = 3,
                                                  "Mean teaching score" = 5,
                                                  "Mean international score" = 6,
                                                  "Mean research score" = 7,
                                                  "Mean citations score" = 8,
                                                  "Mean income score" = 9),
                                   selected = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.sourceCountry == 3", #CWUR
                      h4("CWUR ranking metrics"),
                      
                      radioButtons("selectedStat_cwur",
                                   "Select metric",
                                   choices = list("Top rank" = 1, 
                                                  "Median rank" = 2,
                                                  "Count of ranked universities" = 3,
                                                  "Mean education score" = 5,
                                                  "Mean alumni employment score" = 6,
                                                  "Mean faculty score" = 7,
                                                  "Mean publications score" = 8,
                                                  "Mean influence score" = 9,
                                                  "Mean citations score" = 10,
                                                  "Mean broad impact score" = 11,
                                                  "Mean patents score" = 12),
                                   selected = 1)
                    )
                    
                    ),
                
                column(width = 9,
                    tabsetPanel(
                      tabPanel("Map",
                      htmlOutput("country.map")
                      ),
                      tabPanel("Data",
                        div(dataTableOutput("country.table"), style = "font-size:80%")
                        )
                      )
                    )
              )
             ),

      
      tabPanel("Universities",
              fluidRow(
                column(width = 3,
                       h4("Ranking organization"),
                       radioButtons("sourceUni",
                                    "Select source",
                                    choices = list("Shanghai Rankings" = 1,
                                                   "Times World University Rankings" = 2,
                                                   "Center for World University Rankings" = 3),
                                    selected = 1),
                       
                       conditionalPanel(
                         condition = "input.sourceUni == 1", #Shanghai
                         sliderInput("s.sh.rank", "Shanghai rank:", min=0, max=500, value=c(1,100)),
                         sliderInput("s.sh.alumni", "Alumni score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.sh.award", "award score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.sh.hici", "hici score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.sh.ns", "ns score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.sh.pub", "pub score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.sh.pcp", "pcp score:", min=0, max=100, value=c(0,100))
                       ),
                       
                       conditionalPanel(
                         condition = "input.sourceUni == 2", #Times
                         sliderInput("s.t.rank", "Times rank:", min=0, max=400, value=c(1,100)),
                         sliderInput("s.t.teaching", "Teaching score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.t.international", "International score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.t.research", "Research score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.t.citations_times", "Citations score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.t.income", "Income score:", min=0, max=100, value=c(0,100))
                       ),
                       
                       conditionalPanel(
                         condition = "input.sourceUni == 3", #CWUR
                         sliderInput("s.c.rank", "CWUR rank:", min=0, max=1000, value=c(1,100)),
                         sliderInput("s.c.education", "Education score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.alumni", "Alumni employment score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.faculty", "Faculty score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.pub", "Publications score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.influence", "Influence score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.citations", "Citations score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.impact", "Broad impact score:", min=0, max=100, value=c(0,100)),
                         sliderInput("s.c.patents", "Patents score:", min=0, max=100, value=c(0,100))
                       )
                ),
                column(width = 9,
                       tabsetPanel(
                         tabPanel("Map",
                                  leafletOutput("unimap")
                                  # fluidRow(width=12,
                                  #          selectizeInput('selectUni',
                                  #                         'Select university',
                                  #                         universities)),
                                  # fluidRow(width=12,
                                  #   box(width = 4,
                                  #       "box1",
                                  #       htmlOutput("bar.shanghai")
                                  #       ),
                                  #   box(width = 4,
                                  #       "box2",
                                  #       htmlOutput("bar.times")),
                                  #   box(width = 4,
                                  #       "box3",
                                  #       htmlOutput("bar.cwur"))
                                  # )
                         ),
                         tabPanel("Data",
                                  div(dataTableOutput("uni.table"), style = "font-size:80%")
                         )
                       )
                )
              )
      ),


    tabPanel("Uni Profile",
             fluidRow(
               column(width = 12,
                      selectizeInput('selectUni',
                                     'Select university',
                                     universities),
                      fluidRow(width=12,
                               box(width = 4,
                                              "Shanghai Scores",
                                              htmlOutput("bar.shanghai")
                                          ),
                                          box(width = 4,
                                              "Times Scores",
                                              htmlOutput("bar.times")),
                                          box(width = 4,
                                              "CWUR Scores*",
                                              htmlOutput("bar.cwur"))
                                 )
             ))),    
    
    
    
      tabPanel("Compare",
              fluidRow(
                column(3,
                       h4("Select universities for comparison"),
                    selectizeInput("uni1",
                                   "Select university 1",
                                   universities),
                    selectizeInput("uni2",
                                   "Select university 2",
                                   universities),
                    selectizeInput("uni3",
                                   "Select university 3",
                                   universities)
                    ),
                column(9,
                    tabsetPanel(
                      tabPanel("Shanghai Scores",
                               htmlOutput("compare.shanghai")),
                      tabPanel("Times Scores",
                               htmlOutput("compare.times")),
                      tabPanel("CWUR Rankings",
                               htmlOutput("compare.cwur"))
                      )
                    )
                )
              )
    )
  )