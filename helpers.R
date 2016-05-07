split_rank <- function(rank) {
  if (grepl('-',rank)) {
    x = as.numeric(strsplit(rank, split='-')[[1]][1])
    y = as.numeric(strsplit(rank, split='-')[[1]][2])
    new_rank = round(x + (y-x)/2)
  } else {
    new_rank = rank
  }
  return(new_rank)
}




baruni <- function(df, yvar) {
  baruni <- gvisColumnChart(df,
                             yvar=yvar,
                             options=list(height=500,
                                          legend="{position: 'top', maxLines: 10}",
                                          vAxis='{minValue:0, maxValue:100}'))
  baruni
}



compuni <- function(df, yvar) {
  compuni <- gvisColumnChart(df,
                               xvar="new_name", 
                               yvar=yvar,
                               options=list(height=500,
                                            legend="bottom"))
  compuni
}


ranktoscore <- function(x) { 
  return((1-x/1000) * 100) #n=1000 obs in CWUR 2015
}