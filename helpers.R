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

compuni <- function(df, yvar) {
  compuni <- gvisColumnChart(df,
                               xvar="new_name", 
                               yvar=yvar,
                               options=list(height=500))
  compuni
}