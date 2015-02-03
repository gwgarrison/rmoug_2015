
choro_president <- function (election_year = "2012"){
  
  data(df_president_ts)
  
  dp <- tbl_df(select(df_president_ts,region,contains(election_year)))
  
  names(dp) <- c("region","value")
  choro.p = suppressWarnings(StateChoropleth$new(dp))
  choro.p$title = paste(election_year," Election Results")
  choro.p$ggplot_scale = scale_fill_manual(name="Candidate", 
                                           values=c("blue", "red","green"), 
                                           drop=FALSE)
  #suppressMessages(choro.p$render())  
#  print(choro.p)
  suppressWarnings(choro.p)
  
}