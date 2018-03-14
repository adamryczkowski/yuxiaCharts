ts_nominal_dispatch<-function(property_accessor) {
  #browser()

  class_dep<-property_accessor$get_property('dv.vartype')
  if (class_dep=='D') {
    property_accessor$reverse_vars()
  }
  db_obj<-property_accessor$serve_db()

  dvlevels<-db_obj$dvlevels()
  if(length(dvlevels)<2) {
    return(list(plot_df=NULL, season_df=NULL))
  }

  period_value<-property_accessor$get_property('period_value')
  period_unit<-property_accessor$get_property('period_unit')
  property_accessor$set_report_dispatcher(ts_nominal_reports)

  mydt<-db_obj$chunkdf_ivdvgv()
  #browser()
  dummies_df<-setNames(lapply(dvlevels,
                              function(x){
                                as.integer(as.numeric(mydt$dv) == x)
                              }),
                       paste0('dv_', stringr::str_pad(dvlevels, width=max(nchar(dvlevels)), pad = "0")))

  dummies_df<-cbind(mydt %>% select(-dv), as_tibble(dummies_df)) %>% as_tibble %>% group_by(gv, iv)
  if(db_obj$is_grouped()) {
    dummies_df<-group_by(dummies_df, gv, iv)
  } else {
    dummies_df<-group_by(dummies_df, iv)
  }
  all_df<-dplyr::inner_join(dummies_df %>% summarise(n=n()),
                            dummies_df %>%  summarise_at(vars(starts_with('dv_')), sum),
                            by=c('gv', 'iv')) %>% data.table

  for(i in dvlevels) {
    setattr(all_df[[paste0('dv_',stringr::str_pad(i,
                                                  width=max(nchar(dvlevels)),
                                                  pad = "0"))]], 'label', names(dvlevels)[which.max(dvlevels==1)])
  }

  tidy_df<-tidyr::gather(all_df, dv, counts, -iv, -gv, -n) %>% mutate(perc=counts/n, dv=factor(dv)) %>%
    mutate(iv2=as.numeric(difftime(iv, lubridate::ymd(paste0(year(iv),'-01-01')), units = 'days'))/365 + year(iv)) %>% data.frame()


  tidy_df$dv <- plyr::revalue(tidy_df$dv,setNames(names(dvlevels),
                                                  paste0('dv_',stringr::str_pad(dvlevels,
                                                                                width=max(nchar(dvlevels)),
                                                                                pad = "0"))))

  plot_df<-tidy_df %>% group_by(gv, dv) %>%  tidyr::nest() %>%  mutate(freqs=map(data, decompose_freqs)) %>% select(-data) %>% tidyr::unnest()


  if(period_value == '') {
    period_value=1
  }
  if(period_unit == '') {
    period_unit='year'
  }

  season_df<-create_season_df(mydt, date_var='iv', factor_var = 'dv', groupby = 'gv',
                              period_value = period_value, period_unit = period_unit)

  return(list(plot_df=plot_df, season_df=season_df))
}


ts_nominal_reports<-function(pAcc, statistics) {
  #browser()
  db_obj<-pAcc$serve_db()

  dvlevels<-db_obj$dvlevels()

  if(length(dvlevels)<2) {

    language<-pAcc$get_property('language')
    pAcc$done_discovery()
    if(language=='PL') {
      msg<-paste0("Nie udało się wykonać żadnych analiz, bo w grupie ",   db_obj$filter_label(), " są mniej niż dwa poziomy zmiennej ", db_obj$depvar_label())
    } else {
      msg<-paste0("Cannot perform any analyses, because in ",   db_obj$filter_label(), " there are less than two levels of variable ", db_obj$depvar_label())
    }
    return(list(error=function(pAcc, statistics, chapter) {chapter$insert_paragraph(msg)}))
  }
  pAcc$done_discovery()

  plots<-list(
    periodogram_nominal=function(pAcc, statistics, chapter) plot_periodogram(pAcc, chapter, plot_df = statistics$plot_df),
    plot_seasons=function(pAcc, statistics, chapter) plot_seasons(pAcc, chapter, season_df = statistics$season_df)
  )
  return(plots)

}


decompose_freqs<-function(myts) {
  max_period<-floor(nrow(myts)/3)

  myts2<-ts(myts$counts, frequency = max_period,  start=year(min(myts$iv)))

  my_mean<-mean(myts$counts)

  rap_stl<-stl(myts2, s.window = 'periodic')
  df <- as.data.frame(rap_stl$time.series)
  df<-cbind(myts, seasonal = df$seasonal+my_mean, trend = df$trend, remainder=df$remainder+my_mean)
  return(as_tibble(df))
}
