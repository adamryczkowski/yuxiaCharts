create_season_df<-function(dt, date_var, factor_var, groupby=NULL, period_value, period_unit) {
  mydf<-make_seasonal_nominal_df(dt, date_var = date_var, factor_var = factor_var,
                                 groupby = groupby,
                                 period_value = period_value, period_unit = period_unit)

  if(!is.null(groupby)) {
    grs<-c('groupby','factor_var')
  } else {
    grs<-'factor_var'
  }

  mydt<-mydf %>% group_by_(.dots = c(grs, 'small_gr')) %>%
    summarize(m = mean(counts, na.rm=TRUE),
              n=n(),
              sd=sd(counts, na.rm=TRUE),
              ci=qt(0.975,df=n-1)*sd/sqrt(n))


  #Powinienem na tym etapie użyć bayesowskiego modelu hierarchicznego, aby
  #połączyć niepewność związaną z pojedynczym pomiarem (r. dirichleta) i
  #wariancją wewnątrz danego miesiąca/innego punktu czasowego.
  #
  #Zastosuję tylko sd.


  number_of_samples<-1000
  calc_errors<-function(count_df) {
    #    browser()
    sample<-gtools::rdirichlet(number_of_samples, count_df$counts)
    count_df$sd<-apply(sample, 2, sd)
    count_df$m<-apply(sample,2,mean)
    return(count_df)
  }
  if(!is.null(groupby)) {
    a<-mydf %>% group_by(groupby, big_gr,small_gr)
  } else {
    a<-mydf %>% group_by(big_gr,small_gr)
  }
  a<-a %>% do(calc_errors(.))


  #  debugonce(get_mean_sd)
  #  pr<-readRDS('tmp_nominal_seasons.rds')
  #  model<-gen_mean_sd_model()
  if(!is.null(groupby)) {
    pr<-a %>% group_by(groupby, small_gr, factor_var) %>% select(perc, sd, big_gr)
  } else {
    pr<-a %>% group_by(small_gr, factor_var) %>% select(perc, sd, big_gr)
  }

  #  saveRDS(pr, 'tmp_nominal_seasons.rds')

  means<-get_mean_sd_multiple(pr, gr_var='big_gr', m_var='perc', sd_var='sd')
  #  pr<-pr %>% do(get_mean_sd(.$perc, .$sd, model=model))

  if(!is.null(groupby)) {
    pr <- means %>% group_by(groupby, small_gr)
  } else {
    pr <- means %>% group_by(small_gr)
  }
  pr<-pr %>% arrange(-as.numeric(factor_var)) %>%
    mutate(cum_m = cumsum(m), cil=cum_m-m_se, ciu=cum_m+m_se, cum_m_b=lag(cum_m, default=0))
  setattr(pr$small_gr, 'units', attr(mydf$small_gr, 'units'))

  return(pr)
}


all_time_units<-c('year', 'quarter', 'month', 'week', 'day',
                  'hour', 'minute', 'sec')
all_time_units_names_pl_m<-c('rok', 'kwartał', 'miesiąc', 'tydzień', 'dzień',
                             'godzina', 'minuta', 'sekunda')
all_time_units_names_pl_d<-c('lata', 'kwartały', 'miesiące', 'tygodnie', 'dni',
                             'godziny', 'minuty', 'sekundy')
all_time_units_names_pl_mn<-c('lat', 'kwartałów', 'miesięcy', 'tygodni', 'dni',
                              'godzin', 'minut', 'sekund')
all_time_units_names_pl_mn_m<-c('lata', 'kwartały', 'miesiące', 'tygodnie', 'dni',
                                'godziny', 'minuty', 'sekundy')

next_units<-c(year='',
              quarter='year', month='year', week='year',
              day='month',
              hour='day', minute='hour', sec='minute') #Która następna jednostka jest
#potrzebna, aby stworzyć ładną datę? Np. gdy zaczynamy z "day", to next_unit to month a nie week. Bo chcemy mieć
#datę sformatowaną jako "12 marca" a nie "3 dzień i 20 tydzień"
atu_len<-c(31557600, 7889400, 2629800, 604800, 86400, 3600, 60, 1)
#main_time_units służą do pisania ładnych zakresów dat i dlatego zawierają tylko te jednostki,
#które są NIEZBĘDNE do wyprodukowania czytelnej daty

unit_labels_year<-function(values, language='PL', next_unit='') {
  as.character(values) #Year gets simply converted to string
}

cast_date_to_year<-function(dates, next_unit='') {
  return(lubridate::year(dates))
}

reference_sequence_year<-function(date_from, date_to, step, flag_labels=FALSE, next_unit=''){
  year_from=year(date_from)
  year_to=year(date_to)
  if(flag_labels) {
    return(seq(year_from, year_to+1, by=step))
  }
  return(as.Date(paste0(seq(year_from, year_to+1, by = step),'-01-01')))
}

unit_labels_quarter<-function(values, language='PL', next_unit='year', type='astronomical') {
  if(next_unit!='year') {
    stop("Incompatible next_unit: year")
  }
  if(type %in% c('astronomical', 'meteorological')){
    if(language=='PL') {
      labels<-c('zima', 'wiosna','lato', 'jesień')
    } else if (language=='EN') {
      labels<-c('winter', 'spring','summer', 'autumn')
    } else {
      stop(paste0("Unknown language ", language))
    }
  } else {
    labels<-paste0('Q',1:4)
  }
  return(labels[values])
}


cast_date_to_quarter<-function(dates, next_unit='year', type='astronomical') {
  library(lubridate)
  ref_quarters<-reference_sequence_quarter(min(dates), max(dates), step = 1, type = type)
  intervals<-lubridate::interval(ref_quarters[-length(ref_quarters)], ref_quarters[-1]-period(1, 'day'))
  ref_quarters_lab<-reference_sequence_quarter(min(dates), max(dates), step = 1, type = type, flag_labels = TRUE)

  season_pos<-map_int(dates, ~which(. %within% intervals) )
  return(ref_quarters_lab[season_pos])
}

get_seasons_by_type<-function(type) {
  if(type=='astronomical') {
    seasons<-function(year) {
      return(c(as.Date(paste0(year-1, '-12-22')),
               as.Date(paste0(year, '-03-20')),
               as.Date(paste0(year, '-06-21')),
               as.Date(paste0(year, '-09-22'))))
    }
  } else if (type =='meteorological') {
    seasons<-function(year) {
      return(c(as.Date(paste0(year-1, '-12-01')),
               as.Date(paste0(year, '-03-01')),
               as.Date(paste0(year, '-06-01')),
               as.Date(paste0(year, '-09-01'))))
    }
  } else if (type=='quarters') {
    seasons<-function(year) {
      return(c(as.Date(paste0(year, '-01-01')),
               as.Date(paste0(year, '-04-01')),
               as.Date(paste0(year, '-07-01')),
               as.Date(paste0(year, '-10-01'))))
    }
  } else {
    stop(paste0("Unknown season type: ", type, ". Available values: astronomical, meteorological, quarters."))
  }
  return(seasons)
}


# type %in% c('astronomical', 'meteorological','quarters')
reference_sequence_quarter<-function(date_from, date_to, step, flag_labels=FALSE, next_unit='year', type='astronomical'){
  year_from=year(date_from)
  year_to=year(date_to)
  seasons<-get_seasons_by_type(type)

  ref_range<-Reduce(c,purrr::map(seq(year_from, year_to+1), seasons))
  pos_from<-max(1,which.min(ref_range<date_from)-1)
  pos_to<-min(length(ref_range), which.max(ref_range>date_to))
  if(flag_labels) {
    if(next_unit!='year') {
      stop("Incompatible next_unit: year")
    }
    labels<-1:4
    ref_range_lab<-rep(labels, (year_to - year_from + 3))
    ans<-ref_range_lab[seq(pos_from, pos_to, by=step)]
  } else {
    ans<-ref_range[seq(pos_from, pos_to, by=step)]
  }
  return(ans)
}

unit_labels_month<-function(values, language='PL', next_unit='year') {
  if(next_unit!='year') {
    stop("Incompatible next_unit: year")
  }
  if(language=='PL') {
    labels<-c('Sty','Lut','Mar','Kwi','Maj','Cze','Lip','Sie','Wrz','Paź','Lis','Gru')
  } else if (language=='EN') {
    labels<-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  } else {
    stop(paste0('Unknown language ', language))
  }
  return(labels[values])
}

cast_date_to_month<-function(dates, next_unit='year') {
  return(lubridate::month(dates))
}

reference_sequence_month<-function(date_from, date_to, step, flag_labels=FALSE, next_unit='year'){
  year_from=year(date_from)
  year_to=year(date_to)

  make_months<-function(year) {as.Date(paste0(year,'-',1:12,'-01'))}
  ref_range<-Reduce(c,purrr::map(seq(year_from, year_to+1), make_months))
  pos_from<-max(1,which.min(ref_range<date_from)-1)
  pos_to<-min(length(ref_range), which.max(ref_range>date_to))
  if(flag_labels) {
    if(next_unit!='year') {
      stop("Incompatible next_unit: year")
    }
    labels<-1:12
    ref_range_lab <- rep(labels, year_to - year_from + 3)
    ans<-ref_range_lab[seq(pos_from, pos_to, by=step)]
  } else {
    ans<-ref_range[seq(pos_from, pos_to, by=step)]
  }
  return(ans)
}

unit_labels_week<-function(values, language='PL', next_unit='year') {
  if(next_unit!='year') {
    stop("Incompatible next_unit: year")
  }
  if(language=='PL') {
    labels<-paste0("tyg. ", 1:53)
  } else if (language=='EN') {
    labels<-paste0("week ", 1:53)
  } else {
    stop(paste0('Unknown language ', language))
  }
  return(labels[values])
}

cast_date_to_week<-function(dates, next_unit='year', flag_sunday_first_day_of_week=FALSE) {
  if (flag_sunday_first_day_of_week) {
    return(lubridate::wday(dates))
  } else {
    return((lubridate::wday(dates) %% 7)+1)
  }
}

reference_sequence_week<-function(date_from, date_to, step, flag_labels=FALSE, next_unit='year',
                                  flag_sunday_first_day_of_week=FALSE){
  ref_dates<-as.Date(seq.POSIXt(as.POSIXct(date_from),
                                as.POSIXct(date_to)+lubridate::period('1 week'), by='week'))

  start_weekday<- lubridate::wday(ref_dates[[1]])
  if(flag_sunday_first_day_of_week) {
    ref_range<-ref_dates - lubridate::period(start_weekday-1, 'day')
  } else {
    ref_range<-ref_dates - lubridate::period(start_weekday, 'day')
  }
  pos_from<-max(1,which.min(ref_range<date_from)-1)
  pos_to<-min(length(ref_range), which.max(ref_range>date_to))
  ans<-ref_range[seq(pos_from, pos_to, by=step)]
  if(flag_labels) {
    if(next_unit!='year') {
      stop("Incompatible next_unit: year")
    }


    ans<-labels[lubridate::wday(ans)]
  }
  return(ans)
}

unit_labels_day<-function(values, language='PL', next_unit='month') {
  if(next_unit=='week') {
    if(language=='PL') {
      labels<-c('Ni', 'Pn', 'Wt', 'Śr', 'Cz', 'Pi', 'So')
    } else if (language=='EN') {
      labels<-c('Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa')
    } else if (language=='') {
      labels<-1:7
    } else {
      stop(paste0('Unknown language ', language))
    }
  } else if(next_unit=='month') {
    labels<-as.character(1:31)
  } else if(next_unit=='year') {
    labels<-as.character(1:366)
  } else {
    stop(paste0("Incompatible next_unit: ",next_unit))
  }
  return(labels[values])
}

cast_date_to_day<-function(dates, next_unit='month') {
  if(next_unit=='year') {
    ans<-lubridate::yday(dates)
  } else if(next_unit=='month') {
    ans<-lubridate::day(dates)
  } else if(next_unit=='week') {
    ans<-(lubridate::wday(today()) - 2) %%7 +1
  } else {
    stop(paste0("Incompatible next_unit: ", next_unit))
  }
}

reference_sequence_day<-function(date_from, date_to, step, flag_labels=FALSE, next_unit='month') {
  ans<-as.Date(seq.POSIXt(as.POSIXct(date_from),
                          as.POSIXct(date_to)+lubridate::period('1 day'),
                          by=paste0(step, ' day')))
  if(flag_labels) {
    ans<-cast_date_to_day(ans, next_unit=next_unit)
  }
  return(ans)
}

unit_labels_hour<-function(values, language='PL', next_unit='day') {
  if(next_unit=='day') {
    if(language=='PL') {
      labels<-as.character(0:23)
    } else if (language=='EN') {
      labels<-c(paste0(1:12, " AM"), paste0(1:12, " PM"))
    } else {
      stop(paste0('Unknown language ', language))
    }
  } else {
    stop(paste0("Incompatible next_unit: ",next_unit))
  }
  return(labels[values])
}

cast_date_to_hour<-function(dates, next_unit='day') {
  return(lubridate::hour(dates)+1)
}

reference_sequence_hour<-function(date_from, date_to, step, flag_labels=FALSE, language='PL', next_unit='day') {
  ans<-seq.POSIXt(as.POSIXct(date_from),
                  as.POSIXct(date_to)+lubridate::period('1 hour'),
                  by=paste0(step, ' hour'))
  if(flag_labels) {
    ans<-cast_date_to_hour(ans, next_unit=next_unit)
  }
  return(ans)
}

unit_labels_minute<-function(values, language='PL', next_unit='hour') {
  if(next_unit=='hour') {
    labels<-as.character(0:59)
  } else {
    stop(paste0("Incompatible next_unit: ",next_unit))
  }
  return(labels[values])
}

cast_date_to_minute<-function(dates, next_unit='hour') {
  return(lubridate::minute(dates)+1)
}

reference_sequence_minute<-function(date_from, date_to, step, flag_labels=FALSE, language='PL', next_unit='hour') {
  ans<-seq.POSIXt(as.POSIXct(date_from),
                  as.POSIXct(date_to)+lubridate::period('1 minute'),
                  by=paste0(step, ' min'))
  if(flag_labels) {
    ans<-cast_date_to_minute(ans, next_unit=next_unit)
  }
  return(ans)
}

unit_labels_second<-function(values, language='PL', next_unit='minute') {
  if(next_unit=='minute') {
    labels<-as.character(0:59)
  } else {
    stop(paste0("Incompatible next_unit: ",next_unit))
  }
  return(labels[values])
}

cast_date_to_second<-function(dates, next_unit='minute') {
  return(lubridate::second(dates)+1)
}

reference_sequence_sec<-function(date_from, date_to, step, flag_labels=FALSE, language='PL', next_unit='hour') {
  ans<-seq.POSIXt(as.POSIXct(date_from),
                  as.POSIXct(date_to)+lubridate::period('1 sec'),
                  by=paste0(step, 'sec'))
  if(flag_labels) {
    ans<-cast_date_to_second(ans, next_unit=next_unit)
  }
  return(ans)
}

reference_sequences<-list(year=reference_sequence_year, quarter=reference_sequence_quarter,
                          month=reference_sequence_month, week=reference_sequence_week,
                          day=reference_sequence_day,hour=reference_sequence_hour,
                          minute=reference_sequence_minute, sec=reference_sequence_sec)

reference_labels<-list(year=unit_labels_year, quarter=unit_labels_quarter,
                       month=unit_labels_month, week=unit_labels_week,
                       day=unit_labels_day,hour=unit_labels_hour,
                       minute=unit_labels_minute, sec=unit_labels_second)

cast_date_to_unit_fn<-list(year=cast_date_to_year, quarter=cast_date_to_quarter,
                           month=cast_date_to_month , week=cast_date_to_week ,
                           day=cast_date_to_day, hour=cast_date_to_hour,
                           minute=cast_date_to_minute, sec=cast_date_to_second)



reference_sequence<-function(date_from, date_to, unit, step,...) {
  ref_fn<-reference_sequences[[unit]]
  if(is.null(ref_fn)) {
    stop(paste0("Unkown unit ", unit))
  }
  return(ref_fn(date_from=date_from, date_to=date_to, step=step, flag_labels=FALSE,...))
}

reference_sequence_labels<-function(date_from, date_to, unit, step, language='PL', next_unit=NULL,  ...) {
  ref_fn<-reference_sequences[[unit]]
  if(is.null(ref_fn)) {
    stop(paste0("Unkown unit ", unit))
  }
  if(is.null(next_unit)) {
    next_unit<-next_units[[unit]]
  }
  values<-ref_fn(date_from=date_from, date_to=date_to, step=step, flag_labels=TRUE, next_unit=next_unit, ...)
  if(language!='') {
    lab_fn<-reference_labels[[unit]]
    labels<-lab_fn(values, language=language, next_unit=next_unit, ...)
    return(labels)
  } else{
    return(values)
  }
}

main_time_units<-c('year', 'month', 'day',  'hour', 'minute', 'sec') #Main time units are used to generate dates

cast_date_to_time_unit<-function(dates, unit, language='', next_unit=NULL, ...) {
  fn<-cast_date_to_unit_fn[[unit]]
  if(is.null(fn)) {
    stop(paste0("Cannot find unit ", unit))
  }
  if(is.null(next_unit)) {
    next_unit<-next_units[[unit]]
  }
  values<-fn(dates, next_unit=next_unit, ...)
  if(language!='') {
    lab_fn<-reference_labels[[unit]]
    labels<-lab_fn(values, language=language, next_unit=next_unit, ...)
    return(labels)
  } else{
    return(values)
  }
}


#Zwraca optymalną jednostkę czasu, jaką należy zaznaczyć na osi czasu wykresu,
#aby przedstawić odstęp czasu unit_span
first_significant_time_unit_from_period<-function(unit_span, lo_threshold=0.6) {
  if(as.numeric(unit_span, 'year')>lo_threshold) {
    return('year')
  } else if (as.numeric(unit_span, 'month')>3*lo_threshold) {
    return('quarter')
  } else if (as.numeric(unit_span, 'month')>lo_threshold) {
    return('month')
  } else if (as.numeric(unit_span, 'week')>lo_threshold) {
    return('week')
  } else if (as.numeric(unit_span, 'day')>lo_threshold) {
    return('day')
  } else if (as.numeric(unit_span, 'hour')>lo_threshold) {
    return('hour')
  } else if (as.numeric(unit_span, 'minute')>lo_threshold) {
    return('minute')
  } else if (as.numeric(unit_span, 'second')>lo_threshold) {
    return('sec')
  } else {
    stop("Not supported sub-second time resolution")
  }
}

#Zwraca optymalną jednostkę czasu, jaką należy zaznaczyć na osi czasu wykresu,
#aby przedstawić odstęp czasu unit_span
amount_of_time_units<-function(all_dates, ref_date, date_unit) {
  intervals<-lubridate::interval(start = ref_date, end = all_dates)
  if(date_unit == 'quarter') {
    ans<-as.numeric(intervals, 'month') / 3
  } else {
    ans<-as.numeric(intervals, date_unit)
  }
  return(ans)
}

#Funkcja zwraca liczbę, przez którą należy przemnożyc jednostkę value_comp [unit_comp], aby
#dostać value_ref [unit_ref]
period_factor<-function(unit_ref, value_ref, unit_comp, value_comp) {
  ref_pos <- match(unit_ref, all_time_units)
  if(is.na(ref_pos)) {
    stop(paste0("Cannot find unit ", unit_ref))
  }
  comp_pos <- match(unit_comp, all_time_units)
  if(is.na(comp_pos)) {
    stop(paste0("Cannot find unit ", unit_comp))
  }

  ref_len<-atu_len[[ref_pos]]
  comp_len<-atu_len[[comp_pos]]

  return(ref_len/comp_len * value_ref/value_comp)
}

decrease_time_unit<-function(unit) {
  pos<-match(unit, all_time_units)
  if (is.na(pos)){
    stop(paste0("Unknown time unit ", unit))
  }
  if (pos == length(all_time_units)) {
    warning(paste0("Cannot decrease unit ", unit, " any further"))
    browser()
    return(unit)
  }
  return(all_time_units[[pos+1]])
}















make_seasonal_df<-function(dt, date_var, dep_var, groupby,  period_value=1, period_unit='years',
                           date_var_label=NULL, small_unit, flag_return_date_gr=FALSE) {
#  browser()
  if(is.null(date_var_label)) {
    date_var_label<-as.character(danesurowe::GetVarLabel(dt, date_var, quote_varname = '`'))
  }

  if(is.null(groupby)) {
    nowe_nazwy<-setNames(c(date_var, dep_var), c('date_var', 'dep_var'))
    grs<-character(0)
  } else {
    nowe_nazwy<-setNames(c(date_var, dep_var, groupby), c('date_var', 'dep_var', 'groupby'))
    grs<-c('groupby')
  }
  mydt<-dt %>% select_(.dots=nowe_nazwy) %>% na.omit

  all_dates <- (mydt %>% select(date_var))[[1]]
  date_density<-unique_date_density_uniform(all_dates, period_value = period_value, period_unit=period_unit)
  natural_unit<-infer_natural_base_time_unit(date_density, period_value, period_unit,1)
  #Mając jednostkę bazową, należy

  if(natural_unit=='quarter') {
    nominator<-lubridate::period(3, 'month')
  } else {
    nominator<-lubridate::period(1, natural_unit)
  }

  tick_count<-suppressWarnings(lubridate::period(period_value, period_unit)/nominator)

  range<-genTicks(range_from = 0, range_to = tick_count, maxCount = date_density)

  var_gr<-gen_time_factor(all_dates =  all_dates,
                          period_value = period_value, period_unit=period_unit,
                          small_value = range$step, small_unit=natural_unit)
  var_gr2<-gen_one_time_factor(all_dates = all_dates, period_value = 1, period_unit = natural_unit)
  mydt<-cbind(mydt, big_gr=var_gr$big_gr, small_gr=var_gr$small_gr, detail_gr=var_gr2$time_gr)

  setattr(mydt$small_gr, 'units', natural_unit)
  setattr(mydt$big_gr, 'units', period_unit)


  if(flag_return_date_gr) {
    return(list(mydt=mydt, var_gr=var_gr2, small_unit=natural_unit))
  }else {
    return(mydt)

  }

}






#Funkcji zadaje się, jaki jest okres sezonowości (domyślnie 1 rok),
#jaką zmienną należy agregować i na jakie grupować.
#Funkcja sama wymyśla adekwatny krok czasowy, na jaki należy podzielić zbiór danych,
#i tworzy tabelę z agregacjami i czynnikiem "timegr", który nadaje się do wyświetlania
#na wykresie.
make_seasonal_nominal_df<-function(dt, date_var, factor_var, groupby,  period_value=1, period_unit='years',
                                   date_var_label=NULL, small_unit) {
  # dt<-read_all()
  # date_var<-'birth'
  # factor_var<-'m_haplotyp'
  # groupby<-'sex'
  # period_value=1
  # period_unit='year'
  # date_var_label<-NULL

  grs=character(0)
  if(!is.null(groupby)){
    grs<-'groupby'
  }

  mydf<-make_seasonal_df(dt = dt, date_var = date_var, dep_var = factor_var, groupby = groupby,
                         period_value = period_value, period_unit = period_unit,
                         date_var_label = date_var_label, small_unit = small_unit) %>% select(-detail_gr, factor_var=dep_var)

  factor_levels<-danesurowe::GetLevels(dt[[factor_var]])
  dummies_df<-setNames(lapply(factor_levels,
                              function(x){
                                as.integer(as.numeric(mydf$factor_var) == x)
                              }),
                       paste0('.factor_', stringr::str_pad(factor_levels, width=max(nchar(factor_levels)), pad = "0")))



  dummies_df<-cbind(mydf, as_tibble(dummies_df)) %>%
    select(-factor_var) %>% as_tibble %>% group_by_(.dots=c(grs,'small_gr', 'big_gr'))

  all_df<-dplyr::inner_join(dummies_df %>% summarise(n=n()),
                            dummies_df %>%  summarise_at(vars(starts_with('.factor_')), sum),
                            by=c(grs, 'small_gr', 'big_gr'))  %>% data.table



  setattr(all_df$small_gr, 'label', date_var_label)
  setattr(all_df$big_gr, 'label', date_var_label)

  for(i in seq_along(factor_levels)) {
    setattr(all_df[[paste0('.factor_', stringr::str_pad(i, width=max(nchar(factor_levels)), pad = "0"))]],
            'label', names(factor_levels)[which.max(i==1)])
    setnames(all_df, paste0('.factor_',stringr::str_pad(i, width=max(nchar(factor_levels)), pad = "0")),
             paste0('factor_',stringr::str_pad(i, width=max(nchar(factor_levels)), pad = "0")))
  }

  if(!is.null(groupby)) {
    tidy_df<-tidyr::gather(all_df, factor_var, counts, -big_gr, -small_gr, -groupby, -n)
  } else {
    tidy_df<-tidyr::gather(all_df, factor_var, counts, -big_gr, -small_gr, -n)
  }


  tidy_df <- tidy_df %>%
    mutate(perc=counts/n, factor_var=factor(factor_var)) %>% data.frame()


  tidy_df$factor_var <- plyr::revalue(tidy_df$factor_var,
                                      setNames(names(factor_levels),
                                               paste0('factor_',stringr::str_pad(factor_levels,
                                                                                 width=max(nchar(factor_levels)),
                                                                                 pad = "0"))))
  setattr(tidy_df$small_gr, 'units', attr(mydf$small_gr, 'units'))
  setattr(tidy_df$big_gr, 'units', attr(mydf$big_gr, 'units'))

  return(tidy_df)
}


#Funkcja bierze all_dates i produkuje zmienną nominalną dzielącą dany przedział na
#wartości odpowiadające zadanej period_unit i period_value.
gen_one_time_factor<-function(all_dates, period_value=1, period_unit,
                              language='PL') {

  date_from<-lubridate::floor_date(min(all_dates), unit=paste(period_value, period_unit))
  date_to<-lubridate::ceiling_date(max(all_dates), unit=paste(period_value, period_unit))

  unit_pos<-match(period_unit, all_time_units)
  date_sequence<-reference_sequence(date_from, date_to, period_value, unit = period_unit)

  date_labels<-format_date(date_sequence,  language=language )

  num_seq<-amount_of_time_units(all_dates = date_sequence, ref_date = min(all_dates),
                                date_unit = period_unit)/period_value

  var<-cut.POSIXt(as.POSIXct(all_dates),
                  breaks=as.POSIXct(date_sequence),
                  labels=date_labels[-length(date_labels)])

  return(list(time_gr=var, time_sequence=date_sequence, numeric_sequence=num_seq, date_labels=date_labels))
}

#Funkcja bierze all_dates i produkuje zmienną nominalną dzielącą dany przedział na
#wartości odpowiadające zadanej period_unit i period_value. Podział nie musi być
#ładny dla użytkownika - ważne, aby zachował wszystkie dane.
gen_time_factor<-function(all_dates, period_value=1, period_unit, small_value, small_unit,
                          language='PL') {

  #browser()
  date_from<-lubridate::floor_date(min(all_dates), unit=paste(period_value, period_unit))
  date_to<-lubridate::ceiling_date(max(all_dates), unit=paste(period_value, period_unit))

  #  nice_time_labels(date_from=date_from, date_to=date_to, number_of_ticks = number_of_ticks,
  #                   max_significant_unit = decrease_time_unit(period_unit), language = language)

  unit_pos<-match(period_unit, all_time_units)
  date_sequence<-reference_sequence(date_from, date_to, period_value, unit = period_unit)

  date_labels<-format_date(date_sequence,  language=language )

  var<-cut.POSIXt(as.POSIXct(all_dates),
                  breaks=as.POSIXct(date_sequence),
                  labels=date_labels[-length(date_labels)])

  date_sequence<-reference_sequence(date_from, date_to, small_value, unit = small_unit)
  date_labels<-format_date(date_sequence, max_significant_unit = decrease_time_unit(period_unit), language = language)
  var2<-cut.POSIXt(as.POSIXct(all_dates),
                   breaks=as.POSIXct(date_sequence))
  levels(var2)<-date_labels
  return(list(big_gr=var, small_gr=var2))
}



#Returns true, if number of non-modal values is less or equal than threshold_count
are_values_almost_constant<-function(values, threshold_count=1) {
  return(sum(mode(values)!=values)<=threshold_count)
}

mode<-function(values) {
  ux <- unique(values)
  ux[which.max(tabulate(match(values, ux)))]
}

.measure_unit_heterogenocity<-function(date_sequence, unit) {
  if(unit=='year') {
    vals<-year(date_sequence)
  } else if(unit=='quarter') {
    vals<-quarter(date_sequence)
  } else if(unit=='month'){
    vals<-month(date_sequence)
  } else if(unit=='week') {
    vals<-lubridate::wday(date_sequence)
  } else if(unit=='day') {
    vals<-lubridate::day(date_sequence)
  } else if(unit=='hour') {
    vals<-lubridate::hour(date_sequence)
  } else if(unit=='minute') {
    vals<-lubridate::minute(date_sequence)
  } else if(unit=='sec') {
    vals<-lubridate::second(date_sequence)
  } else {
    stop(paste0("Unkown time unit ", unit))
  }
  return(are_values_almost_constant(vals, 0))
}

infer_natural_time_unt<-function(date_sequence){
  try_count<-0
  for(u in main_time_units) {
    m<-get_unit_values(date_sequence,u)
    if(m<0.1) {
      try_count<-try_count+1
    }
    if(try_count==2) {
      break
    }
    actual_unit<-u
  }
  return(actual_unit)
}

genDateRange<-function(date_from, date_to, step,  step_unit) {
  as.Date(seq.POSIXt(as.POSIXct(date_from), as.POSIXct(date_to), by=paste(step, step_unit)))
}

#Funkcja zwraca statystykę mierzącą stopień, w jakim dana sekwencja zmienia się w miarę danej jednostki.
#Jeśli blisko zeru, to znaczy że sekwencja w dużym stopniu (lub 100%) przylega do naturalnej wielokrotności jednostki.
#Wartość bliska 1 oznacza, że jednostka try_unit nie jest naturalną jednostką, do której można bez strat zaokrąglić
#daną sekwencję dat.
get_unit_values<-function(date_sequence, try_unit) {
  unit_pos<-match(try_unit, all_time_units)

  dates<-reference_sequence(date_from = min(date_sequence), date_to=max(date_sequence), step = 1, unit = try_unit)

  date_distance<-function(try_date) {
    min(abs(as.numeric(lubridate::interval(try_date,dates))))
  }

  return(2*max(map_dbl(date_sequence, date_distance)/atu_len[[unit_pos]]))
}




#Funkcja zwaraca optymalną odległość między ticks dla zadanego zakresu zmienności range_from, range_to
#oraz mininalnej liczby
genTicks <- function(range_from, range_to, maxCount, base=10, allowed_factors=c(2,5)){
  allowed_factors<-c(1, allowed_factors, base)
  range<-range_to-range_from
  logMaxTick <- log10(range/maxCount)
  exponent <- floor(logMaxTick)
  mantissa <- 10^(logMaxTick-exponent)
  mantissa <- allowed_factors[findInterval(mantissa,allowed_factors, rightmost.closed = TRUE)+1]
  tick<-mantissa*base^exponent


  return(list(from=floor(range_from / tick) * tick,
              to=ceiling(range_to/tick) * tick,
              step=tick))
}


#Funkcja dla zadanego okresu zwraca gęstość występowania unikalnych
#wartości z wektora all_dates
unique_date_density_uniform<-function(all_dates, period_value, period_unit, cut_perc=0.2){
  unit_pos<-match(period_unit, all_time_units)
  if(is.na(unit_pos)) {
    stop(paste0('Unknown time unit ', period_unit))
  }

  if(cut_perc>0) {
    start_range<-ceiling(length(all_dates) * cut_perc/2)
    end_range<-floor(length(all_dates) * (1-cut_perc/2))
    all_dates_int<-sort(all_dates)[start_range:end_range]
  } else {
    all_dates_int<-all_dates
  }

  date_low<-lubridate::floor_date(min(all_dates_int), unit=period_unit)
  date_low_next<-date_low + lubridate::period(period_value, period_unit)
  date_high_prev<-lubridate::floor_date(max(all_dates_int), unit=period_unit)
  date_high<-date_high_prev + lubridate::period(period_value, period_unit)
  unit_length<-atu_len[[unit_pos]] * period_value

  lo_unit_perc<-as.numeric(lubridate::as.period(lubridate::interval(min(all_dates_int), date_low_next)))/unit_length
  hi_unit_perc<-as.numeric(lubridate::as.period(lubridate::interval(date_high_prev, max(all_dates_int))))/unit_length
  cut_dates<-as.Date(seq.POSIXt(as.POSIXct(date_low), as.POSIXct(date_high), paste(period_value, period_unit)))

  weighted_sum<-length(cut_dates)-3+lo_unit_perc+hi_unit_perc

  df<-tibble(gr=cut(all_dates_int, cut_dates),
             val=all_dates_int)

  return(as.numeric(df %>% group_by(gr) %>%
                      summarise(n_unique=length(unique(val))) %>%
                      summarize(mean=sum(n_unique)))/weighted_sum)
}

#Funkcja zwraca jednostkę czasu, która jest najszersza i w której średnio będzie co najmniej target_unit_density elementów dat
infer_natural_base_time_unit<-function(date_density, period_value, period_unit, target_unit_density=1.0) {
  unit_pos<-match(period_unit, all_time_units)
  best_unit_pos<-which.max(atu_len[[unit_pos]]/atu_len>date_density/period_value/target_unit_density)-1

  return(all_time_units[[max(1,min(best_unit_pos, length(all_time_units)))]])
}


tickSize <- function(range,minCount){
  af <- c(1,2,5) # allowed factors
  mantissa <- af[findInterval(mantissa,af)]
  return(mantissa*10^exponent)
}

convertUnits<-function(value, unit) {
  if(unit=='quarter'){
    return(list(value=value*3, unit='month'))
  } else {
    return(list(value=value, unit=unit))
  }
}

nice_unit_name<-function(unit, language='PL', flag_plural_form=FALSE) {
  pos<-match(unit, all_time_units)
  if(is.na(pos)){
    stop(paste0("Unknown time unit ", unit))
  }
  if (language=='PL') {
    if(flag_plural_form){
      dic<-all_time_units_names_pl_mn_m
    } else {
      dic<-all_time_units_names_pl_m
    }
  } else {
    stop("Other languages not implemented yet")
  }
  return(dic[[pos]])
}

nice_time_labels<-function(date_from, date_to, number_of_ticks=NULL, manual_small_unit=NULL, manual_small_value=NULL,
                           max_significant_unit='nothing', language = 'PL') {
  # date_from=as.Date('2010-04-15')
  # date_from=lubridate::ymd_hms("2012-06-23 13:34:00")
  # date_to=as.Date('2012-07-1')
  # number_of_ticks=10

  if(xor(is.null(manual_small_unit), is.null(manual_small_value))){
    stop("You must provide both manual_small_unit and manual_small_value")
  }
  if(!xor(is.null(manual_small_unit), is.null(number_of_ticks))) {
    stop("You must provide either number_of_ticks or manual_small_unit and value")
  }

  if(!is.null(number_of_ticks)) {
    unit_span<-lubridate::as.period(lubridate::interval(date_from, date_to)/number_of_ticks)
    unit_span_unit <- first_significant_time_unit_from_period(unit_span)

    #    units<-convertUnits(1, unit_span_unit)
    #    date_sequence<-reference_sequence(date_from, date_to, unit=units$unit, step=units$value)
    date_sequence<-reference_sequence(date_from, date_to, unit=unit_span_unit, step=1)
    #    how_many_units_per_span_unit<-suppressWarnings(unit_span/lubridate::period(units$value, units$unit))
    #    how_many_units_in_total<-lubridate::interval(date_from, date_to)/lubridate::period(units$value, units$unit)

    #    tick_step<-genTicks(0, how_many_units_in_total, maxCount = number_of_ticks)
  } else {

  }


  date_str<-format_date(date_sequence = date_sequence, max_significant_unit = max_significant_unit,
                        min_significant_unit = unit_span_unit, language = language)

  names(date_sequence)<-date_str

  return(date_sequence)
}

format_date<-function(date_sequence, max_significant_unit='year', min_significant_unit='sec', language='PL', ...) {
  if(max_significant_unit=='nothing') {
    max_significant_unit<-'year'
  }
  pos_unit_hi<-match(max_significant_unit, all_time_units)
  if(is.na(pos_unit_hi)) {
    stop(paste0("Uknown unit ", max_significant_unit))
  }
  pos_unit_lo<-match(min_significant_unit, all_time_units)
  if(is.na(pos_unit_lo)) {
    stop(paste0("Uknown unit ", min_significant_unit))
  }
  u<-all_time_units[[pos_unit_lo]]
  next_unit<-next_units[[u]]
  lab<-list()

  while (TRUE) {
    if(match(u, all_time_units) < pos_unit_hi) {
      break
    }
    lab[u]<-list(cast_date_to_time_unit(date_sequence, unit=u, language = language, next_unit = next_unit))


    u<-next_unit
    if(u=='') {
      break
    }
    next_unit<-next_units[[u]]
  }

  date_uniques<-purrr::map_int(lab, ~length(unique(.)))
  # tmp<-setNames(rep(1, length(all_time_units)), all_time_units)
  # date_uniques<-c(date_uniques, tmp[setdiff(all_time_units, names(date_uniques))])
  # date_uniques<-date_uniques[all_time_units]

  lab[date_uniques==1]<-NULL

  #Teraz mamy listę z komponentami dat. Należy z tego zbudować sensowny wektor stringów
  #Syntaks oczywiście zależy od języka.
  #Oto przykłady: "Zima 2012", "23 wrz 2012", "23 tyg. 2011", "23 wrz 1994 23:21:00" "so"

  #Sortujemy wpisy wg. kolejności: quarter, day, week, month, year i konkatenuj je
  get_names<-c('quarter', 'day', 'week', 'month', 'year')
  if(any(get_names %in% names(lab))){
    pos<-purrr::map_int(names(lab), ~which(. == get_names))
    lab_days<-lab[get_names[pos]]
    str_dates<-do.call(paste, lab_days)
  } else {
    str_dates<-rep('', length(date_sequence))
  }

  get_names<-c('hour', 'minute', 'sec')
  if(any(get_names %in% names(lab))){
    pos<-purrr::map_int(names(lab), ~which(. == get_names))
    lab_seconds<-lab[get_names[pos]]
    str_seconds<-do.call(function(...) paste(..., collapse = ':'), lab_days)
    if(strlen(str_dates[[1]])>0) {
      str_seconds<-paste0(" ", str_seconds)
    }

  } else {
    str_seconds<-rep('', length(date_sequence))
  }
  str_dates<-paste0(str_dates, str_seconds)
  return(str_dates)
}

name_period<-function(period_unit, period_value=1, language='PL') {
  unit_pos <- match(period_unit, all_time_units)
  if(is.na(unit_pos)){
    stop(paste0("Cannot identify unit ", period_unit))
  }
  if(language=='PL') {
    ans<-danesurowe::liczebnik(liczba = period_value, mianownik = all_time_units_names_pl_m[unit_pos],
                               dopelniacz = all_time_units_names_pl_d[unit_pos],
                               lmnoga = all_time_units_names_pl_mn[unit_pos], flag_skip_zero = TRUE)
    return(ans)
  } else {
    browser("Not implemented yet")
  }
}
