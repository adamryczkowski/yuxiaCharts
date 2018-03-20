ts_trend_dispatch<-function(pAcc) {
  period_value<-pAcc$get_property('period_value', default_value = 1, validator=validate_numeric)
  period_unit<-pAcc$get_property('period_unit', default_value = 'year', validator=validate_string)
  reg_time_unit<-pAcc$get_property('reg_time_unit', default_value = period_unit, validator=validate_numeric)
  hdi_proc<-pAcc$get_property('hdi_proc', default_value = 0.95, validator=validate_numeric)
  class_dep<-pAcc$get_property('dv.vartype')
  if (class_dep=='D') {
    pAcc$reverse_vars()
  }
  db_obj<-pAcc$serve_db()
  pAcc$set_report_dispatcher(ts_trend_reports)
  db_obj$groupvar_label()
  pAcc$done_discovery()

  db<-db_obj$chunkdf_ivdvgv()

  ref_date<-min(db$iv)

  ans<-make_seasonal_df(dt = db, date_var = 'iv', dep_var = 'dv', groupby = 'gv',
                        period_value = period_value, period_unit = period_unit, flag_return_date_gr=TRUE)

  seas_df <- ans$mydt %>% select(-big_gr, -small_gr)
  grvar<-ans$var_gr

  grvars<-'detail_gr'
  if(db_obj$is_grouped()) {
    grvars<-c(grvars, 'groupby')
  }

  synth_df<-seas_df %>% as_tibble() %>% group_by_(.dots=grvars) %>%
    summarise(m = mean(dep_var, na.rm=TRUE), se = sd(dep_var, na.rm=TRUE)/sqrt(sum(!is.na(dep_var))))

  time_factor<-1/period_factor(unit_ref = reg_time_unit, value_ref = 1, unit_comp = ans$small_unit, value_comp = 1)
  date_label_df<-tibble(labels=grvar$date_labels, dates=grvar$time_sequence, values=grvar$numeric_sequence * time_factor)
  synth_df<-dplyr::left_join(synth_df %>% mutate(labels=as.character(detail_gr)), date_label_df, by=c(labels='labels'))
  period<-1/period_factor(unit_ref = reg_time_unit, value_ref = 1, unit_comp = period_unit, value_comp = period_value)

  library(rstan)
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  if(db_obj$is_grouped()) {
    tmp_df <- synth_df %>% group_by(groupby) %>% select(x=values, groupby, detail_gr) %>% summarise(n_times=n())
    grlevels <- as.character(unique(tmp_df$groupby))
    num_x <- (synth_df %>% filter(groupby == grlevels[[1]]) %>% group_by(detail_gr) %>% select(x=values, detail_gr))$x
    n_times <- length(num_x)

    n_groups <- sum(tmp_df$n_times>0)
    stan_model <- system.file('stan/trend-h.stan', package = 'yuxiaCharts')
    fits<-list()
    for(i in seq(n_groups)){
      num_y <- (synth_df %>% filter(groupby == grlevels[[i]]))$m
      num_se <- (synth_df %>% filter(groupby == grlevels[[i]]))$se

      stan_data<-list(
        n_times=n_times,
        n_groups=NULL,
        x=num_x,
        y=num_y,
        period=period,
        sigma=num_se,
        prior_a=sd(num_y)/sd(num_x),
        prior_b=sd(num_y)
      )
      fits[[i]]<-stan(file = stan_model,
                      data = stan_data,
                      chains = 16, iter = 4000, control=list(adapt_delta=0.99))
      # fits[[i]]<-stan(file = stan_model,
      #            data = stan_data,
      #            chains = 16, iter = 4000, control=list(max_treedepth=25, adapt_delta=0.99))

    }
    setattr(fits, 'label', db_obj$groupvar_label())
    names(fits)<-grlevels
    # num_x <- (synth_df %>% group_by(detail_gr) %>% select(x=values, groupby, detail_gr) %>%
    #             tidyr::spread(key = groupby, value = x) %>% ungroup %>% select(-detail_gr))[[1]]
    # n_times <- max(tmp)
    # num_y <- synth_df %>% group_by(detail_gr) %>% select(x=m, groupby, detail_gr) %>%
    #   tidyr::spread(key = groupby, value = x) %>% ungroup %>% select(-detail_gr) %>% data.matrix() %>% t()
    #
    # num_se <- synth_df %>% group_by(detail_gr) %>% select(x=se, groupby, detail_gr) %>%
    #   tidyr::spread(key = groupby, value = x) %>% ungroup %>% select(-detail_gr) %>% data.matrix() %>% t()
    # stan_model<-'stan/trend-hs.stan'
    sims<-purrr:::map(fits, extract)
    y<-unlist(map(sims, ~apply(.$pred_y, 2, mean)))
    dim(y)<-c(length(y)/n_groups, n_groups)

    y_sd<-unlist(map(sims, ~apply(.$pred_y, 2, sd)))
    dim(y_sd)<-c(length(y)/n_groups, n_groups)
    pars<-list(
      fi=map_dbl(sims, ~mean(.$fi)),
      fi_sd=map_dbl(sims, ~sd(.$fi)),
      a=map_dbl(sims, ~mean(.$a)),
      a_sd=map_dbl(sims, ~sd(.$a)),
      b=map_dbl(sims, ~mean(.$b)),
      b_sd=map_dbl(sims, ~sd(.$b)),
      tau=map_dbl(sims, ~mean(.$tau)),
      tau_sd=map_dbl(sims, ~sd(.$tau)),
      #    theta=apply(sim$theta,2,mean),
      #    theta_sd=apply(sim$theta,2,sd),
      y=y,
      y_sd=y_sd
    )

    # pars<-list(
    #   fi=apply(sim$fi, 2, mean),
    #   fi_sd=apply(sim$fi, 2, sd),
    #   a=apply(sim$a, 2, mean),
    #   a_sd=apply(sim$a, 2, sd),
    #   b=apply(sim$b, 2, mean),
    #   b_sd=apply(sim$b, 2, sd),
    #   tau=apply(sim$tau, 2, mean),
    #   tau_sd=apply(sim$tau, 2, sd),
    #   mu=apply(sim$mu, 2, mean),
    #   mu_sd=apply(sim$mu, 2, sd),
    #   #    theta=apply(sim$theta,2,mean),
    #   #    theta_sd=apply(sim$theta,2,sd),
    #   y=apply(sim$pred_y, c(3,2), mean),
    #   y_sd=apply(sim$pred_y, c(3,2), sd)
    # )

  } else {
    num_x <- (synth_df %>% group_by(detail_gr) %>% select(x=values, detail_gr))$x
    n_times <- length(num_x)
    n_groups<-NULL
    num_y <- synth_df$m
    num_se <- synth_df$se
    stan_model<-'stan/trend-h.stan'
    stan_data<-list(
      n_times=n_times,
      n_groups=n_groups,
      x=num_x,
      y=num_y,
      period=period,
      sigma=num_se,
      prior_a=sd(num_y)/sd(num_x),
      prior_b=sd(num_y)
    )
    library(rstan)
    rstan::rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())
    fit<-stan(file = stan_model,
              data = stan_data,
              chains = 16, iter = 8000)
    sim<-extract(fit)
    pars<-list(
      fi=mean(sim$fi),
      fi_sd=sd(sim$fi),
      a=mean(sim$a),
      a_sd=sd(sim$a),
      b=mean(sim$b),
      b_sd=sd(sim$b),
      tau=mean(sim$tau),
      tau_sd=sd(sim$tau),
      #    theta=apply(sim$theta,2,mean),
      #    theta_sd=apply(sim$theta,2,sd),
      y=apply(sim$pred_y, 2, mean),
      y_sd=apply(sim$pred_y, 2, sd)
    )
    fits<-list(fit)
  }


  if(db_obj$is_grouped()) {
    dfy<-as_tibble(pars$y) %>% tidyr::gather(key = 'groupby', value = 'y')
    dfysd<-as_tibble(pars$y_sd) %>% tidyr::gather(key = 'groupby', value = 'y_sd')

    sim_df<-cbind(dfy,dfysd %>% select(-groupby), x=stan_data$x)
    if(is.ordered(synth_df$groupby)) {
      sim_df$groupby<-ordered(sim_df$groupby,  labels=names(db_obj$gvlevels(flag_recalculate = TRUE)))
    } else {
      sim_df$groupby<-factor(sim_df$groupby,  labels=names(db_obj$gvlevels(flag_recalculate = TRUE)))
    }
    danesurowe::copy_var_attributes(synth_df$groupby, dt_dest = sim_df, 'groupby' )
    plot_df<-suppressWarnings(dplyr::full_join(sim_df, synth_df %>% rename(x=values), by=c("groupby", "x"))) %>%
      select(groupby, x, real_y=m, real_y_sd=se, sim_y=y, sim_y_sd=y_sd, labels, dates, detail_gr)
  } else {
    plot_df<-tibble(x=stan_data$x, real_y=stan_data$y, real_y_sd=stan_data$sigma,
                    sim_y=pars$y, sim_y_sd=pars$y_sd, dates=synth_df$dates)
  }
  if(db_obj$is_grouped()) {
    setattr(plot_df$groupby, 'label', db_obj$groupvar_label())
  }
  return(list(plot_df=plot_df, models=fits))
}

ts_trend_reports<-function(pAcc, statistics) {
  plots <- list(sinus_trend=plot_sinus_season,
                fi_season=plot_fi_season,
                ab_season=plot_ab_season,
                tau_season=plot_tau_season,
                periodogram=plot_periodogram_numeric)
  return(plots)
}

plot_sinus_season<-function(pAcc, statistics, chapter){
  period_value<-pAcc$get_property('period_value', default_value = 1, validator=validate_numeric)
  period_unit<-pAcc$get_property('period_unit', default_value = 'year', validator=validate_string)
  reg_time_unit<-pAcc$get_property('reg_time_unit', default_value = period_unit, validator=validate_numeric)

  plot_df<-statistics$plot_df
  dt<-plot_df
  db_obj<-pAcc$serve_db()
  db_obj$groupvar_label()
  db_obj$indepvar_label()
  db_obj$depvar_label()
  db_obj$filter_label()
  pAcc$done_discovery()
  #browser()
  #The basic plot

  if(db_obj$is_grouped()) {
    h<-ggplot(data=plot_df, aes(x, y=real_y)) + facet_wrap(~groupby) +
      geom_errorbar(mapping = aes(ymin=real_y-1.96*real_y_sd, ymax=real_y+1.96*real_y_sd), color='gray30') +
      geom_point()
    grlabels<-db_obj$groupvar_label()

    h<-set_nominal_colors(h, level_count = length(grlabels), flag_never_user_dashes = TRUE)$h
  } else {
    h<-ggplot(data=plot_df, aes(x, y=real_y)) + geom_errorbar(mapping = aes(ymin=real_y-1.96*real_y_sd, ymax=real_y+1.96*real_y_sd)) + geom_point()
  }


  ref_date<-min(plot_df$dates)

  big_xlabels<-nice_time_labels(date_from = ref_date, date_to = max(plot_df$dates), number_of_ticks = 9)
  big_xlabels_num<-amount_of_time_units(all_dates = big_xlabels, ref_date = ref_date, date_unit = reg_time_unit)
  #  names(big_xlabels_num)<-names(big_xlabels)

  small_xlabels<-nice_time_labels(date_from = ref_date, date_to = max(plot_df$dates), number_of_ticks = 20)
  small_xlabels_num<-amount_of_time_units(all_dates = small_xlabels, ref_date = ref_date, date_unit = reg_time_unit)



  if(db_obj$is_grouped()){
    if (nchar(db_obj$groupvar_label())>20){
      h<-h+theme(legend.position="bottom")
    }
    if(max(nchar(names(db_obj$gvlevels(TRUE))))>40){
      h<-h+theme(legend.direction = 'vertical')
    }
  }

  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(big_xlabels)

  if(max(nchar(big_xlabels))>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }

  h<-h + xlab(db_obj$indepvar_label()) + ylab(db_obj$depvar_label()) +
    scale_x_continuous(breaks = big_xlabels_num, labels = names(big_xlabels), minor_breaks = small_xlabels_num)
  if(db_obj$is_grouped()) {
    h<- h +
      geom_ribbon(mapping = aes(x, ymin=sim_y-1.96*sim_y_sd, ymax=sim_y+1.96*sim_y_sd, color=groupby, fill=groupby), alpha=0.2) +
      labs(colour=db_obj$groupvar_label(), fill=db_obj$groupvar_label())
    #+ geom_line(mapping = aes(x, sim_y, color=groupby)) + labs(colour=labs$grlab, fill=labs$grlab)
    grlen<-nchar(db_obj$groupvar_label())
    grlens<-max(4 + nchar(names(db_obj$gvlevels(TRUE))))
    if(max(grlen, grlens)>20) {
      h<-h+theme(legend.position="bottom")
      if(grlen + grlens > 60) {
        h<-h+theme(legend.position="bottom",legend.direction="vertical")
      }
    }
  } else {
    h<- h +
      geom_ribbon(mapping = aes(x, ymin=sim_y-1.96*sim_y_sd, ymax=sim_y+1.96*sim_y_sd), color='red', fill='red',linetype='dotted',  alpha=0.2)
    #     + geom_line(mapping = aes(x, sim_y), color='red')
  }
  #  browser()
  caption<-paste0("Wykresy przedstawiające zmiany ", db_obj$depvar_label(TRUE), " w czasie ", db_obj$indepvar_label(TRUE))
  if(db_obj$is_grouped()) {
    caption<-paste0(caption, " w podziale na ", db_obj$groupvar_label(TRUE))
  }
  caption<-paste0(caption, " policzone na zbiorze ", db_obj$filter_label(TRUE), " z naniesionym dopasowanym trendem sezonowym w formie sinusoidy o okresie ",
                  name_period(period_unit, period_value), " przy pomocy ",
                  "hierarchicznego modelu bayesowskiego. Szerokością wstążki oznaczono błąd predykcji wynikający z nieoznaczoności dopasowanych parametrów modelu. ")

  rap_prefix <- paste0("Aby sprawdzić istnienie istotnego statystycznie sinusoidalnego trendu o długości ", name_period(period_unit, period_value),
                       " dopasowano model bayesowski.")

  chapter$insert_paragraph(rap_prefix, tags=c('ts_trend','ts_trend_ts'))
  chapter$insert_ggchart(caption=caption, gg=h, chart_prefix='ts_sinus', tags=c('ts_trend','ts_trend_ts'))
}

plot_fi_season<-function(pAcc, statistics, chapter){
  hdi_proc<-pAcc$get_property('hdi_proc', default_value = 0.95, validator=validate_numeric)
  pAcc$done_discovery()
  ans<-bayes_par_plot(fits = statistics$models, pars = 'fi', hdi_proc = hdi_proc, point_est = 'mean',
                      par_unit_names='π', par_units=1/2, par_labels = "Faza sinusoidy")
  h<-ans$h
  tab<-ans$tab
  chart_caption<-paste0("Rozkład dopasowanego do modelu parametru określającego fazę dopasowanej sinusoidy. ",
                        "Przyjęto zakres zmienności fazy sinusoidy równej połowie jej okresu, ale za to ",
                        "pozwolono parametrowi regresji stojącemu przy sinusoidzie przyjmować wartości ujemne. ",
                        "W ten sposób zapewniono jednoznaczność modelu (identifiability). ",
                        "Pionową kreską oznaczono średnią (środek ciężkości) rozkładu. ",
                        "Zacieniowano 95% obszar HDI (High Density Interval), tj. najmniejszy przedział, który zawiera 95% rozkładu. Jest na 95% prawdopodobne, że parametr znajduje się wewnątrz tego przedziału. ")
  hash<-chapter$insert_ggchart(caption=chart_caption, gg=h, chart_prefix='ts_fi_season', tags=c('ts_trend','ts_trend_fi'))


  tab_caption<-paste0("Tabela komplementarna do wykresu @fig:", hash, ", przedstawiająca statystyki przedstawionych rozkładów. ")
  chapter$insert_table(caption=tab_caption, table_df=tab, tags=c('ts_trend','ts_trend_fi'), flag_header_in_md=FALSE)
#  rap<-add_simple_table(tab, caption=tab_caption, tab_label = tab_label)
#  return(list(chart=h, label=chart_caption))
}

plot_ab_season<-function(pAcc, statistics, chapter){
  hdi_proc<-pAcc$get_property('hdi_proc', default_value = 0.95, validator=validate_numeric)
  pAcc$done_discovery()

  ans<-bayes_par_plot(fits = statistics$models, pars = c('a', 'b') , hdi_proc = hdi_proc, point_est = 'mean',
                      par_labels = c('Liniowy','Sinusoidalny'))
  h<-ans$h
  tab<-ans$tab
  chart_caption<-paste0("Rozkład dopasowanych do modelu współczynników regresji określających odpowiednio ",
                        "komponent liniowy ($a$) oraz komponent sezonowy ($b$). ",
                        "Pionową kreską oznaczono średnią (środek ciężkości) rozkładu. ",
                        "Zacieniowano 95% obszar HDI (High Density Interval), tj. najmniejszy przedział, który zawiera 95% rozkładu. Jest na 95% prawdopodobne, że parametr znajduje się wewnątrz tego przedziału. ")
  hash<-chapter$insert_ggchart(caption=chart_caption, gg=h, chart_prefix='ts_ab_season', tags=c('ts_trend','ts_trend_ab'))

  tab_caption<-paste0("Tabela komplementarna do wykresu @fig:", hash, ", przedstawiająca statystyki przedstawionych rozkładów. ")
  chapter$insert_table(caption=tab_caption, table_df=tab, tags=c('ts_trend','ts_trend_ab'), flag_header_in_md=FALSE)
}

plot_tau_season<-function(pAcc, statistics, chapter){
  hdi_proc<-pAcc$get_property('hdi_proc', default_value = 0.95, validator=validate_numeric)
  pAcc$done_discovery()
  ans<-bayes_par_plot(fits = statistics$models, pars = c('tau') , hdi_proc = hdi_proc, point_est = 'mean')
  h<-ans$h
  tab<-ans$tab
  chart_caption<-paste0("Rozkład parametru τ, będącego miarą niewyjaśnionej przez model wariancji, tj. wariancja tego, ",
                        " co zostaje po odjęciu od policzonych obserwacji w danym miesiącu a) komponentu stałego, ",
                        "b) komponentu sinusoidalnego, i c) wariancji wewnątrzmiesięcznej. ",
                        "Im model okazał się lepszy, tym mniejsza jest wartość τ. ",
                        "Pionową kreską oznaczono średnią (środek ciężkości) rozkładu. ",
                        "Zacieniowano 95% obszar HDI (High Density Interval), tj. najmniejszy przedział, który zawiera 95% rozkładu. Jest na 95% prawdopodobne, że parametr znajduje się wewnątrz tego przedziału. ")
  hash<-chapter$insert_ggchart(caption=chart_caption, gg=h, chart_prefix='ts_tau_season', tags=c('ts_trend','ts_trend_tau'))

  tab_caption<-paste0("Tabela komplementarna do wykresu @fig:", hash, ", przedstawiająca statystyki przedstawionych rozkładów. ")
  chapter$insert_table(caption=tab_caption, table_df=tab, tags=c('ts_trend','ts_trend_tau'), flag_header_in_md=FALSE)
}

plot_periodogram_numeric<-function(pAcc, statistics, chapter){
#  browser()
  db_obj<-pAcc$serve_db()

  dt<-statistics$plot_df
    # dt, filtr, zn, zz, groupby = NULL, hash, labs,
    # period_unit, period_value, reg_time_unit, hdi_proc)
  if(db_obj$is_grouped()) {
#    browser() #Nie przetestowałem tej ścieżki
    plotdf<-dt %>% as_tibble %>% select(dv=groupby, iv = dates, counts=real_y)
  } else {
    plotdf<-dt %>% as_tibble %>% select(iv = dates, counts=real_y)
  }

  ans<-plot_periodogram(pAcc, plotdf)
  chapter$insert_ggchart(caption=ans$label, gg=ans$chart, chart_prefix='ts_trend_periodogram', tags= c('ts_trend', 'ts_trend_periodogram'))
}
