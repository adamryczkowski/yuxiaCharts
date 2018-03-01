plot_seasons<-function(pAcc, chapter, season_df = statistics$season_df){
  db_obj<-pAcc$serve_db()

  language<-pAcc$get_property('language')
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()
  flag_is_groupby<-db_obj$is_grouped()

  #TODO: Mam zbiór danych w którym jest zmienna zn_gr, która dzieli daty na czynnik.
  #Należy zrobić wykres profile + wąsy błędów, w którym:
  #wąsy błędów są pokolorowane skalą kolorystyczną dostosowaną do skali. 12 mies = 4 kolory.
  #linie są pokolorowane wg. zmiennej grupującej, przy tym jeśli zmiennej grupującej
  #jest więcej, niż 8 sztuk, należy dodać linie przerywane i wtedy mamy 16 sztuk.



  pd <- position_dodge(0.3)

  xlabels<-db_obj$ivlevels(flag_recalculate = TRUE)
  ylabels<-db_obj$dvlevels(flag_recalculate = TRUE)

  dt<-db_obj$chunkdf_ivdvgv()
  h<-ggplot(data = season_df, mapping = aes(fill=factor_var,
                                     colour=factor_var,
                                     y = cum_m,
                                     x = as.numeric(small_gr)))

  ans<-set_nominal_colors(h = h, level_count = length(ylabels), flag_never_user_dashes = TRUE)
  h<-ans$h
  flag_use_lines<-ans$flag_use_lines


  if(flag_use_lines) {
    h <- h + aes(linetype=factor_var) + geom_line()
  } else {
    h <- h + geom_ribbon(mapping = aes(ymin=cum_m_b,  ymax=cum_m), alpha=0.3)  + geom_line()
  }
  h<-h+
    geom_errorbar(aes(ymin=cil, ymax=ciu), width=.1, position=pd) +
    scale_x_continuous(breaks=seq_along(xlabels), label=xlabels) +
    scale_y_continuous(labels = scales::percent)

  #browser()
  h<-set_xlabels(h = h, zzlab = db_obj$depvar_label(), xlabels = xlabels, ylabels = ylabels, flag_y_percent = TRUE)


  h<-h + xlab(NULL) + ylab(NULL)

  grlab<-db_obj$indepvar_label(flag_md = FALSE)
  h <- h + labs(fill=grlab, color=grlab)

  if(flag_is_groupby) {
    df<-season_df %>% select(groupby, small_gr, factor_var, m, m_se, tau, tau_se)
    mycols<-db_obj$groupvar_label()
    h<- h + facet_wrap( ~groupby )
  } else {
    df<-season_df %>% select(groupby, small_gr, factor_var, m, m_se, tau, tau_se)
  }
  df <- df %>%
    mutate(m_txt = danesurowe::report_value_with_error(m*100, m_se*100),
           tau_txt=danesurowe::report_value_with_error(tau*100, tau_se*100)) %>%
    select(-m, -m_se, -tau, -tau_se, tau_txt)


  if(language=='PL') {
    mycols <- c(mycols, db_obj$indepvar_label(),  db_obj$depvar_label(), "udział %", "τ")

    label<-paste0("Wykres przedstawiający komponent sezonowy. Błędy standardowe policzono przy użyciu bayesowskiego modelu miksującego błędy liczone poprzez estymator odchylenia standardowego pomiędzy latami i błędy liczone z rozkładu Dirichleta. Parametr miksujący, τ, zbiega do zera, jeśli pomiary dla każdego roku w danym miesiącu są ze sobą zgodne oraz do odchylenia standardowego między latami, jeśli pomiary proporcji zmiennej grupującej pomiędzy latami w danym miesiącu nie są zgodne.")
    if(!is.null(attr(season_df$small_gr,'units'))){
      nice_unit<-nice_unit_name(attr(season_df$small_gr,'units'), language)
      label<-paste0(label, " Najmniejszą podziałką osi X oznaczono ", nice_unit)
    }
    tags<-c('nominal_ts_4seasons')
    chart_hash<-chapter$insert_chart(caption = label, gg = h, tags = tags, chart_prefix = 'nominal_ts_4seasons')
    tablabel <- paste0("Tabela ze wartościami użytymi w wykresie @fig:", chart_hash, '. ')
  } else if (language=='EN') {
    browser()
  } else {
    browser()
  }

#  browser()
  names(df) <- mycols
  chapter$insert_table(caption = tablabel, table_df = df, tags = tags, flag_header_in_md = TRUE)


  return(chapter)
}
