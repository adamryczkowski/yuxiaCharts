boxplot_aggregate_dispatch<-function(pAcc) {
  db_obj<-pAcc$serve_db()

  if(db_obj$is_indepvar_aggregate()) {
    db_obj$reverse_vars()
  }

  bootstrap_n<-pAcc$get_property('aggregate_bootstrap_n')
  if(is.na(bootstrap_n)) {
    bootstrap_n<-500
    pAcc$put_property('aggregate_bootstrap_n', bootstrap_n)
  }

  pAcc$set_report_dispatcher(function(pAcc, statistics) {
    pAcc$done_discovery()
    return(list(boxplot_aggregate = boxplot_aggregate))
  })

  mydt<-db_obj$chunkdf
  zz<-db_obj$depvar


  bootdf<-zz$boot_ivgv(bootstrap_n = bootstrap_n)

  return(list(bootdf=bootdf))
}


boxplot_aggregate<-function(pAcc, statistics, chapter){
#  browser()
  language<-pAcc$get_property('language')
  bootstrap_n<-pAcc$get_property('aggregate_bootstrap_n', validate_int, 500)
  db_obj<-pAcc$serve_db()
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()

  dv<-db_obj$depvar

  b<-statistics$bootdf
  dt<-db_obj$chunkdf

  if(db_obj$is_grouped()) {
    h<-ggplot(data = b, mapping = aes(y = m, x = iv, fill = gv, colour=gv))
    if(identical(attr(dt[[db_obj$groupvar_name]],'f.o.b'),2) )
    {
      h<-h+scale_fill_brewer(palette="Blues") + scale_color_grey(start = 0.5, end=0)
    } else {
      h<-h+scale_fill_brewer(palette="Set2") + scale_color_brewer(palette="Dark2")
    }
  } else {
    h<-ggplot(data = b, mapping = aes(y = m, x = iv))
  }

  h <- h + geom_boxplot(aes(middle=q50, lower=q25, upper=q75, ymin=q05, ymax=q95), stat='identity') +
    ylab(db_obj$depvar_label()) + xlab(db_obj$indepvar_label())
  if(db_obj$is_grouped()){
    h <- h + labs(fill=db_obj$groupvar_label(), color=db_obj$groupvar_label())
    if (nchar(db_obj$groupvar_label())>40){
      h<-h+theme(legend.position="bottom",legend.direction="vertical")
    } else if (nchar(db_obj$groupvar_label())>20){
      h<-h+theme(legend.position="bottom")
    }
  }

  if (identical(dv$theoretical_max,1) && identical(dv$theoretical_min,0)){
    h <- h + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  } else {
    h <- h + scale_y_continuous(limits = c(dv$theoretical_min, dv$theoretical_max))
  }
  if (dv$unit == '%') {
    h <- h + scale_y_continuous(labels = scales::percent)
  }

  xtick_labels <- names(db_obj$ivlevels(flag_recalculate = TRUE))
  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(xtick_labels)


  if(max(nchar(xtick_labels))>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }

  if(language=='PL') {
    label <- paste0("Wykres typu box plot ilustrujący rozkład ", db_obj$depvar_label(TRUE), " w podziale na ", db_obj$indepvar_label(TRUE),
                    ". Rozkład wartości został uzyskany metodą bootstrap dla n = ", bootstrap_n, " symulacji. " )
    if (db_obj$is_grouped()){
      label<-paste0(label, "Kolorami oznaczono wykresy odpowiadające poziomom ", db_obj$groupvar_label(TRUE), ". ")
    }
    label <- paste0(label, "Analizę wykonano na zbiorze: ", db_obj$filter_label(TRUE))
  } else if (language=='EN') {
    browser()
  } else {
    browser()
  }
  tags<-c('bootstrap', 'aggregate_vars', 'boxplot')
  chart_hash<-chapter$insert_chart(caption=label, gg=h, chart_prefix='aggregate_boxplot', tags=tags)

  b2 <- b %>%  mutate(m=danesurowe::report_value_with_error(value=m, ci=sd), q05=danesurowe::report_single_value(q05),
                      q25=danesurowe::report_single_value(q25),q50=danesurowe::report_single_value(q50),
                      q75=danesurowe::report_single_value(q75),q95=danesurowe::report_single_value(q95)) %>% select(-sd)

  if(db_obj$is_grouped()) {
    mycols <- db_obj$groupvar_label()
  } else {
    mycols <- character(0)
  }
  mycols <- c(mycols, db_obj$indepvar_label(), "Wartość punktowa z Błędem standardowym", "$Q_{2,5\\%}$", "Dolny kwartyl", "Mediana", "Górny kwartyl", "$Q_{97,5\\%}$")
  colnames(b2) <- mycols

  tablabel <- paste0(
    "Tabela ze statystykami pozycyjnymi wartości ", db_obj$depvar_label(TRUE), " użytymi w wykresie @fig:", chart_hash,
    '. Ponieważ zmienna zależna ma sens dopiero na grupie rekordów, ',
    'statystyki rozkładu jej wartości można było policzyć tylko przy pomocy techniki bootstrap. ',
    'Wszystkie kolumny poza "Wartością punktową" zostały policzona na podstawie rozkładu otrzymanego dla ', bootstrap_n, ' symulacji bootstrap. ')

  chapter$insert_table(caption=tablabel, table_df=b2, tags=tags, flag_header_in_md=TRUE)

  return(chapter)
}


boxplot_wyliczany<-function(dt, filtr, zn, zz, groupby = '', hash, labs, bootstrap_n=500){
  if (zz %in% colnames(dt)) {
    tmp <- zz
    zz <- zn
    zn <- tmp
    zzlab <- labs$znlab
    znlab <- labs$zzlab
    zzlab_md <- labs$znlab_md
    znlab_md <- labs$zzlab_md
  } else {
    zzlab <- labs$zzlab
    znlab <- labs$znlab
    zzlab_md <- labs$zzlab_md
    znlab_md <- labs$znlab_md
  }

  zzfn <- eval(parse(text=paste0(zz,'()')))
  if (! 'wyliczenie' %in% class(zzfn)){
    stop(paste0("Brak funkcji wyliczającej ", zz))
  }

  filter_lab <- labs$filter_lab

  label <- paste0("Wykres typu box plot ilustrujący rozkład ", zzlab_md, " w podziale na ", znlab_md, ". Rozkład wartości został uzyskany metodą bootstrap dla n = ", bootstrap_n, " symulacji. " )
  if (!is.null(groupby)){
    label<-paste0(label, "Kolorami oznaczono wykresy odpowiadające poziomom ", labs$grlab_md, ". ")
  }
  label <- paste0(label, "Analizę wykonano na zbiorze: ", filter_lab)

  #Mam zbiór dt. Najpierw filtr. Potem wybieram zmienne

  browser() #Dodaj do tabeli liczebności w grupach
  mydt<-dt
  setkeyv(dt, c(zzfn$index, groupby, zn))

  do_boot<-function(dt, bootstrap_n) {
    #    browser()
    if (nrow(dt)>0) {
      b<-simpleError('')
      while('error' %in% class(b)){
        b<-tryCatch(
          boot::boot(dt, zzfn$fn2, R = bootstrap_n, ncpus=4, parallel = 'multicore'),
          error=function(e) {e}
        )
        if(ceiling(bootstrap_n/2)<bootstrap_n){
          bootstrap_n <- ceiling(bootstrap_n/2)
        } else {
          out <- zzfn$fn(dt,seq(nrow(dt)))
          return(list(m=as.numeric(out$value), sd=as.numeric(out$error), q05=as.numeric(NA), q25=as.numeric(NA), q50=as.numeric(NA), q75=as.numeric(NA), q95=as.numeric(NA)))
          #          stop("Failed to get correct number of bootstrap repetitions")
        }
      }
      qs<-quantile(b$t, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
      return(list(m=b$t0, sd=sd(b$t), q05=qs[[1]], q25=qs[['25%']], q50=qs[['50%']], q75=qs[['75%']], q95=qs[[5]]))
    } else {
      return(list(m=as.numeric(NA), sd=as.numeric(NA), q05=as.numeric(NA), q25=as.numeric(NA), q50=as.numeric(NA), q75=as.numeric(NA), q95=as.numeric(NA)))
    }
  }
  #  if (length(c(groupby, zn))>0){
  b<-mydt[, as.data.table(do_boot(.SD, bootstrap_n=bootstrap_n)), by = c(groupby, zn)]
  #  } else {
  #    b<-mydt[, as.data.table(do_boot(.SD, bootstrap_n=bootstrap_n))]
  #  }

  if(!is.null(groupby)) {
    h<-ggplot(data = b, mapping = aes_string(y = 'm', x = zn, fill = groupby, colour=groupby))
    if(identical(attr(dt[[groupby]],'f.o.b'),2) )
    {
      h<-h+scale_fill_brewer(palette="Blues") + scale_color_grey(start = 0.5, end=0)
    } else {
      h<-h+scale_fill_brewer(palette="Set2") + scale_color_brewer(palette="Dark2")
    }
  } else {
    h<-ggplot(data = b, mapping = aes_string(y = 'm', x = zn))
  }

  h <- h + geom_boxplot(aes(middle=q50, lower=q25, upper=q75, ymin=q05, ymax=q95), stat='identity') +
    ylab(zzlab) + xlab(znlab)
  if(!is.null(groupby)){
    h <- h + labs(fill=labs$grlab, color=labs$grlab)
    if (nchar(labs$grlab)>40){
      h<-h+theme(legend.position="bottom",legend.direction="vertical")
    } else if (nchar(labs$grlab)>20){
      h<-h+theme(legend.position="bottom")
    }
  }

  if (is.null(zzfn$min))  {
    zzfn$min<-NA
  }
  if (is.null(zzfn$max)) {
    zzfn$max<-NA
  }
  if (identical(zzfn$max,1) && identical(zzfn$min,0)){
    h <- h + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  } else {
    h <- h + scale_y_continuous(limits = c(zzfn$min,zzfn$max))
  }
  if (zzfn$type == '%') {
    h <- h + scale_y_continuous(labels = scales::percent)
  }

  xtick_labels <- danesurowe::GetLabels(dt[[zn]], flag_recalculate = TRUE)
  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(xtick_labels)


  if(max(nchar(xtick_labels))>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }



  if(!is.null(groupby)) {
    b2<-b %>%  data.frame() %>% rename_(gropby=groupby, zn = zn)
  } else {
    b2<-b %>%  data.frame() %>% rename_(zn = zn)
  }

  b2 <- b2 %>%  mutate(m=danesurowe::report_value_with_error(value=m, ci=sd), q05=danesurowe::report_single_value(q05),
                       q25=danesurowe::report_single_value(q25),q50=danesurowe::report_single_value(q50),
                       q75=danesurowe::report_single_value(q75),q95=danesurowe::report_single_value(q95)) %>% select(-sd)



  labs<-get_labels(zz=zz, zn=zn,filtr=filtr, groupby = groupby,dt=dt)
  if(!is.null(groupby)) {
    mycols <-   labs$grlab
  } else {
    mycols <- character(0)
  }
  mycols <- c(mycols, znlab, "Wartość punktowa z Błędem standardowym", "$Q_{2,5\\%}$", "Dolny kwartyl", "Mediana", "Górny kwartyl", "$Q_{97,5\\%}$")
  colnames(b2) <- mycols

  tablabel <- paste0(
    "Tabela ze statystykami pozycyjnymi wartości ", zzlab_md, " użytymi w wykresie @fig:", hash,
    '. Ponieważ zmienna zależna ma sens dopiero na grupie rekordów, ',
    'statystyki rozkładu jej wartości można było policzyć tylko przy pomocy techniki bootstrap. ',
    'Wszystkie kolumny poza "Wartością punktową" zostały policzona na podstawie rozkładu otrzymanego dla ', bootstrap_n, ' symulacji bootstrap. ')

  tab_label <- generate_table_hash(cell_hash = hash, plot_function = 'boxplot_wyliczany')
  msg<-add_simple_table(tab=b2, caption = tablabel, tab_label = tab_label)

  return(list(chart=h,label=label, rap_postfix=msg))
}
