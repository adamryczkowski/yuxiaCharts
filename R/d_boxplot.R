boxplot_dispatch<-function(property_accessor) {
#  browser()
  db_obj<-property_accessor$serve_db()
  class_dep<-property_accessor$get_property('dv.vartype')
  if (class_dep=='F') {
    property_accessor$reverse_vars()
  }
  language<-property_accessor$get_property('language', validator=function(x) checkmate::checkChoice(x, c('EN', 'PL')) )
  property_accessor$get_property('gv.f.o.b')
  property_accessor$set_report_dispatcher(boxplot_functions)
  db<-property_accessor$done_discovery()
  return(NULL)
}


boxplot_functions<-function(property_accessor, statistics) {
  property_accessor$done_discovery()
  out_funs<-list(anovas=function(pAcc, statistics, chapter) boxplot_comments(pAcc, chapter),
                 errorpoints=function(pAcc, statistics, chapter) boxplot(pAcc, do_violinplot=FALSE, chapter),
                 violinplot=function(pAcc, statistics, chapter) boxplot(pAcc, do_violinplot=TRUE, chapter))
}


boxplot_comments<-function(pAcc, doc) {
#  browser()
  db_obj<-pAcc$serve_db()
  language<-pAcc$get_property('language')
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()
  pAcc$done_discovery()
  dt<-db_obj$chunkdf_ivdvgv()

  #  labs <- yuxia::get_labels(zz=zz, zn=zn, groupby = groupby, dt=dt, filtr = filtr)
  if(db_obj$is_grouped()) {
    mod2<-dt %>% data.frame() %>% group_by(gv) %>%
      mutate(iv = iv, dv = dv) %>%
      do(tryCatch(
        broom::tidy(anova(lm(dv ~ iv, data=.))),
        error = function(e) data.frame(p.value=NA, statistic=NA, sumsq=NA, meansq=NA, term=c('iv', 'Residuals'), df=NA)))

    if(sum(is.na(mod2$p.value)) == nrow(mod2)){
      doc$insert_paragraph("Cannot make the statistical tests.")
      return(doc)
    }

    mod3<-mod2 %>% select(-p.value, -statistic)

    mod_df <- mod3 %>% select(-sumsq, -meansq) %>% tidyr::spread(key=term, value=df) %>% mutate(df2 = Residuals, df1 = iv) %>% select(-Residuals, -iv)
    mod_sumsq <- mod3 %>% select(-df, -meansq) %>% tidyr::spread(key=term, value=sumsq) %>% mutate(sumsq2 = Residuals, sumsq1 = iv) %>% select(-Residuals, -iv)
    mod_meansq <- mod3 %>% select(-df, -sumsq) %>% tidyr::spread(key=term, value=meansq) %>% mutate(meansq2 = Residuals, meansq1 = iv) %>% select(-Residuals, -iv)
    mod_rest <- mod2 %>% filter(term=='iv') %>% select(-meansq, -sumsq, -df, -term)

    dfs<-list(mod_df, mod_sumsq, mod_meansq, mod_rest)
    func <- function(...){
      df1 = list(...)[[1]]
      df2 = list(...)[[2]]
      col1 = colnames(df1)[1]
      col2 = colnames(df2)[1]
      xxx = left_join(..., by = setNames(col2,col1))
      return(xxx)
    }
    mod4 <- Reduce( func, dfs)
#    return(list(mod4=mod4))

    mod_txt <- mod4 %>% ungroup()%>% transmute(
      gv = as.character(gv),
      #      sumsq1_txt = report_single_value(sumsq1),
      #      sumsq2_txt = report_single_value(sumsq2),
      meansq1_txt = danesurowe::report_single_value(meansq1),
      meansq2_txt = danesurowe::report_single_value(meansq2),
      statistic_txt = danesurowe::report_F_test(F=statistic, df1=df1, df2=df2),
      pvalue_txt = danesurowe::report_pvalue_long(p.value)
    )

    if(language=='PL') {
      colnames(mod_txt) <- c(db_obj$groupvar_label(flag_md = TRUE), "Średnia suma kwadratów zn", "Średnia suma kwadratów reszt", "Statystyka testowa", "Istotność")
      tablabel <- paste0("Wyniki analizy wariancji zmiennej zależnej ", db_obj$depvar_label(flag_md = TRUE), " efektu zmiennej ",
                         db_obj$indepvar_label(flag_md = TRUE), " w rozbiciu na ", db_obj$groupvar_label(flag_md = TRUE), ". ")
      if (db_obj$filterstring!='') {
        tablabel <- paste0(tablabel, "Obliczenia zostały wykonane na ", db_obj$filter_label(flag_md = TRUE), ". ")
      }
    } else if (language=='EN') {
      colnames(mod_txt) <- c(db_obj$groupvar_label(), "Mean sum of squares for d.v.", "Mean sum of squares for residuals", "Test statistic", "P-value")
      browser()
    }

    doc$insert_table(caption=tablabel, table_df=mod_txt, c('anova', 'statistics'))
#    ret<-add_simple_table(tab=mod_txt, caption = tablabel, tab_label = tab_label)



    # tab1 <- mod_txt
    # qual1 <- evaluate_qual_of_table(as.matrix(mod_txt))
    #
    # tab <- t(rbind(colnames(mod_txt), as.matrix(mod_txt))) %>% as_tibble()
    # colnames(tab) <- tab[1,]
    # tab2<-tab[-1,]
    # qual2 <- evaluate_qual_of_table(as.matrix(tab))
    # if(qual1 > qual2) {
    #   tab <- tab2
    # } else {
    #   tab <- tab1
    # }

  } else {
    mod1 <- lm(eval(parse(text=paste0(dv, " ~ ", iv))), data=dt)
    rap1 <- anova(mod1)
    return(list(mod1=mod1))

    #    rap2 <- summary(mod1)
    if(language=='PL') {
      tab<-data.frame("Źródło zmienności"=c("Pomiędzy grupami","Wewnątrz grup", "Suma"),
                      "Suma kwadratów"=c(rap1$`Sum Sq`, sum(rap1$`Sum Sq`)),
                      "df"=c(rap1$Df, NA),
                      "Wariancja"=c(rap1$`Mean Sq`, NA),
                      "F"=c(rap1$`F value`, NA),
                      "Istotność"=c(rap1$`Pr(>F)`,NA))
      tablabel <- paste0("Wyniki analizy wariancji zmiennej zależnej ", zzlab, " efektu zmiennej ", znlab, ". Obliczenia zostały na: ", labs$filter_lab)
      doc$insert_table(caption=tablabel, table_df=tab, c('anova', 'statistics'))
    } else {
      browser()
    }
  }

  return(doc)
}

boxplot<-function(pAcc, do_violinplot=FALSE, chapter, remove_outliers = FALSE){
  db_obj<-pAcc$serve_db()
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()
  pAcc$get_property('gv.f.o.b')
  language<-pAcc$get_property('language')
#  browser()


  #Mam zbiór dt. Najpierw filtr. Potem wybieram zmienne

  dt<-db_obj$chunkdf_ivdvgv()

  mydt <- dt %>%  filter(complete.cases(.))  %>% as.data.table()

  if(remove_outliers) {
    mydt <- mydt %>% filter(dv > stats::quantile(dv, 0.025) && dv < stats::quantile(dv, 0.975))
  }
  setkeyv(mydt, c('gv', 'iv'))

  language<-pAcc$get_property('language')

  if(do_violinplot) {
    if(language=='PL') {
      label <- paste0("Wykres wiolinowy ilustrujący rozkład ", db_obj$depvar_label(flag_md = TRUE),
                      " w podziale na ", db_obj$indepvar_label(flag_md = TRUE), ". ")
      if (db_obj$is_grouped()){
        label<-paste0(label, "Kolorami oznaczono poziomy zmiennej ", db_obj$groupvar_label(flag_md = TRUE), ". ")
      }
      label <- paste0(label, "Analizę wykonano na zbiorze: ", db_obj$filter_label(flag_md = TRUE), ", ",
                      danesurowe::liczebnik(nrow(mydt), "przypadek", "przypadki", "przypadków"), '. ')
      label <- paste0(label, "W polach wypisano liczność każdej z grup. Małymi poziomymi kreskami oznaczono kwartyle, a czarną grubą kreską oznaczono średnią arytmetyczną.")
    } else {
      browser()
    }
  } else {
    if(language=='PL') {
      label <- paste0("Wykres ilustrujący wartości średnie oraz ich błędy standardowe ",
                      db_obj$depvar_label(flag_md = TRUE), " w podziale na ", db_obj$indepvar_label(flag_md = TRUE), ". ")
      if (db_obj$is_grouped()){
        label<-paste0(label, "Kolorami oznaczono poziomy zmiennej ", db_obj$groupvar_label(flag_md = TRUE), ". ")
      }
      label <- paste0(label, "Analizę wykonano na zbiorze: ", db_obj$filter_label(flag_md = TRUE), ", ",
                      danesurowe::liczebnik(nrow(mydt), "przypadek", "przypadki", "przypadków"), '. ')
      label <- paste0(label, "W polach wypisano liczność każdej z grup a wąsami oznaczono 95% przedział ufności.")
    } else {
      browser()
    }
  }

  if(!do_violinplot) {
    dfsrc <- mydt %>% as_tibble %>% group_by( iv, gv) %>%
      summarise(m=mean(dv, na.rm=TRUE), sd=sd(dv, na.rm=TRUE), n=n(), ci=qt(0.975,df=n-1)*sd/sqrt(n),
                cil = m - ci, ciu = m + ci)
    h<-ggplot(data = dfsrc, mapping = aes(y = m, x = iv))

  } else {
    h<-ggplot(data = mydt, mapping = aes(y = dv, x = iv))
  }


  if(db_obj$is_grouped()) {
    h<-h + aes(fill = gv, colour=gv)

    if(identical(pAcc$get_property('gv.f.o.b') ,2) )
    {
      h<-h+scale_fill_brewer(palette="Blues") + scale_color_grey(start = 0.5, end=0)
    } else {
      h<-h+scale_fill_brewer(palette="Set2") + scale_color_brewer(palette="Dark2")
    }
  }

  if(do_violinplot) {
    if('integer' %in% class(dt$dv)){
      h <- h + geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), bw = 1)
    } else {
      h <- h + geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
    }
  } else {
    pd <- position_dodge(0.3)
    h <- h + geom_point(position=pd) +
      geom_errorbar(aes(ymin=cil, ymax=ciu), width=.1, position=pd)
  }

  h <- h + ylab(db_obj$depvar_label(flag_md = FALSE)) + xlab(db_obj$indepvar_label(flag_md = FALSE))
  if(db_obj$is_grouped()){
    h <- h + labs(fill='gv', color='gv')
    grlab<-db_obj$groupvar_label(flag_md = FALSE)
    if (nchar(grlab)>40){
      h<-h+theme(legend.position="bottom",legend.direction="vertical")
    } else if (nchar(grlab)>20){
      h<-h+theme(legend.position="bottom")
    }
  }


  if (identical(danesurowe::GetTheoreticalMin_1(dt$dv),0) && identical(danesurowe::GetTheoreticalMax_1(dt$dv),1)){
    h <- h + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  } else {
    h <- h + scale_y_continuous(limits = c(danesurowe::GetTheoreticalMin_1(dt$dv), danesurowe::GetTheoreticalMax_1(dt$dv)))
  }
  if (Hmisc::units(dt$dv) == '%') {
    h <- h + scale_y_continuous(labels = scales::percent)
  }

  xtick_labels <- danesurowe::GetLabels(dt$iv, flag_recalculate = TRUE)
  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(xtick_labels)


  if(max(nchar(xtick_labels))>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }

  quant_min = quantile(dt$dv, probs=0.05, na.rm = TRUE)
  quant_max = quantile(dt$dv, probs=0.95, na.rm = TRUE)
  if(db_obj$is_grouped()) {
    df <- as.data.frame(mydt) %>% group_by(iv, gv) %>%
      summarise( m = mean(dv, na.rm=TRUE),
                 quant_hi  = quantile(dv, probs=0.625, na.rm = TRUE),
                 quant_lo  = quantile(dv, probs=0.375, na.rm = TRUE),
                 sum_val   = ifelse((m-quant_min)/(quant_max-m)>0.8 , quant_lo, quant_hi))

    df <- cbind(df, as.data.frame(mydt) %>% group_by(iv, gv) %>% summarise( count = n()) )
  } else {
    df <- as.data.frame(mydt) %>% group_by(iv) %>%
      summarise( m = mean(dv, na.rm=TRUE),
                 quant_hi  = quantile(dv, probs=0.625, na.rm = TRUE),
                 quant_lo  = quantile(dv, probs=0.375, na.rm = TRUE),
                 sum_val   = ifelse((m-quant_min)/(quant_max-m)>0.8 , quant_lo, quant_hi))

    df <- cbind(df, as.data.frame(mydt) %>% group_by(iv) %>% summarise( count = n()) )
  }
  df <- as.data.table(df)
  if(db_obj$is_grouped()) {
    df <- df %>% group_by(iv) %>% filter(count>2) %>% mutate(nlevels = sum(count>2))


    #    df[,x:=seq(nrow(df))/length(danesurowe::GetLevels(factor(mydt[[groupby]])))+1/length(danesurowe::GetLevels(factor(mydt[[groupby]])))/2]
  } else {
    #    df[,x:=seq(nrow(df))]
    df <- dt %>% mutate_(iv=iv, nlevels=1 ) %>%  mutate(x=as.integer(iv)) %>% data.table()
  }
  df <-df  %>% mutate(x=as.integer(iv) + (as.integer(gv) - (1+nlevels)/2)/3,
                      label=danesurowe::report_integer(count))
  library(ggrepel)
  if (do_violinplot) {
    h <- h + annotate("label_repel", x=df$x, y=df$sum_val, label=df$label, box.padding = unit(1, "lines"))
    h <- h + geom_segment(aes(x=x-1/3/nlevels,xend=x+1/3/nlevels, y=m, yend=m ), data=df, colour='black')
  } else {
    h <- h + annotate("text_repel", x=df$x, y=df$m, label=df$label, box.padding = unit(1, "lines"))
  }
  tags<-'ggplot'
  if(do_violinplot) {
    tags<-c(tags, 'violinplot')
  } else {
    tags<-c(tags, 'mean_boxplot')
  }
#  browser()
  chart_hash<-chapter$insert_chart(caption = label, gg = h, tags = tags)
  if(db_obj$is_grouped()) {
    mycols <- db_obj$groupvar_label(flag_md = FALSE)
    tab<-mydt %>%  data.frame() %>% group_by(gv, iv)
  } else {
    mycols <- character(0)
    tab<-mydt %>%  data.frame() %>% group_by(iv)
  }
  tab2<-tab %>% summarize(mean = mean(dv), sd = sd(dv), n=n(),
                          ci=qt(0.975,df=n-1)*sd/sqrt(n))

  if(do_violinplot) {
    tab1<-tab %>%  do(cbind(broom::tidy(quantile(.$dv, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))) %>% tidyr::spread(key = names, value = x)))
    danesurowe::copy_dt_attributes(tab2, tab1)
    tab <- left_join(tab1, tab2, by = c('gv', 'iv')) %>%
      mutate(n_txt = danesurowe::report_integer(n),
             mean = danesurowe::report_value_with_error(mean, ci, flag_insert_error=FALSE),
             sd = danesurowe::report_value_with_error(sd, sd/(2*sqrt(n)), flag_insert_error = FALSE))
    if(db_obj$is_grouped()) {
      tab<-tab %>% select(gv, iv, n, contains('%'), mean, sd)
    } else {
      tab<-tab %>% select(iv, n, contains('%'), mean, sd)
    }
    if(language=='PL') {
      mycols <- c(mycols, db_obj$indepvar_label(flag_md = FALSE) , "N", "$Q_{2,5\\%}$", "Dolny kwartyl", "Mediana", "Górny kwartyl", "$Q_{97,5\\%}$", "Średnia", "SD")
      tablabel <- paste0("Tabela ze statystykami pozycyjnymi użytymi w wykresie @fig:", chart_hash,
                         '. Kwantyle zostały użyte przy pomocy „metody nr 7”, używanej ',
                         'domyślnie dla zmiennych ciągłych w programach S i R. ')
    } else {
      browser()
    }
  } else {
    tab <- tab2  %>% mutate(n_txt = danesurowe::report_integer(n),
                            mean_txt = danesurowe::report_value_with_error(mean, ci),
                            sd_txt = danesurowe::report_value_with_error(sd, sd/(2*sqrt(n)), flag_insert_error = FALSE)) %>%
      select(gv, iv, n_txt, mean_txt, sd_txt)
    if(language=='PL') {
      mycols <- c(mycols, db_obj$indepvar_label(flag_md = FALSE),"N", "Średnia ± szerokość 95% przedz. ufności", "SD")
      tablabel <- paste0("Tabela ze statystykami opisowymi użytymi w wykresie @fig:", chart_hash,
                         '. Średnia została podana razem z szerokowścią 95% przedziału ',
                         'ufności i zaokrąglona do 2 miejsce znaczących. ',
                         'Odchylenie standardowe zostało zaokrąglone do 2 miejsc ',
                         'znaczących swojego błędu standardowego. ')
    } else {
      browser()
    }
  }
  colnames(tab) <- mycols
  tags<-c('plot_table', 'means')
  if(do_violinplot) {
    tags<-c(tags, 'violinplot')
  } else {
    tags<-c(tags, 'mean_boxplot')
  }
  chapter$insert_table(caption = tablabel, table_df = tab, tags = tags)
  return(chapter)
}
