two_by_two_test<-function(pAcc, statistics, chapter) {
  #browser()
  db_obj<-pAcc$serve_db()
  flag_force_logit = pAcc$get_property('crosstab.force_logit')
  flag_gr_after_indep = pAcc$get_property('table_group_first')
  language<-pAcc$get_property('language')

  fob_dv <- pAcc$get_property('dv.f.o.b')
  fob_iv <- pAcc$get_property('iv.f.o.b')

  if(language=='PL') {
    msg2 <- paste0("Policzono statystyki mierzące związek między ", db_obj$indepvar_label(TRUE), " a ", db_obj$depvar_label(TRUE))
    if(db_obj$is_grouped()){
      msg2 <- paste0(msg2, " w rozbiciu na ", db_obj$groupvar_label(TRUE), ". \n")
    } else {
      msg2<-paste0(msg2, ". ")
    }

    if(db_obj$filter_label()!='') {
      msg2<-paste0(msg2, 'Analizę wykonano na zbiorze: ', db_obj$filter_label(TRUE), '. \n')
    }
  } else if(language=='EN') {
    browser()
  } else {
    browser()
  }

  chapter$insert_paragraph(msg2)

  cat_fob<-paste0(fob_iv, fob_dv)
#  browser()
  if(cat_fob %in% c('11', '13', '31', '33')) {
    two_by_two_chi2_test(pAcc, statistics, chapter)
  } else if (cat_fob %in% c('32')) {
    pAcc$reverse_vars()
    two_by_two_U_test(pAcc, statistics, chapter)
  } else if (cat_fob %in% c('23')) {
    two_by_two_U_test(pAcc, statistics, chapter)
  } else if (cat_fob == '22') {
    two_by_two_rho_test(pAcc, statistics, chapter)
  } else if (cat_fob %in% c('12', '21')) {
    if(cat_fob=='12') {
      pAcc$reverse_vars()
    }
    two_by_two_KW_test(pAcc, statistics, chapter)
    #browser() #This test needs to be implemented
  } else {
    browser()
  }

}

two_by_two_chi2_test<-function(pAcc, statistics, chapter) {
  #browser()
  db_obj<-pAcc$serve_db()
  language<-pAcc$get_property('language')


  mydt<-db_obj$chunkdf_ivdvgv()


  if(db_obj$is_grouped()){
    if(language=='PL') {
      msg <- paste0("Dla każdej grupy wykonano test niezależności χ²\uA0 Pearsona. ")
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }
    fn<-function(df) {
      if(length(unique(df$dv))<2 || length(unique(df$iv))<2) {
        ans<-list(n=nrow(na.omit(cbind(sex=df$iv, d=df$dv))),
                  test = "-",
                  pvalue = "-",
                  efsize = "-")

      } else {
        cramer_v <- lsr::cramersV(df$iv, df$dv)
        testt <- chisq.test(df$iv, df$dv)

        ans<-list(n=nrow(na.omit(cbind(sex=df$iv, d=df$dv))),
                  test=paste0('χ²(', testt$parameter, ')\uA0=\uA0',
                              format(testt$statistic, digits=0, nsmall = 2, big.mark=' ')),
                  pvalue=danesurowe::report_pvalue_long(testt$p.value),
                  efsize=danesurowe::report_single_value(cramer_v)
        )
      }
      return(as_tibble(ans))
    }
    tab<-mydt %>% data.frame() %>% group_by(gv)  %>% tidyr::nest() %>%
      mutate(ans=map(data, fn)) %>% select(-data) %>% tidyr::unnest() %>%
      mutate(n=format(n, digits=0, big.mark='\uA0'))

    if(language=='PL') {
      tab_c<-c( db_obj$groupvar_label(), 'Wielkość próby', 'Statystyka testowa', 'Istotność', 'V Craméra')
      tab <- setLabels(tab, tab_c)
      caption <- paste0(
        "Statystyki testowe i siła związku dla zależności ",
        db_obj$indepvar_label(TRUE), " vs ", db_obj$depvar_label(TRUE), " w podziale na ", db_obj$groupvar_label(TRUE),
        ". Siła związku jest wyrażona statystyką V Craméra.")
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }

    chapter$insert_table(caption = caption, table_df = tab, tags = c('crosstab_test', 'perason_chi2'))

  } else {
    cramer_v <- lsr::cramersV(mydt$iv, mydt$dv)
    testt <- chisq.test(mydt$iv, mydt$dv)

    if(language=='PL') {
      msg <- paste0("Wykonano test niezależności χ²\uA0 Pearsona na ",
                     danesurowe::liczebnik(liczba = nrow(mydt),
                                           mianownik = 'przypadku', dopelniacz = 'przypadkach',
                                           lmnoga = 'przypadkach', flag_skip_one = FALSE, flag_skip_zero = FALSE),
                     '. Uzyskano wartość statystyki χ²(', testt$parameter, ') równą ',
                     format(testt$statistic, digits=0, nsmall = 2, big.mark='\uA0'),
                     '; ', danesurowe::report_pvalue_long(testt$p.value),'. ')
      if (testt$p.value<0.05) {
        msg<-paste0(msg, "Siła związku wyrażona statystyką V Craméra: ",
                    format(cramer_v, scientific=FALSE, digits=0, nsmall=2, big.mark='\uA0'),
                    '. ')
      }
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }
    chapter$insert_paragraph(msg, tags=c('crosstab_test', 'pearson_chi2'))

  }
  return(chapter)
}


two_by_two_U_test<-function(pAcc, statistics, chapter) {
#  browser()
  db_obj<-pAcc$serve_db()
  language<-pAcc$get_property('language')

  dvlevels<-db_obj$dvlevels()

  mydt<-db_obj$chunkdf_ivdvgv()


  do_test<-function(ldf) {
    df <- ldf[[1]]
    if(length(unique(df$dv))<2) {
      if(as.character(df$dv[[1]])==names(dvlevels)[[1]]) {
        n1 <- nrow(df)
        n2 <- 0
      } else {
        n1 <- 0
        n2 <- nrow(df)
      }
      return(
        data.frame(statistic_txt = '-', pvalue_txt = '-', estimate_txt = '-', n1 = n1, n2 = n2, pXgtY_txt = '-',
                   n1_txt = format(n1, big.mark = '\uA0'), n2_txt = format(n2, big.mark = '\uA0'),
                   p.value=NA, pXgtY = NA, estimate = NA, conf.low = NA, conf.high = NA, statistic = NA)
      )

    } else {
      do_1test<-function(iv, dv) {
        ans<-tryCatch(
          broom::tidy(wilcox.test(x = as.integer(df$iv), y = as.integer(df$dv), conf.int = TRUE)),
          error=function(e) tibble(estimate=NA, statistic=NA, p.value=NA, conf.low=NA, conf.high=NA, method=NA, alternative=NA)
        )
        return(ans)
      }

      ans <- tryCatch(
        df %>% na.omit() %>% mutate_all(as.integer) %>%  group_by(dv) %>% tidyr::nest() %>%
          tidyr::spread_(key_col='dv', value_col='data') %>%
          do(cbind(do_1test(.[[1]], .[[2]]),
                   data.frame(n1=nrow(.[[1]][[1]]), n2=nrow(.[[2]][[1]]))
          )) %>%
          mutate(statistic_txt=danesurowe::report_single_value(statistic),
                 pvalue_txt=danesurowe::report_pvalue_long(p.value),
                 estimate_txt=danesurowe::report_value_with_bounds(estimate, conf.low, conf.high),
                 pXgtY = estimate/n1/n2,
                 n1_txt = danesurowe::report_integer(n1),
                 n2_txt = danesurowe::report_integer(n2),
                 pXgtY_txt = danesurowe::report_single_value(pXgtY)
          ) %>% select(-method, -alternative),
        error=function(e) tibble(estimate=NA, statistic=NA, p.value=NA, conf.low=NA, conf.high=NA, n1=0, n2=0,
                                 statistic_txt='-', pvalue_txt='-', estimate_txt='-', pXgtY=NA, n1_txt=0, n2_txt=0,
                                 pXgtY_txt=NA)
      )

      return(ans)
    }
  }
  if(db_obj$is_grouped()){
    #    browser()
    #    grs<-mydt %>% data.frame() %>% mutate_all(as.integer) %>%  group_by_(.dots=c(groupby)) %>% tidyr::nest()
    grs<-mydt %>% data.frame() %>% group_by(gv) %>% tidyr::nest()

    #browser()
    tab_df <- grs %>% group_by(gv) %>%  do(do_test(.$data))
  } else {
    tab_df <-do_test(list(mydt%>%data.frame()))
  }
  tab_df_txt <- tab_df %>% select(-estimate, -statistic, -p.value, -conf.low, -conf.high, -n1, -n2, -pXgtY) %>%  data.frame()

  if(language=='PL') {
    collabels <- c(db_obj$groupvar_label(), "U Manna-Whitneya", "Istotność",
                   "Przesunięcie rozkładów", "$N_X$", "$N_Y$", "$P(X > Y)$")
    tab_caption <- "Statystyki testu U-Manna-Wilcoxona-Whitneya wraz z estymatami siły związku: przesunięcia rozkładów (wraz z 95% przedziałem ufności) oraz prawdopodobieństwa, że losowo wybrana obserwacja z jednej grupy jest większa od obserwacji z drugiej grupy."
  } else if(language=='EN') {
    browser()
  } else {
    browser()
  }
  setLabels(tab_df_txt, collabels)
  # for(i in seq(ncol(tab_df_txt))) {
  #   setattr(tab_df_txt[[i]], 'label', collabels[[i]])
  # }
  chapter$insert_table(caption=tab_caption, table_df = tab_df_txt, tags = c('crosstab_test', 'u-mww-test'))
  return(chapter)
}

two_by_two_KW_test<-function(pAcc, statistics, chapter) {
  db_obj<-pAcc$serve_db()
  language<-pAcc$get_property('language')

  dvlevels<-db_obj$dvlevels()

  mydt<-db_obj$chunkdf_ivdvgv()
  browser()


  do_test<-function(ldf) {
    df <- ldf[[1]]
    if(length(unique(df$dv))<2) {
      if(as.character(df$dv[[1]])==names(dvlevels)[[1]]) {
        n1 <- nrow(df)
        n2 <- 0
      } else {
        n1 <- 0
        n2 <- nrow(df)
      }
      return(
        data.frame(statistic_txt = '-', pvalue_txt = '-', estimate_txt = '-', n1 = n1, n2 = n2, pXgtY_txt = '-',
                   n1_txt = format(n1, big.mark = '\uA0'), n2_txt = format(n2, big.mark = '\uA0'),
                   p.value=NA, pXgtY = NA, estimate = NA, conf.low = NA, conf.high = NA, statistic = NA)
      )

    } else {
      do_1test<-function(dt) {
        ans<-tryCatch(
          broom::tidy(stats::kruskal.test(dv ~ iv, data=dt)),
          error=function(e) tibble(parameter=NA, statistic=NA, p.value=NA, method=NA)
        )
        return(ans)
      }

      ans <- tryCatch(
        df %>% na.omit() %>% mutate_all(as.integer) %>%  group_by(dv) %>% tidyr::nest() %>%
          tidyr::spread_(key_col='dv', value_col='data') %>%
          do(cbind(do_1test(.[[1]], .[[2]]),
                   data.frame(n1=nrow(.[[1]][[1]]), n2=nrow(.[[2]][[1]]))
          )) %>%
          mutate(statistic_txt=danesurowe::report_single_value(statistic),
                 pvalue_txt=danesurowe::report_pvalue_long(p.value),
                 estimate_txt=danesurowe::report_value_with_bounds(estimate, conf.low, conf.high),
                 pXgtY = estimate/n1/n2,
                 n1_txt = danesurowe::report_integer(n1),
                 n2_txt = danesurowe::report_integer(n2),
                 pXgtY_txt = danesurowe::report_single_value(pXgtY)
          ) %>% select(-method, -alternative),
        error=function(e) tibble(estimate=NA, statistic=NA, p.value=NA, conf.low=NA, conf.high=NA, n1=0, n2=0,
                                 statistic_txt='-', pvalue_txt='-', estimate_txt='-', pXgtY=NA, n1_txt=0, n2_txt=0,
                                 pXgtY_txt=NA)
      )

      return(ans)
    }
  }
  if(db_obj$is_grouped()){
    #    browser()
    #    grs<-mydt %>% data.frame() %>% mutate_all(as.integer) %>%  group_by_(.dots=c(groupby)) %>% tidyr::nest()
    grs<-mydt %>% data.frame() %>% group_by(gv) %>% tidyr::nest()

    #browser()
    tab_df <- grs %>% group_by(gv) %>%  do(do_test(.$data))
  } else {
    tab_df <-do_test(list(mydt%>%data.frame()))
  }
  tab_df_txt <- tab_df %>% select(-estimate, -statistic, -p.value, -conf.low, -conf.high, -n1, -n2, -pXgtY) %>%  data.frame()

  if(language=='PL') {
    collabels <- c(db_obj$groupvar_label(), "U Manna-Whitneya", "Istotność",
                   "Przesunięcie rozkładów", "$N_X$", "$N_Y$", "$P(X > Y)$")
    tab_caption <- "Statystyki testu U-Manna-Wilcoxona-Whitneya wraz z estymatami siły związku: przesunięcia rozkładów (wraz z 95% przedziałem ufności) oraz prawdopodobieństwa, że losowo wybrana obserwacja z jednej grupy jest większa od obserwacji z drugiej grupy."
  } else if(language=='EN') {
    browser()
  } else {
    browser()
  }
  setLabels(tab_df_txt, collabels)
  # for(i in seq(ncol(tab_df_txt))) {
  #   setattr(tab_df_txt[[i]], 'label', collabels[[i]])
  # }
  chapter$insert_table(caption=tab_caption, table_df = tab_df_txt, tags = c('crosstab_test', 'u-mww-test'))
  return(chapter)
}

two_by_two_rho_test<-function(pAcc, statistics, chapter) {
  #browser()
  db_obj<-pAcc$serve_db()
  language<-pAcc$get_property('language')
  db_obj$groupvar_label()
  db_obj$filter_label()
  db_obj$depvar_label()
  db_obj$indepvar_label()

  mydt<-db_obj$chunkdf_ivdvgv()

  if (db_obj$is_grouped()){
    #browser()
    do_test<-function(dv, iv) {
      ans<-tryCatch(
        broom::tidy(Hmisc::rcorr(dv, iv, type=c('crosstab_test', 'spearman'))),
        error=function(e) {
          data.frame(column1=NA, column2=NA, estimate=NA_real_, n=length(dv), p.value=NA_real_)
        }
      )
      return(ans)
    }
    table<-mydt %>%  group_by(gv) %>% do(do_test(.$dv, .$iv))
    #table<-mydt %>%  group_by(gv) %>% do(broom::tidy(Hmisc::rcorr(.$dv, .$iv, type='spearman')))
    tab <- table %>% data.frame() %>% mutate(
      n=format(n, digits=0, big.mark='\uA0'),
      estimate=format(estimate, scientific=FALSE, digits=0, nsmall=2, big.mark='\uA0'),
      p.value=danesurowe::report_pvalue_long(p.value)) %>%
      select(gv, n, estimate, p.value)
    if(language=='PL') {
      colnames(tab) <- c(db_obj$groupvar_label(), 'Wielkość próby', 'ρ Spearmana', 'Istotność')
      setattr(tab, 'caption', paste0(
        "Statystyki testowe i siła związku dla zależności ",
        db_obj$indepvar_label(TRUE), " vs ", db_obj$depvar_label(TRUE), " w podziale na ", db_obj$groupvar_label(TRUE),
        ". Siła związku jest wyrażona statystyką ρ (rho) Spearmana."))
      msg <- paste0('Wykonano ',
                    danesurowe::liczebnik(liczba = nrow(tab),
                                          mianownik = 'test', dopelniacz = 'testy',
                                          lmnoga = 'testów', flag_skip_one = FALSE ),
                    ' na istotność współczynników ρ Spearmana na zbiorze ',
                    db_obj$filter_label(TRUE), '. ')
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }
    chapter$insert_paragraph(msg)
    chapter$insert_table(caption = attr(tab, 'caption'), table_df = tab, tags = c('crosstab_test', 'spearman_test'), flag_header_in_md=TRUE)

  } else {
    browser()
    table<-mydt  %>% do(broom::tidy(Hmisc::rcorr(.$dv, .$iv, type='spearman')))
    if(language=='PL') {
      msg <- paste0("Wykonano test korelacji ρ\uA0Spearmana na ",
                    danesurowe::liczebnik(liczba = table$n,
                                          mianownik = 'przypadku', dopelniacz = 'przypadkach',
                                          lmnoga = 'przypadkach', flag_skip_one = FALSE, flag_skip_zero = FALSE),
                    '. Uzyskano wartość statystyki korelacji ρ(', table$n-2, ') równą ',
                    danesurowe::report_single_value(table$estimate),
                    '; ', danesurowe::report_pvalue_long(table$p.value),'.\n\n')
      chapter$insert_paragraph(msg, tags=c('crosstab_test', 'spearman_test'))
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }

  }
  return(msg)
}

