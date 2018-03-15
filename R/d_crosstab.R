crosstab_dispatch<-function(pAcc) {
#  browser()
  pAcc$set_report_dispatcher(crosstab_reports)
  db_obj<-pAcc$serve_db()
  bootstrap_n<-pAcc$get_property('logit.bootstrap_n', validator = relationshipMatrix::validate_int, default = 10000)

  dvlevels<-db_obj$dvlevels(TRUE)
  ivlevels<-db_obj$ivlevels(TRUE)
  language<-pAcc$get_property('language')
  flag_force_logit<-pAcc$get_property('crosstab.force_logit', validator = relationshipMatrix::validate_bool, default = FALSE)
  if(is.na(flag_force_logit)) {
    flag_force_logit<-FALSE
  } else {
    flag_force_logit<-as.logical(flag_force_logit)
  }

  if(length(dvlevels)<2) {
    return()
  }
  if(length(ivlevels)<2) {
    return()
  }

  mydt<-db_obj$chunkdf_ivdvgv()
  if(db_obj$is_grouped()) {
    mydt<-mydt %>% group_by(dv, iv, gv) %>% summarise(n=n()) %>%
      group_by(gv, iv) %>%
      mutate(frq=n/sum(n), zero=1-cumsum(lag(frq, default=0)), pos = zero - frq/2)
  } else {
    mydt<-mydt %>% group_by(dv, iv) %>% summarise(n=n()) %>%
      group_by(iv) %>%
      mutate(frq=n/sum(n), zero=1-cumsum(lag(frq, default=0)), pos = zero - frq/2)
  }

  flag_logit = FALSE
  flag_logit_rev = FALSE
  if(sum(mydt$frq < 0.01)>1 || flag_force_logit) {
    flag_logit_rev = FALSE
    flag_logit = FALSE
    if (length(dvlevels)==2)
    {
      flag_logit=TRUE
    }
    if (length(ivlevels)==2)
    {
      flag_logit_rev=TRUE
    }
  }
  pAcc$put_property('crosstab.logit', flag_logit)
  pAcc$put_property('crosstab.logit_rev', flag_logit_rev)

  logit_df1<-NULL
  logit_df2<-NULL
  freqdf<-mydt
  if(flag_logit || flag_logit_rev ){
    get_quantiles<-function(dt, bootstrap_n, varname) {
      #    browser()
      get_q <- function(var) {
        npos<-sum(var==2)
        nneg<-sum(var==1)
        qs <- c(logit_beta_quantiles(a = npos + 1, b = nneg + 1))
        ans<-list(q05=qs[[paste0(format(2.5), '%')]], q25=qs[['25%']], q50=qs[['50%']],
                  q75=qs[['75%']], q95=qs[[paste0(format(97.5), '%')]], npos=npos, nneg=nneg)
        return(ans)
      }
      ans<-c(list(varname = varname), get_q(as.integer(dt[[varname]])))
      return(ans)
    }

    #browser()
    mydt<-as.data.table(db_obj$chunkdf_ivdvgv())
    if (flag_logit) {
      if(db_obj$is_grouped()) {
        logit_df1<-mydt[, as.data.table(get_quantiles(.SD, bootstrap_n=bootstrap_n, varname='dv')), by = c('gv', 'iv')]
      } else {
        logit_df1<-mydt[, as.data.table(get_quantiles(.SD, bootstrap_n=bootstrap_n, varname='dv')), by = c('iv')]
      }
      logit_df1 <- data.table(logit_df1 %>% mutate(m = Vectorize(car::logit, vectorize.args = 'p')(c((1+npos)/(npos+nneg+2)), adjust=FALSE)))
      danesurowe::copy_dt_attributes(mydt, logit_df1)
      setattr(logit_df1$m,'label',Hmisc::label(mydt$iv))
      setattr(logit_df1$m, 'level1', danesurowe::GetLabels(factor(mydt$dv))[[2]])
      setattr(logit_df1$m, 'level0', danesurowe::GetLabels(factor(mydt$dv))[[1]])
    }
    if (flag_logit_rev) {
      if(db_obj$is_grouped()) {
        logit_df2<-mydt[, as.data.table(get_quantiles(.SD, bootstrap_n=bootstrap_n, varname='iv')), by = c('gv', 'dv')]
      } else {
        logit_df2<-mydt[, as.data.table(get_quantiles(.SD, bootstrap_n=bootstrap_n, varname='iv')), by = c('dv')]
      }
      logit_df2 <- data.table(logit_df2 %>% mutate(m = Vectorize(car::logit, vectorize.args = 'p')(c((1+npos)/(npos+nneg+2)), adjust=FALSE)))
      danesurowe::copy_dt_attributes(mydt, logit_df2)
      setattr(logit_df2$m,'label',Hmisc::label(mydt$iv))
      setattr(logit_df2$m, 'level1', danesurowe::GetLabels(factor(mydt$iv))[[2]])
      setattr(logit_df2$m, 'level0', danesurowe::GetLabels(factor(mydt$iv))[[1]])
      data.table::setnames(logit_df2, 'dv', 'iv')
    }
  }
  return(list(freqdf=freqdf, logit_df1=logit_df1, logit_df2=logit_df2))
}

logit_beta_quantiles<-function(a,b,bootstrap_n=10000, quantiles = c(0.025,0.25,0.5,0.75,0.975)) {
  s<-mydistinv(a,b,runif(bootstrap_n))
  qs<-quantile(s, c(0.025,0.25,0.5,0.75,0.975))
  return(c(qs, sd=sd(s)))
}

mydistinv<-function(a,b,p) {
  log(-1+1/(1-zipfR::Rbeta.inv(p, a, b)))
}

crosstab_reports<-function(pAcc, statistics) {
#  browser()
  db_obj<-pAcc$serve_db()

  dvlevels<-db_obj$dvlevels(TRUE)
  ivlevels<-db_obj$ivlevels(TRUE)
  language<-pAcc$get_property('language')
  msg<-''
  if(length(dvlevels)<2) {
    if(language=='PL') {
      msg<-paste0("Zależności nie można policzyć, bo zmienna ", db_obj$depvar_label(TRUE), " ma tylko jeden poziom: „", names(dvlevels), '”. \n')
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }
  }
  if(length(ivlevels)<2) {
    if(language=='PL') {
      msg<-paste0(msg, "Zależności nie można policzyć, bo zmienna ", db_obj$indepvar_label(TRUE), " ma tylko jeden poziom: „", names(ivlevels), '”.')
    } else if(language=='EN') {
      browser()
    } else {
      browser()
    }
  }

  if(nchar(msg)>0) {
    return(list(error=function(pAcc, statistics, chapter) {chapter$insert_paragraph(msg)}))
  }

  plots<-list(
    two_by_two_test=two_by_two_test,
    crosstab=function(pAcc, statistics, chapter) crosstab_plot(pAcc, statistics$freqdf, chapter),
    crosstab_inv=function(pAcc, statistics, chapter) {
      pAcc$reverse_vars()
      freqdf<-statistics$freqdf
      freqdf$tmp<-freqdf$iv
      freqdf$iv<-freqdf$dv
      freqdf$dv<-freqdf$tmp
      crosstab_plot(pAcc, freqdf, chapter)
    }
  )

  if(!is.null(statistics$logit_df1)) {
    plots<-c(plots, list(
      plot_logit=function(pAcc, statistics, chapter) {
        pAcc$put_property('logit.rev', FALSE)
        plot_logit(pAcc, statistics$logit_df1, chapter)
      }
    ))
  }

  if(!is.null(statistics$logit_df2)) {
    plots<-c(plots, list(
      plot_logit=function(pAcc, statistics, chapter) {
        pAcc$reverse_vars()
        pAcc$put_property('logit.rev', TRUE)
        plot_logit(pAcc, statistics$logit_df2, chapter)
      }
    ))
  }

  pAcc$done_discovery()
  return(plots)
}

