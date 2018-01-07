classify_analyses<-function(tododf)
{
  i = 1
  klasy <- rep('', nrow(tododf))
  depvartype <- klasy
  indepvartype <- klasy
  for (i in seq(nrow(tododf))) {
    ans <- classify_cell(i, tododf)
    klasy[[i]] <- ans$klasa
    depvartype[[i]] <- ans$depvartype
    indepvartype[[i]] <- ans$indepvartype
  }
  dispatchers<-get_dispatchers()
  return(list(tododf=cbind(tododf, dispatcher=klasy, indepvartype=indepvartype, depvartype=depvartype),
              dispatchers=dispatchers))
}

classify_row<-function(rownr, tododf){
  if(tododf$depvar.is_aggregate) {
    depvartype<-'W'
    depvarwyliczany<-TRUE
  } else {
    depvarwyliczany<-FALSE
    depvartype <- tododf$depvar.vartype
    depvartype <- switch(depvartype,
                         B='F',
                         F='F', 'L'='F', S='F',
                         I='N', N='N', D='D', T='D' )
  }

  if(tododf$indepvar.is_aggregate) {
    indepvartype<-'W'
    indepvarwyliczany<-TRUE
  } else {
    indepvarwyliczany<-FALSE
    indepvartype <- tododf$indepvar.vartype
    indepvartype <- switch(indepvartype,
                         B='F',
                         F='F', 'L'='F', S='F',
                         I='N', N='N', D='D', T='D' )
  }

  varstype <- paste0(indepvartype,depvartype)
  if (!indepvarwyliczany && !depvarwyliczany) {
    if(varstype == 'NN')
    {
      klasa <- 'hexplot'
    } else if (varstype == 'NF' || varstype == 'FN') {
      klasa <- 'boxplot'
    } else if (varstype == 'FF') {
      klasa <- 'crosstab'
    } else if (varstype %in% c('DN', 'ND')) {
      klasa <- 'ts_trend'
    } else if (varstype == 'DD') {
      browser() #Nie mamy wykresów Date-Date. Należy wyprodukować scatterplot
    } else if (varstype %in% c('DF', 'FD')) {
      klasa <- 'ts_nominal'
    } else {
      browser()
    }
  } else if (varstype == 'WW') {
    klasa <- 'dwa_wyliczenia'
  } else if(varstype %in% c('WN', 'NW')) {
    klasa <- 'XY_wyliczany'
  } else if (varstype %in% c('WF','FW')) {
    klasa <- 'boxplot_wyliczany'
  } else if (varstype %in% c('DW', 'WD')) {
    klasa <- 'ts_wyliczany'
  } else {
    browser()
  }
  return(list(depvartype=depvartype, indepvartype = indepvartype, klasa = klasa))
}

get_dispatchers<-function() {
  list(hexplot=hexplot,
       boxplot=boxplot_dispatch,
       crosstab=crosstab,
       ts_trend=ts_trend,
       ts_nominal=ts_nominal,
       dwa_wyliczenia=dwa_wyliczenia,
       XY_wyliczany=XY_wyliczany,
       boxplot_wyliczany=boxplot_wyliczany,
       ts_wyliczany=ts_wyliczany)
}
