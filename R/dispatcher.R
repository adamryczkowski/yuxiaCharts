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

classify_cell<-function(rownr, tododf){
  depvar_prefix<-getOption('relationshipMatrix.property_depvar_prefix')
  dv_is_aggregate_propname<-paste0(depvar_prefix, getOption('relationshipMatrix.is_aggregate'))
  if(tododf[[dv_is_aggregate_propname]][[rownr]]) {
    depvartype<-'W'
    depvarwyliczany<-TRUE
  } else {
    depvarwyliczany<-FALSE
    depvartype <- as.character(tododf[[paste0(depvar_prefix, 'vartype')]][[rownr]])
    depvartype <- switch(depvartype,
                         B='F',
                         F='F', 'L'='F', S='F',
                         I='N', N='N', D='D', T='D' )
  }

  indepvar_prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
  iv_is_aggregate_propname<-paste0(indepvar_prefix, getOption('relationshipMatrix.is_aggregate'))
  if(tododf[[iv_is_aggregate_propname]][[rownr]]) {
    indepvartype<-'W'
    indepvarwyliczany<-TRUE
  } else {
    indepvarwyliczany<-FALSE
    indepvartype <- as.character(tododf[[paste0(indepvar_prefix, 'vartype')]][[rownr]])
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
  list(hexplot=hexplot_dispatch,
       boxplot=boxplot_dispatch,
       crosstab=crosstab_dispatch,
       ts_trend=ts_trend_dispatch,
       ts_nominal=ts_nominal_dispatch,
       dwa_wyliczenia=dwa_wyliczenia_dispatch,
       XY_wyliczany=XY_wyliczany_dispatch,
       boxplot_wyliczany=boxplot_aggregate_dispatch,
       ts_wyliczany=ts_wyliczany_dispatch)
}
