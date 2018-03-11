#Here we define all the Aggregate types

dimorphism1_fun<-function(dt, i, j=1){
  stats <- dt[i,list(m = mean(preg_weight, na.rm=TRUE), sd = sd(preg_weight, na.rm=TRUE), n=sum(!is.na(preg_weight)) ), by='sex']
  ib <- which(stats$sex == '♂')
  ig <- which(stats$sex == '♀')
  if(length(ib)==0) {
    return(list(value=NA, error=NA))
  }
  if(length(ig)==0) {
    return(list(value=NA, error=NA))
  }
  mb <- stats$m[[ib]]
  mg <- stats$m[[ig]]
  sdb <- stats$sd[[ib]]
  sdg <- stats$sd[[ig]]
  nb <- stats$n[[ib]]
  ng <- stats$n[[ig]]
  return(list(value=(mb - mg)/mb,
              error=sqrt((mg/mb/mb*sdb)^2/nb +
                           (sdg/mb)^2/ng)
  ))
}

dimorphism1_fun2<-function(dt, i){
  stats <- dt[i,list(m = mean(preg_weight, na.rm=TRUE), sd = sd(preg_weight, na.rm=TRUE), n=sum(!is.na(preg_weight)) ), by='sex']
  ib <- which(stats$sex == '♂')
  ig <- which(stats$sex == '♀')
  if(length(ib)==0) {
    return(list(value=NA, error=NA))
  }
  if(length(ig)==0) {
    return(list(value=NA, error=NA))
  }
  mb <- stats$m[[ib]]
  mg <- stats$m[[ig]]
  return((mb - mg)/mb)
}


dimorphism2_fun<-function(dt,i){
  stats <- dt[i,list(m = mean(preg_weight, na.rm=TRUE), sd = sd(preg_weight, na.rm=TRUE), n=sum(!is.na(preg_weight)) ), by='sex']
  ib <- which(stats$sex == '♂')
  ig <- which(stats$sex == '♀')
  if(length(ib)==0) {
    return(list(value=NA, error=NA))
  }
  if(length(ig)==0) {
    return(list(value=NA, error=NA))
  }
  mb <- stats$m[[ib]]
  mg <- stats$m[[ig]]
  sdb <- stats$sd[[ib]]
  sdg <- stats$sd[[ig]]
  nb <- stats$n[[ib]]
  ng <- stats$n[[ig]]
  return(list(value=(mb - mg)/sdb,
              error=sqrt((sdg/sdb)^2/ng+
                           1/nb+
                           ((mb-mg)/sdg/sdg)*((sdg/(ng-1)))^2/2)
  ))
}

dimorphism2_fun2<-function(dt,i){
  stats <- dt[i,list(m = mean(preg_weight, na.rm=TRUE), sd = sd(preg_weight, na.rm=TRUE), n=sum(!is.na(preg_weight)) ), by='sex']
  ib <- which(stats$sex == '♂')
  ig <- which(stats$sex == '♀')
  if(length(ib)==0) {
    return(list(value=NA, error=NA))
  }
  if(length(ig)==0) {
    return(list(value=NA, error=NA))
  }
  mb <- stats$m[[ib]]
  mg <- stats$m[[ig]]
  sdb <- stats$sd[[ib]]
  return((mb - mg)/sdb)
}

prop_plci_fn<-function(dt, i){
  stats <- dt[i,list(n=.N), by='sex']
  ib <- which(stats$sex == '♂')
  ig <- which(stats$sex == '♀')
  if(length(ib)==0) {
    nb <- 0
  } else {
    nb <- stats$n[[ib]]
  }
  if(length(ig)==0) {
    ng <- 0
  } else {
    ng <- stats$n[[ig]]
  }


  return(list(value=nb/(nb+ng),
              error=sqrt((ng/(nb+ng)^2)^2 * nb+
                           (nb/(nb+ng)^2)^2 * ng)
  ))
}

prop_plci_fun2<-function(dt, i){
  stats <- dt[i,list(n=.N), by='sex']
  ib <- which(stats$sex == '♂')
  ig <- which(stats$sex == '♀')

  if(length(ib)==0) {
    nb <- 0
  } else {
    nb <- stats$n[[ib]]
  }
  if(length(ig)==0) {
    ng <- 0
  } else {
    ng <- stats$n[[ig]]
  }

  return(nb/(nb+ng))
}


prop_death_fun<-function(dt, i){
  stats <- dt[i,list(n=.N), by='death']
  ib <- which(stats$death == 'Martwo urodzony')
  ig <- which(stats$death == 'Żywo urodzony')

  if(length(ib)==0) {
    nb <- 0
  } else {
    nb <- stats$n[[ib]]
  }
  if(length(ig)==0) {
    ng <- 0
  } else {
    ng <- stats$n[[ig]]
  }

  return(list(value=ng/(nb+ng),
              error=sqrt((ng/(nb+ng)^2)^2 * nb+
                           (nb/(nb+ng)^2)^2 * ng)
  ))
}

prop_death_fun2<-function(dt, i){
  stats <- dt[i,list(n=.N), by='death']
  ib <- which(stats$death == 'Martwo urodzony')
  ig <- which(stats$death == 'Żywo urodzony')

  if(length(ib)==0) {
    nb <- 0
  } else {
    nb <- stats$n[[ib]]
  }
  if(length(ig)==0) {
    ng <- 0
  } else {
    ng <- stats$n[[ig]]
  }

  return(ng/(nb+ng))
}


allAggregates<-function() {
  ans<-list()
  ans$dymorfizm1<-relationshipMatrix::AggregateType$new(name = 'dymorfizm1',
                                                        label = 'Dymorfizm płciowy masy noworodka, wzór 1',
                                                        fn = dimorphism1_fun, fn2 = dimorphism1_fun2, index_var = 'sex', vars = 'preg_weight')
  ans$dymorfizm2<-relationshipMatrix::AggregateType$new(name = 'dymorfizm2',
                                                        label = 'Dymorfizm płciowy masy ciała noworodka',
                                                        fn = dimorphism2_fun, fn2 = dimorphism2_fun2, index_var = 'sex', vars = 'preg_weight')
  ans$prop_plci<-relationshipMatrix::AggregateType$new(name = 'prop_plci',
                                                        label = 'Udział liczby urodzeń chłopców',
                                                        fn = prop_plci_fn, fn2 = prop_plci_fun2, index_var = 'sex',
                                                        theoretical_min = '0', theoretical_max = '1', unit = '%')
  ans$prop_death<-relationshipMatrix::AggregateType$new(name = 'prop_death',
                                                        label = 'Udział liczby urodzeń żywych',
                                                        fn = prop_death_fun, fn2 = prop_death_fun2, index_var = 'death',
                                                        theoretical_min = '0', theoretical_max = '1', unit = '%')
  return(ans)
}
