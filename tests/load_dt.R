loaddt<-function() {
  if(exists('dt')) {
    if('data.frame' %in% class(dt)) {
      return(dt)
    }
  }
  dt<-readRDS('tests/dt.rds')
  return(dt)
}

