loaddt<-function() {
  if(exists('dt')) {
    if('data.frame' %in% class(dt)) {
      return(dt)
    }
  }
  dt<-readRDS('/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/cache/dt_lgasga.rds')
#  dt<-readRDS('tests/dt.rds')
  return(dt)
}

