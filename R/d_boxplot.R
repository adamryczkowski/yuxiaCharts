boxplot_dispatch<-function(property_accessor) {
  plots <- tibble(plotfunction=rep('boxplot',2), filename=c('violinplot', 'errorpoints'), do_violinplot=c(FALSE, TRUE))

  labs<-get_labels(zz=zz, zn=zn, filtr=filtr, groupby=groupby, dt=mydt)

  msg <- boxplot_comments(mydt = mydt, zn = zn, zz = zz, groupby = groupby, filtr=filtr, labs = labs, hash=hash)

  return(list(plots=plots, rap_postfix=msg))
}

