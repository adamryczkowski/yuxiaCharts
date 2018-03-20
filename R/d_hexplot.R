hexplot_dispatch<-function(pAcc) {
  pAcc$set_report_dispatcher(hexplot_reports)
  pAcc$done_discovery()
  #Do nothing else
}

hexplot_reports<-function(pAcc, statistics) {
  plots=list(
    hexplot=plot_hexplot,
    hexplot_inv=function(pAcc, statistics, chapter) {
      pAcc$reverse_vars()
      plot_hexplot(pAcc, statistics, chapter)
    }
  )
}



plot_hexplot<-function(pAcc, tmp, chapter) {

  filter_lab <- labs$filter_lab

  if(groupby == ''){
    groupby <- NULL
  } else {
    grlab <- labs$grlab
    grlab_md <- labs$grlab_md
  }

  label <- paste0("Wykres typu hex plot ilustrujący gęstość rozrzutu zmiennych ", zzlab_md, " i ", znlab_md, ". Środkowa krzywa jest krzywą regresji loess. " )
  if (!is.null(groupby)){
    label<-paste0(label, "Kolorami oznaczono wykresy odpowiadające poziomom ", grlab_md, ". ")
  }
  label <- paste0(label, "Analizę wykonano na zbiorze: ", labs$filter_lab)

  mydt <- dt

  auto_binwidth <- function(varname) {
    uniq <- (unique(dt[[varname]]) %>% na.omit())

    if (length(uniq) < 40) {
      return(mean(diff(sort(uniq)))* 7/ 6)
    }
    minmax <- quantile(mydt[[varname]], probs = c(0.025, 0.975))
    span <- as.numeric(diff(minmax))
    #    span <- max(mydt[[varname]])-min(mydt[[varname]])
    npoints <- sum(uniq>minmax[[1]] & uniq<minmax[[2]])
    if (npoints > 100) {
      npoints <- 100
    }
    return(2 *
             (span+1)/npoints * 2
    )
  }

  maxsize <- 35000
  if (nrow(mydt)>maxsize){
    smoothdata <- mydt %>% sample_n(size = maxsize)
  } else {
    smoothdata <- mydt
  }

  breaks_auto <- function(varname) {
    uniq <- (unique(dt[[varname]]) %>% na.omit())

    if (length(uniq) < 20) {
      return(sort(uniq))
    } else {
      return(ggplot2::waiver())
    }
  }
  #  browser()
  h <- ggplot(mydt, aes_string(x = zn, y = zz)) +
    geom_hex(data=mydt, mapping = aes_string(x = zn, y = zz), na.rm=TRUE, binwidth = c(auto_binwidth(zn),auto_binwidth(zz) )) +
    scale_fill_gradient2(name = "Częstość", mid='white', high='black',labels=function(x) {format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)})

  if(!is.null(groupby))
  {
    h <- h + geom_smooth(method='loess', data=smoothdata, mapping = aes_string(x = zn, y = zz, color=groupby), show.legend=TRUE, n=100)
    h <- h + labs(fill=grlab, color=grlab)
  } else {
    h <- h +  geom_smooth(method='loess', data=smoothdata, mapping = aes_string(x = zn, y = zz), show.legend=TRUE, n=100)
  }
  h <- h + scale_x_continuous(limits=as.numeric(quantile(mydt[[zn]], na.rm=TRUE, probs = c(0.025, 0.975)))) +
    scale_y_continuous(breaks = breaks_auto(zz), sec.axis = dup_axis(name=' ') ) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(znlab)+
    ylab(zzlab)

  return(list(chart=h, label=label))
}

