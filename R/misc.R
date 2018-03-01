get_nominal_colors<-function(count, flag_never_user_dashes=FALSE,
                             flag_dont_use_set3=FALSE, flag_use_paired_colors=FALSE) {
  flag_use_lines<-FALSE
  if(flag_use_paired_colors){
    stop("flag_use_paired_colors not implemented yet!")
  }

  set2_maxcolors<-RColorBrewer::brewer.pal.info['Set2',]$maxcolors
  set3_maxcolors<-RColorBrewer::brewer.pal.info['Set3',]$maxcolors

  if(count<=set2_maxcolors) {
    fillc<-RColorBrewer::brewer.pal(count, "Set2")
    linec<-RColorBrewer::brewer.pal(count, "Dark2")
    lines<-rep("solid", count)
  } else if(count<=set3_maxcolors && !flag_dont_use_set3) {
    linec<-RColorBrewer::brewer.pal(count, "Set3")
    fillc<-RColorBrewer::brewer.pal(count, "Set3")
    lines<-rep("solid", count)
  } else if(count<=set2_maxcolors*2 && !flag_never_user_dashes) {
    scale_threshold<-floor(count/2)
    lines<-c(rep("solid", scale_threshold),
             rep("dashed", length(ylabels)-scale_threshold))
    linec<-c(RColorBrewer::brewer.pal(scale_threshold, "Dark2"),
             RColorBrewer::brewer.pal(length(ylabels)-scale_threshold, "Dark2"))
    fillc<-c(RColorBrewer::brewer.pal(scale_threshold, "Set2"),
             RColorBrewer::brewer.pal(length(ylabels)-scale_threshold, "Set2"))
  } else if(length(ylabels)<=set3_maxcolors*2 && !flag_never_user_dashes && !flag_dont_use_set3) {
    scale_threshold<-floor(count/2)
    lines<-c(rep("solid", scale_threshold),
             rep("dashed", length(ylabels)-scale_threshold))
    linec<-c(RColorBrewer::brewer.pal(scale_threshold, "Set3"),
             RColorBrewer::brewer.pal(length(ylabels)-scale_threshold, "Set3"))
    fillc<-lightenc_colors(linec)
  } else {
    lines<-rep("solid", count)
    if (flag_dont_use_set3)  {
      fillc <- colorRampPalette(RColorBrewer::brewer.pal(set2_maxcolors, "Set2"))(count)
      linec <- colorRampPalette(RColorBrewer::brewer.pal(set2_maxcolors, "Dark2"))(count)
    } else {
      linec <- colorRampPalette(RColorBrewer::brewer.pal(set3_maxcolors, "Set3"))(count)
      fillc <- lightenc_colors(linec)
    }
  }
  return(list(fill_colors=fillc, line_colors=linec, line_styles=lines))
}

set_nominal_colors<-function(h, level_count, flag_never_user_dashes=FALSE, flag_dont_use_set3=FALSE, labels=NULL) {
  cols<-get_nominal_colors(level_count,
                           flag_never_user_dashes=flag_never_user_dashes,
                           flag_dont_use_set3=flag_dont_use_set3)

  if(!is.null(labels)) {
    h<-h +
      scale_linetype_manual(values = cols$line_styles, labels=labels) +
      scale_color_manual(values = cols$line_colors, labels=labels) +
      scale_fill_manual(values = cols$fill_colors, labels=labels)
  } else {
    h<-h +
      scale_linetype_manual(values = cols$line_styles) +
      scale_color_manual(values = cols$line_colors) +
      scale_fill_manual(values = cols$fill_colors)
  }
  return(list(h=h, flag_use_lines=length(unique(cols$line_styles))>1))
}

set_xlabels<-function(h, zzlab, xlabels, ylabels, flag_y_percent=FALSE){
  if (nchar(zzlab)>20){
    h<-h+theme(legend.position="bottom")
  }

  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(xlabels)

  if(flag_y_percent) {
    h <- h + scale_y_continuous(labels = scales::percent)
  }
  if(length(xlabels)==0) {
    xlabels_len<-0
  } else {
    xlabels_len<-max(nchar(xlabels))
  }

  if(xlabels_len>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }

  if(length(ylabels)==0) {
    ylabels_len<-0
  } else {
    ylabels_len<-max(nchar(ylabels))
  }

  if(ylabels_len>40){
    h<-h+theme(legend.direction = 'vertical')
  }
  return(h)
}

#Build Function to Return Element Text Object
rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1];
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (-angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}

setLabels<-function(tab, labels) {
  real_name<-deparse(substitute(tab))
  if(! 'data.frame' %in% class(tab)) {
    browser()
    stop(paste0("Object ", real_name, " is not data.frame, but ", paste0(class(tab), collapse = ', '), ". "))
  }

  if(ncol(tab)!=length(labels)) {
    browser()
    stop(paste0("There must be exactly ", ncol(tab), " labels, not ", length(labels)))
  }
  for(i in seq_along(labels)) {
    setattr(tab[[i]], 'label', labels[[i]])
  }
  return(tab)
}
