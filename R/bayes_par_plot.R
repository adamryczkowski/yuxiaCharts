#Wykres, który rysuje wykres z rozkładami parametrów bayesowskich
#
#fits - albo obiekt typu rstan, albo nazwana lista tych obiektów
#pars - vektor tekstowy nazw parametrów do wyjęcia
#prob_inner - jaki % zbioru zakreskować? To nie jest HDI
par_unit_names='π'
par_units=1/2
par_labels=NULL
point_est<-'mean'
pars<-'fi'

bayes_par_plot<-function(fits, pars, par_labels=NULL,  hdi_proc = 0.95, prob_inner = 0.5,
                         point_est = c("median", "mean", "none"),
                         flag_groups_as_colors=FALSE, flag_pars_as_colors=TRUE, par_units=1, par_unit_names='') {

  if(length(point_est)>1){
    point_est<-'mean'
  }
  if(!is.null(par_labels)){
    par_labels<-rep(par_labels, length.out=length(pars))
  } else {
    par_labels<-pars
  }
  labels<-list()
  labels[['par']]<-par_labels

  par_units<-rep(par_units, length.out=length(pars))
  par_unit_names<-rep(par_unit_names, length.out=length(pars))
  prob_outer<-0.95
  prob_inner<-0.5 #To remove
  probs <- c(0.5 - prob_outer/2, 0.5 - prob_inner/2, 0.5,
             0.5 + prob_inner/2, 0.5 + prob_outer/2)
  if('rstan' %in% class(fits) || length(fits)==1) {
    if(!'rstan' %in% class(fits)) {
      fit <- fits[[1]]
    } else {
      fit <- fits
    }
    pars_df <- as.data.table(rstan::extract(fit, pars))
    for (j in seq(pars)) {
      if(par_units[[j]]!=1){
        l1[,(pars[[j]]):=l1[[ pars[[j]] ]]/par_units[[j]] ]
      }
    }
  } else {
    labels[['groupby']]<-names(fits)
    l1 <- as.data.table(rstan::extract(fits[[1]], pars))
    #Rename the columns
    setnames(l1, pars, paste0(pars, '_', stringr::str_pad(1, width=log10(length(fits)+1), pad = "0")) )
    for (j in seq(pars)) {
      if(par_units[[j]]!=1){
        varname<-paste0(pars[[j]], '_', stringr::str_pad(1, width=log10(length(fits)+1), pad = "0"))
        l1[,(varname):=l1[[varname]]/par_units[[j]] ]
      }
    }

    for(i in seq(2, length(fits))){
      l2 <- as.data.table(rstan::extract(fits[[i]], pars))
      for (j in seq(pars)) {
        l1[,(paste0(pars[[j]], '_', stringr::str_pad(i, width=log10(length(fits)+1), pad = "0"))):=l2[[j]]/par_units[[j]] ]
      }
    }
    if(exists('l2'))
      rm(l2)
    pars_df <- l1 %>% as_tibble %>% select(starts_with(paste0(pars[[1]],"_"))) %>% tidyr::gather() %>%
      select_(.dots=setNames(c('key', 'value'),c('groupby', pars[[1]]))) %>% data.table
    pars_df[,groupby:=factor(pars_df$groupby, labels = names(fits))]
    if(length(pars)>1) {
      for (j in seq(2, length(pars))) {
        tmp_df <- l1 %>% as_tibble %>% select(starts_with(paste0(pars[[j]],"_"))) %>% tidyr::gather() %>%
          select_(.dots=setNames('value',pars[[j]]))
        pars_df[,(pars[[j]]):=tmp_df[[1]] ]
      }
      rm(tmp_df)
    }
    pars_df <- pars_df %>% as_tibble %>% group_by(groupby)
    rm(l1)

  }
  if(length(pars)>1) {
    if(exists('groupby', pars_df)) {
      plot_df <- pars_df %>% tidyr::gather(key = 'par', value = 'value', -groupby, factor_key = TRUE) %>% group_by(groupby, par)
    } else {
      plot_df <- pars_df %>% tidyr::gather(key = 'par', value = 'value', factor_key = TRUE) %>% group_by(par)
    }
  } else {
    plot_df <- pars_df %>% rename_(value=pars[[1]])
    if(exists('groupby', pars_df)) {
      plot_df<-plot_df %>% group_by(groupby)
    }
  }
  if(exists('groupby', where = plot_df)) {
    grattr<-attr(fits, 'label')
    if(!is.null(grattr)) {
      setattr(plot_df$groupby, 'label', grattr)
    }
  }

  gc()
  #Algorytm:
  #Najpierw przyporządkowywujemy kolor, bo takie mamy flagi
  #Następnie offset, najpierw dla par, a potem dla grupy
  color_what <- ''
  facet_what <-'groupby'
  offset_what <- 'par'
  grps<-character(0)
  if (length(pars)>1) {
    grps<-c(grps, 'par')
    if(flag_pars_as_colors) {
      color_what <- 'par'
      if(length(fits)>1){
        offset_what <- 'groupby'
      } else {
        offset_what <- ''
      }
      facet_what <- ''
    }
  }

  if (length(fits)>1) {
    grps<-c(grps, 'groupby')
    if(flag_groups_as_colors) {
      if (color_what != ''){
        stop("You cannot encode both groups and variables as color")
      }
      color_what <- 'groupby'
      if(length(pars)>1){
        offset_what <- 'par'
      } else {
        offset_what <- ''
      }
      facet_what <- ''
    }
  }
  if(facet_what=='groupby' && length(fits)==1){
    facet_what=''
  } else if (facet_what=='par' && length(pars)==1){
    facet_what=''
  }

  if(offset_what=='groupby' && length(fits)==1){
    offset_what=''
  } else if (offset_what=='par' && length(pars)==1){
    offset_what=''
  }

  if(facet_what!='' && offset_what==''){
    offset_what <- facet_what
    facet_what <- ''
  }

  probs_txt<-c('left', 'leftCI', 'median', 'rightCI', 'right')


  quantiles_df <- plot_df %>%
    do(data.frame(prob=probs_txt, quantile=quantile(.$value, probs = probs), mean=mean(.$value), sd=sd(.$value))) %>%
    group_by_(.dots = c(grps, 'prob')) %>%
    tidyr::spread_(key_col='prob', value_col = 'quantile')
  if(color_what != '') {
    if(offset_what !='') {
      quantiles_df <- quantiles_df %>%
        mutate_(group = paste0(as.character(offset_what), ":", as.character(color_what)))
    } else {
      quantiles_df <- quantiles_df %>%
        mutate_(group = as.character(color_what))
    }
  } else {
    if(offset_what != '') {
      quantiles_df <- quantiles_df %>%
        mutate_(group = paste0(as.character(offset_what)))
    }
  }

  if(exists('groupby', where = quantiles_df)) {
    if (exists('par', where = quantiles_df)) {
      quantiles_df<-quantiles_df %>% mutate(filter_str = paste0("groupby == '", as.character(groupby), "' && par == '", as.character(par), "'")) %>%
        mutate(y = 1 + (offset_what=='groupby') * (as.numeric(groupby)-1) + (offset_what=='par')*(as.numeric(par)-1))
    } else {
      quantiles_df<-quantiles_df %>% mutate(filter_str = paste0("groupby == '", as.character(groupby), "'")) %>%
        mutate(y = 1 + (offset_what=='groupby') * (as.numeric(groupby)-1))
    }
  } else {
    quantiles_df<-quantiles_df %>% mutate(filter_str = paste0("par == '", as.character(par), "'"))
  }


  quantiles_df <- data.table(quantiles_df)
  if(exists('groupby', where = quantiles_df)) {
    setattr(quantiles_df$groupby, 'label', attr(fits, 'label'))
  }
  if(exists('par', where = quantiles_df)) {
    setattr(quantiles_df$par, 'label', "Parametr")
  }

  n_dens_pts <- 512
  y_dens <- matrix(0, nrow = n_dens_pts, ncol = nrow(quantiles_df))
  x_dens <- matrix(0, nrow = n_dens_pts, ncol = nrow(quantiles_df))
  y_poly <- NULL
  x_poly <- NULL
  last_row_poly<-0
  poly_groupby<-integer(0)
  poly_par = integer(0)
  poly_group = integer(0)

  for (i in seq(nrow(quantiles_df))) {
    filtr <- quantiles_df$filter_str[[i]]
    var <- (plot_df %>% dplyr::filter_(filtr))$value
    d_temp <- density(var, n=n_dens_pts, from=min(var), to = max(var))
    x_dens[, i] <- d_temp$x
    y_max <- max(d_temp$y)
    y_dens[, i] <- d_temp$y/y_max * 0.8 + quantiles_df$y[[i]]
    hdis<-HDInterval::hdi(d_temp, credMass=hdi_proc, allowSplit = TRUE)
    q1<-hdis[,'begin']
    q3<-hdis[,'end']

    quantiles_df[i, hdi_lower:=min(q1)]
    quantiles_df[i, hdi_upper:=max(q3)]

    #    q1<-quantiles_df$leftCI[[i]]
    #    q3<-quantiles_df$rightCI[[i]]
    for(j in seq(length(q1))) {
      last_row_poly<-last_row_poly+1
      d_temp <- density(var, n=n_dens_pts, from=q1[[j]], to=q3[[j]])
      if(is.null(x_poly)) {
        y_poly <- data.table(p1=rep(0.,  n_dens_pts+2))
        x_poly <- data.table(p1=rep(0.,  n_dens_pts+2))
      }
      x_poly[, (paste0("p",last_row_poly)):=c(d_temp$x[1], as.vector(d_temp$x),
                                              d_temp$x[n_dens_pts])]
      y_poly[, (paste0("p",last_row_poly)):=c(0, as.vector(d_temp$y)/y_max *
                                                0.8, 0) + quantiles_df$y[[i]] ]
      if(exists('groupby', where = quantiles_df)) {
        poly_groupby[last_row_poly]<-quantiles_df$groupby[[i]]
      }
      if(exists('par', where = quantiles_df)) {
        poly_par[last_row_poly]<-quantiles_df$par[[i]]
      }
      poly_group[last_row_poly]<-paste0(quantiles_df$group[[i]],":",j)
    }

    point_value.x <- quantiles_df[[point_est]][[i]]
    point_value.pos <- which.min(abs(point_value.x - x_dens[, i]))
    point_value.y <- y_dens[point_value.pos, i]
    quantiles_df[i, point_value_y:=point_value.y]
    if(length(pars)>1) {
      par_pos<-match(as.character(quantiles_df$par[[i]]), pars)
    } else {
      par_pos<-1
    }
    quantiles_df[i, par_unit_name:=par_unit_names[[par_pos]]]
    quantiles_df[i, par_unit:=par_units[[par_pos]]]
  }

  #  browser()


  h<-ggplot(quantiles_df) + aes_string(group='group' )
  if(color_what!=''){
    h<-h+aes_string(color=color_what, fill=color_what)
  }
  if(facet_what!=''){
    h<-h+aes_string(facet=facet_what)
  }

  h <- h + geom_vline(xintercept = 0, na.rm = TRUE, color = "gray40", size = 0.5) #Linia dla wartości parametru zero
  if(point_est!='none' && point_est!='') {
    h<-h + geom_segment(aes_string(x=point_est, xend=point_est, y='y', yend='point_value_y'))
  }
  if(exists('groupby', quantiles_df)) {
    if(exists('par', quantiles_df)) {
      df_dens <- data.frame(x = as.vector(x_dens), y = as.vector(y_dens),
                            groupby = rep(quantiles_df$groupby, each = n_dens_pts),
                            par = rep(quantiles_df$par, each = n_dens_pts),
                            group = rep(quantiles_df$group, each = n_dens_pts))
      df_poly <- data.frame(x = as.vector(data.matrix(x_poly)), y = as.vector(data.matrix(y_poly)),
                            groupby = factor(rep(poly_groupby, each = n_dens_pts+2), labels=levels(quantiles_df$groupby)),
                            par = factor(rep(poly_par, each = n_dens_pts+2), labels=levels(quantiles_df$par)),
                            group = rep(poly_group, each = n_dens_pts+2))
    } else {
      df_dens <- data.frame(x = as.vector(x_dens), y = as.vector(y_dens),
                            groupby = rep(quantiles_df$groupby, each = n_dens_pts),
                            group = rep(quantiles_df$group, each = n_dens_pts))
      df_poly <- data.frame(x = as.vector(data.matrix(x_poly)), y = as.vector(data.matrix(y_poly)),
                            groupby = factor(rep(poly_groupby, each = n_dens_pts+2), labels=levels(quantiles_df$groupby)),
                            group = rep(poly_group, each = n_dens_pts+2))
    }
  } else {
    df_dens <- data.frame(x = as.vector(x_dens), y = as.vector(y_dens),
                          par = rep(quantiles_df$par, each = n_dens_pts),
                          group = rep(quantiles_df$group, each = n_dens_pts))
    df_poly <- data.frame(x = as.vector(data.matrix(x_poly)), y = as.vector(data.matrix(y_poly)),
                          par = factor(rep(poly_par, each = n_dens_pts+2), labels=levels(quantiles_df$par)),
                          group = rep(poly_group, each = n_dens_pts+2))
  }
  flag_set_colors=FALSE
  if(length(fits)>1) {
    if(flag_groups_as_colors ) {
      h <- h + aes(colour = groupby, fill=groupby)
      flag_set_colors=TRUE
      color_len<-length(fits)
    }
  }
  if(flag_pars_as_colors && length(pars)>1) {
    if(flag_set_colors) {
      stop("You cannot encode both parameters and groups as colors on the same plot. Sorry!")
    }
    h <- h + aes(colour = par, fill=par)
    flag_set_colors=TRUE
    color_len<-length(pars)
  }
  if(flag_set_colors) {
    h<-set_nominal_colors(h, level_count=color_len, flag_never_user_dashes=TRUE, flag_dont_use_set3=TRUE,
                          labels=labels[[color_what]])$h
    h<-h+labs(color=danesurowe::GetVarLabel(quantiles_df, color_what),
              fill=danesurowe::GetVarLabel(quantiles_df, color_what))
  }

  h<-h+geom_line(data=df_dens, mapping=aes(x=x, y=y)) + aes(group=group)
  offset_groups<-danesurowe::GetLevels(quantiles_df[[offset_what]])
  h<-h+scale_y_continuous(breaks = offset_groups, labels=labels[[offset_what]])


  h<-h + geom_polygon(data = df_poly, mapping=aes(x = x, y = y), alpha=0.3, color=0) +aes(group=group)
  if(offset_what!=''){
    h<-h+labs(y=danesurowe::GetVarLabel(plot_df, offset_what))
  }

  if(length(pars)>1) {
    h<-h+labs(x='Wartość parametrów')
  } else {
    if(par_unit_names!=''){
      h<-h+labs(x=paste0(par_labels, ' [', par_unit_names, ']'))
    } else {
      h<-h+labs(x=paste0(par_labels))
    }
    if(par_unit_names!='') {
      breaks<-scales::pretty_breaks(5)(c(min(quantiles_df$left), max(quantiles_df$right)))
      h<-h + scale_x_continuous(breaks = breaks,
                                labels=paste0(breaks, 'π'))
    }
  }

  if(color_what!=''){
    if (nchar(danesurowe::GetVarLabel(plot_df, color_what))>20){
      h<-h+theme(legend.position="bottom")
    }
    if(max(nchar(as.character(danesurowe::GetLabels(quantiles_df[[color_what]]))))>40){
      h<-h+theme(legend.direction = 'vertical')
    }
  }

  quantiles_df <- quantiles_df %>% select(-left, -right, -filter_str, -y, -point_value_y, -group)

  xlim<-c(min(quantiles_df$leftCI), max(quantiles_df$rightCI))
  xlim<-(xlim - mean(quantiles_df$median))*3 + mean(quantiles_df$median)
  xlim2<-c(min(quantiles_df$hdi_lower), max(quantiles_df$hdi_upper))
  h <- h + coord_cartesian(xlim = c(max(xlim[[1]], xlim2[[1]]), min(xlim[[2]], xlim2[[2]])))

  if(length(unique(quantiles_df$par_unit_name))==1) {
    flag_show_units=FALSE
    common_unit<-quantiles_df$par_unit_name[[1]]
    quantiles_df <- quantiles_df %>%
      mutate(par_unit_name='')
  } else {
    common_unit<-''
    quantiles_df <- quantiles_df %>%
      mutate(par_unit_name=ifelse(par_unit_name=='','',paste0(' [',par_unit_name,']')))
    flag_show_units=TRUE
  }

  quantiles_df <- quantiles_df %>%
    mutate(
      mean = paste0(danesurowe::report_value_with_error(mean, ci = sd), par_unit_name),
      median = paste0(danesurowe::report_single_value(median), par_unit_name),
      CI_left = paste0(danesurowe::report_single_value(hdi_lower), par_unit_name),
      CI_right = paste0(danesurowe::report_single_value(hdi_upper), par_unit_name)
    )

  setattr(quantiles_df$mean, 'label', 'Średnia ± odch. stand.')
  setattr(quantiles_df$median, 'label', 'Mediana')
  setattr(quantiles_df$CI_left, 'label', 'Lewa granica HDI')
  setattr(quantiles_df$CI_right, 'label', 'Prawa granica HDI')
  if(exists('par', where = quantiles_df)) {
    quantiles_df$par <- factor(quantiles_df$par, labels = par_labels)
  }

  small_table <- quantiles_df %>% select(-sd, -par_unit_name, -par_unit, -leftCI, -rightCI, -hdi_lower, -hdi_upper)
  return(list(h=h, tab=small_table, quantiles_df=quantiles_df ))
}
