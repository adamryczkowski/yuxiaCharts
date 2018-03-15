#dt musi mieć format:
#dv - jedna zmienna zależna
#iv - zmienna, która będzie kodować kolory
#gv - zmienna, która będzie kodować facets
#counts - wartości, których widmo mamy liczyć
plot_periodogram<-function(pAcc, chapter, plot_df){
  db_obj<-pAcc$serve_db()

  flag_is_groupby<-db_obj$is_grouped()
  language<-pAcc$get_property('language')
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()
  flag_include_smoothed<-pAcc$get_property('include_smoothed_periodogram')
  if(is.na(flag_include_smoothed)) {
    flag_include_smoothed<-TRUE
  } else {
    flag_include_smoothed<-as.logical(flag_include_smoothed)
  }

  ylabels<-db_obj$dvlevels(flag_recalculate = TRUE)

  dt<-db_obj$chunkdf_ivdvgv()
  #browser()
  fn_create_1_spectrum_df <- function(dt) {
    if(sum(dt$counts)==0) {
      return(data.frame(Frequency=seq(1/6, (year(max(dt$iv)) - year(min(dt$iv))+1), 1/6),
                        Spectrum=rep(0, nrow(dt)/2)))
    }
    myts<-ts(scale(dt$counts), start=year(min(dt$iv)), frequency = 12)
    ggfortify:::fortify.spec(spec.pgram(myts, plot=FALSE, detrend=TRUE))
  }

  fn_create_2_spectrum_df <- function(dt) {
    if(sum(dt$counts)==0) {
      return(data.frame(Frequency=seq(1/6, (year(max(dt$iv)) - year(min(dt$iv))+1), 1/6),
                        Spectrum=rep(0, nrow(dt)/2)))
    }
    myts<-ts(scale(dt$counts), start=year(min(dt$iv)), frequency = 12)
    return(ggfortify:::fortify.spec(spec.pgram(myts, plot=FALSE, spans = c(3,3), detrend=TRUE)))
  }

  if(!flag_is_groupby) {
    mydt <- plot_df %>% data.frame() %>% group_by(dv)
  } else {
    mydt <- plot_df %>% data.frame() %>% group_by(gv, dv)
  }

  spec1<-mydt %>% do(fn_create_1_spectrum_df(.)) %>%
    rename(spec1='Spectrum' )
  if(flag_include_smoothed) {
    spec2<-mydt %>% do(fn_create_2_spectrum_df(.)) %>%
      rename(spec2='Spectrum' )

    if(!flag_is_groupby) {
      specs<-inner_join(spec1, spec2, by = c("dv", "Frequency"))
    } else {
      specs<-inner_join(spec1, spec2, by = c("gv", "dv", "Frequency"))
    }
  } else {
    specs <- spec1
  }

  if(flag_include_smoothed) {
    h <- ggplot(specs, aes(Frequency, colour=dv)) + geom_line(mapping=aes(y=spec1), linetype='dotted') +
      geom_line(mapping=aes(y=spec2)) + geom_point(mapping=aes(y=spec2))
  } else {
    h<-ggplot(specs, aes(Frequency, colour=dv, fill=dv)) + geom_line(mapping=aes(y=spec1)) + geom_area(mapping=aes(y=spec1), alpha=0.3)
  }

  if(flag_is_groupby) {
    h<- h + facet_wrap( ~gv)
  }

  ans <- set_nominal_colors(h, level_count = length(ylabels), flag_never_user_dashes = TRUE)
  h <- ans$h + xlab(NULL) + ylab(NULL) + theme(axis.title.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.ticks.y=element_blank()) +
    scale_y_continuous(breaks = NULL)

  if(language=='PL') {
    yrs.labels <- c('dwuletni', 'roczny', 'półroczny',  'kwartalny', 'dwumiesięczny')
  } else if (language == 'EN')  {
    yrs.labels <- c('2-year', 'year', 'half year',  'quartal', '2-month')
  } else {
    browser()
  }
  yrs.period <- c(1/2, 1, 2, 4, 6)


  chardvlab <- max(nchar(c(db_obj$depvar_label(), ylabels)))
  xlabels<-setNames(yrs.period, yrs.labels)

  h<-set_xlabels(h = h, zzlab = db_obj$depvar_label(), xlabels = yrs.labels, ylabels = ylabels, flag_y_percent = FALSE)


  h <- h + labs(color=db_obj$depvar_label())
  # if (charznlab>40){
  #   h<-h+theme(legend.position="bottom",legend.direction="vertical")
  # } else if (charznlab>20){
  #   h<-h+theme(legend.position="bottom")
  # }
  h<-h + scale_x_continuous(breaks=yrs.period, labels=yrs.labels, minor_breaks = NULL)

  if(language=='PL') {
    label <- paste0('Standaryzowane periodogramy dla częstości występowania każdego z poziomów ', db_obj$indepvar_label(TRUE))
    if(flag_is_groupby) {
      label<-paste0(label, ' w podziale na ', db_obj$groupvar_label(TRUE))
    }
    label <- paste0(label, '. Periodogram policzono przy pomocy procedury `spec.pgram` pakietu R. ')
    if(flag_include_smoothed) {
      label <- paste0(label, "Linią przerywaną oraz punktami zaznaczono periodogram (bez wygładzania),",
                      ' natomiast linią ciągłą periodogram wygładzony przy pomocy metody Daniella z szerokościami',
                      ' jądra wygładzania (_spans_) równymi 3 i 3. ')
    } else {
      label <- paste0(label, "Linią zaznaczono periodogram wygładzony przy pomocy metody Daniella z szerokościami",
                      ' jądra wygładzania (_spans_) równymi 3 i 3. ')
    }
    label<-paste0(label, "Oś Y wyznacza gęstość widma, tj. kwadrat transformaty Fouriera sygnału. Jednostką gęstości jest kwadrat jednostki zmiennej zależnej pomnożony przez jednostkę czasu")

    if(!is.null(attr(plot_df$counts, 'units'))) {
      unit<-attr(plot_df$counts, 'units')
      nazwa<-nice_unit_name(unit = reg_time_unit, flag_plural_form = FALSE, language = language)
      label<-paste0(label, ", tj. `", unit, "\uA0×\uA0", unit, "\ua0×\ua0", nazwa, "`")
    }
    label<-paste0(label, '. Nie zaznaczono jednostki na osi Y, aby nie przeładować ryciny informacjami. Bezwzględne wartości widma nie są tak ważne jak kształt wykresu.')

  } else if (language == 'EN') {
    browser()
  } else {
    browser()
  }
#  browser()
  chapter$insert_chart(caption = label, gg = h, chart_prefix = 'periodogram', tags = 'periodogram')

  return(chapter)
}


