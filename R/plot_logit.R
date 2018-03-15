plot_logit<-function(pAcc, dt, chapter){
  #browser()

  db_obj<-pAcc$serve_db()
  flag_gr_after_indep = pAcc$get_property('table_group_first')
  language<-pAcc$get_property('language')
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()
  inv<-pAcc$get_property('logit.rev')
  ivfob<-pAcc$get_property('iv.f.o.b')
  gvfob<-pAcc$get_property('gv.f.o.b')
  nat_order = !pAcc$get_property('logit.sort_iv_levels', relationshipMatrix::validate_bool, NA)
  flag_add_count_boxes<-pAcc$get_property('logit.add_count_boxes', relationshipMatrix::validate_bool, TRUE)
  ivlevels<-db_obj$ivlevels(TRUE)
  bootstrap_n<-pAcc$get_property('logit.bootstrap_n', relationshipMatrix::validate_int, 500)

  pAcc$done_discovery()
  mydt<-dt

  zzlevel1 <- attr(dt$m, 'level1') #danesurowe::GetLabels(factor(mydt[[zz]]))[[1]]
  zzlevel0 <- attr(dt$m, 'level0')


  xmeans_df <- dt %>% group_by(iv) %>% mutate(xx=mean(m))
  xmeans <- (dt %>% group_by(iv) %>% summarize(xx=mean(m)) %>% select(xx))[[1]]
  if(is.na(nat_order)) {
    if(!identical(ivfob, 2) && length(ivlevels)>3 ) {
      if (all(order(xmeans_df$xx) == seq_along(xmeans))) {
        nat_order = TRUE
      } else if (all(order(xmeans) == rev(seq_along(xmeans)))) {
        nat_order = TRUE
      } else {
        nat_order = FALSE
      }
    } else {
      nat_order = TRUE
    }
  }

  if(!nat_order) {
    dt <- xmeans_df %>% arrange(gv, xx) %>% select(-xx)
    if('factor' %in% class(dt$iv)) {
      dt$iv<-factor(dt$iv,levels(dt$iv)[order(xmeans)])
    } else {
      stop("Zamiana poziomów zmiennej innej niż klasy factor nie zaimplementowana")
    }
  }

  if(language=='PL') {
    label <- paste0("Wykres typu box plot ilustrujący rozkład logitu udziału ", zzlevel1,
                    " wśród ", db_obj$filter_label(TRUE), " w podziale na ", db_obj$indepvar_label(TRUE),
                    ". Rozkład został policzony metodą Monte-Carlo z analitycznego wzoru na odwróconą dystrybuantę ",
                    "rozkładu logit ze zmiennej z rozkładu beta dla n = ",
                    bootstrap_n, " symulacji. 95% kwantyle tego rozkładu zostały oznaczone wąsami. " )
    if (db_obj$is_grouped()){
      label<-paste0(label, "Kolorami oznaczono wykresy odpowiadające poziomom ", db_obj$groupvar_label(TRUE), ". ")
    }
    if(!nat_order) {
      label <- paste0(label, "Poszczególne poziomy ", db_obj$indepvar_label(TRUE), " zostały posortowane rosnąco. ")
    }
    if(flag_add_count_boxes) {
      label <- paste0(label, "W chmurkach podano wielkość zbioru, na którym dany box został policzony. ")
    }
  } else if(language=='EN') {
    browser()
  } else {
    browser()
  }


  #  setattr(label, 'pandoc_attributes', paste0('{#fig:', paste0(hash), '}'))


  #Mam zbiór dt. Najpierw filtr. Potem wybieram zmienne

  #  browser()

  if(db_obj$is_grouped()) {
    h<-ggplot(data = dt, mapping = aes(y = q50, x = iv, fill = gv, colour=gv))
    if(identical(gvfob,2) )
    {
      h<-h+scale_fill_brewer(palette="Blues") + scale_color_grey(start = 0.5, end=0)
    } else {
      h<-h+scale_fill_brewer(palette="Set2") + scale_color_brewer(palette="Dark2")
    }
  } else {
    h<-ggplot(data = dt, mapping = aes(y = q50, x = iv))
  }

  #browser()
  maj_breaks<-scales::extended_breaks()(grDevices::extendrange(plogis(dt$q50), f=0.1))
  h <- h + scale_y_continuous(
    breaks=qlogis(maj_breaks),
    labels = scales::percent(maj_breaks))

  #browser()
  mydt<-db_obj$chunkdf_ivdvgv()
  ylabel<-paste0(db_obj$depvar_label(), ": p(", danesurowe::GetLabels(factor(mydt$dv))[[2]], ")")
  if(nchar(ylabel)> 50) {
    ylabel<-paste0("p(", danesurowe::GetLabels(factor(mydt$dv))[[2]], ")")
  }

  h <- h + geom_boxplot(aes(middle=q50, lower=q25, upper=q75, ymin=q05, ymax=q95), stat='identity') +
    ylab(ylabel) + xlab(db_obj$indepvar_label())
  if(db_obj$is_grouped()){
    grlab<-db_obj$groupvar_label()
    h <- h + labs(fill=grlab, color=grlab)
    if (nchar(grlab)>40){
      h<-h+theme(legend.position="bottom",legend.direction="vertical")
    } else if (nchar(grlab)>20){
      h<-h+theme(legend.position="bottom")
    }
  }
  xtick_labels <- names(db_obj$ivlevels())
  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(xtick_labels)


  if(max(nchar(xtick_labels))>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }

  gvlevels<-db_obj$gvlevels()

  if(flag_add_count_boxes) {
    if(db_obj$is_grouped()) {
      if(flag_gr_after_indep) {
        #grlevels<-db_obj$gvlevels()
        df <- dt %>% mutate(x=as.integer(iv) + (2*(as.integer(gv)-1.5)/length(gvlevels)/2)) %>% data.table()
      } else {
        df <- dt %>% mutate(x=as.integer(iv) + (2*(as.integer(gv)-1.5)/length(gvlevels)/2)) %>% data.table()
      }
    } else {
      df <- dt %>%  mutate(x=as.integer(iv)) %>% data.table()
    }
    df[,label:=danesurowe::report_integer(npos+nneg)]

    library(ggrepel)
    h <- h + annotate("label_repel", x=df$x, y=df$m, label=df$label, box.padding = unit(1, "lines"))

  }

  if(inv) {
    chart_prefix<-'logit_inv'
  } else {
    chart_prefix<-'logit'
  }
  chart_hash<-chapter$insert_chart(caption=label, gg=h, tags='logit-plot', chart_prefix=chart_prefix)


  if(db_obj$is_grouped()) {
    cnames <- grlab
  } else {
    cnames <- character(0)
  }

  #  cnames <- c(cnames, znlab_md, "$Q_{2,5\\%}$", "Dolny kwartyl", "Mediana", "Górny kwartyl", "$Q_{97,5\\%}$", paste0("N ", zzlevel1), paste0("N ", zzlevel0), "Wartość")
  if (language=='PL') {
    cnames <- c(cnames, db_obj$indepvar_label(), "Kwantyl 2,5%", "Dolny kwartyl", "Mediana", "Górny kwartyl", "Kwantyl 97,5%", paste0("N ", zzlevel1), paste0("N ", zzlevel0), "Wartość")

    cap<-paste0("Dane wykorzystane w wykresie @fig:", chart_hash, " . Kwantyle zostały policzone na podstawie symulacji Monte-Carlo, natomiast Wartość jest wartością punktową dla danej grupy. Aby pozbyć się nieskończoności w przypadkach, gdy liczności jednej z grup są równe 0, policzono logit z dodaną sztucznie 1 obserwacją do obu grup. ")
  } else if(language=='EN') {
    browser()
  } else {
    browser()
  }
  dt <- data.frame(dt %>% select(-varname))
  setLabels(dt, cnames)
  #browser()
  # for(i in seq(ncol(dt))) {
  #   setattr(dt[[i]], 'label', cnames[[i]])
  # }
  #  colnames(dt) <- cnames
  chapter$insert_table(caption=cap, table_df = dt, tags='logit-plot')

  return(chapter)
}
