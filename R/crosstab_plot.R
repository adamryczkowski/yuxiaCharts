crosstab_plot<-function(pAcc, freqdf, chapter){
  #browser()
  db_obj<-pAcc$serve_db()
  flag_force_logit = pAcc$get_property('crosstab.force_logit')
  flag_gr_after_indep = pAcc$get_property('table_group_first')
  language<-pAcc$get_property('language')
  db_obj$depvar_label()
  db_obj$indepvar_label()
  db_obj$groupvar_label()
  db_obj$filter_label()

  if(language=='PL') {
    label <- paste0("Wykres mozaikowy przedstawiający związek zmiennych ", db_obj$indepvar_label(TRUE), " i ", db_obj$depvar_label(TRUE))
    if (db_obj$is_grouped()){
      label<-paste0(label, " w podziale na ", db_obj$groupvar_label(TRUE), ". ")
    } else {
      label<-paste0(label, '. ')
    }

    if(db_obj$filter_label()!='') {
      label <- paste0(label, "Analizę wykonano na zbiorze obserwacji: ", db_obj$filter_label(TRUE))
    }
  } else if(language=='EN') {
    browser()
  } else {
    browser()
  }


  fob_dv <- db_obj$depvar_property('f.o.b')

  pAcc$done_discovery()
  dt<-freqdf
  #Mam zbiór dt. Najpierw filtr. Potem wybieram zmienne



  h <- ggplot(dt, aes(x = iv, y = n, fill = dv)) +
    geom_bar(position = "fill",stat = "identity") +
    scale_y_continuous(labels = scales::percent_format())

  if(identical(fob_dv, 2) )
  {
    h<-h+scale_fill_brewer(palette="Blues", name = db_obj$depvar_label())
  } else {
    h<-h+scale_fill_brewer(palette="Set2", name = db_obj$depvar_label())
  }

  #     ggrepel::geom_label_repel(aes(label = n, y=pos, fill=preg_weight_gr), color = 'white',
  #                      size = 3)
  #geom_text(aes(label = label, y=pos), size = 3, check_overlap = TRUE, color='black')

  if(db_obj$is_grouped()){
    if(length(danesurowe::GetLabels(factor(dt$iv)))>2){
      h <- h + facet_grid(. ~ gv) + xlab(paste0(db_obj$indepvar_label(), " × ", db_obj$groupvar_label())) + ylab(NULL)
    } else {
      h <- h + facet_grid(gv ~ .) + ylab(db_obj$groupvar_label()) + xlab(db_obj$indepvar_label())
    }
  } else {
    h <- h + ylab("") + xlab(db_obj$indepvar_label())
  }

  if (nchar(db_obj$depvar_label())>40){
    h<-h + theme(legend.position="bottom",legend.direction="vertical")
  } else if (nchar(db_obj$depvar_label())>20){
    h<-h + theme(legend.position="bottom")
  }


  xtick_labels <- names(db_obj$ivlevels(TRUE))
  miejsce <- 48 #liczba znaków na wykresie
  miejsce_na_kategorie <- miejsce / length(xtick_labels)


  if(max(nchar(xtick_labels))>miejsce_na_kategorie){
    h <- h + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    h <- h + theme(axis.text.x = rotatedAxisElementText(30,'x'))
  }

  if(pAcc$is_reversed()) {
    chart_prefix = 'mozaic_plot_rev'
  } else {
    chart_prefix = 'mozaic_plot'
  }
  chart_hash<-chapter$insert_ggchart(caption = label, gg = h, chart_prefix = chart_prefix, tags = 'mozaic_plot')

  if(!pAcc$is_reversed()) {
    tmp<-chapter$discard_changes
    chapter$discard_changes<-TRUE
    chart_hash2<-chapter$insert_ggchart(caption = label, gg = h, chart_prefix = 'mozaic_plot_rev', tags = 'mozaic_plot')
    chapter$discard_changes<-tmp
    caption<- paste0("Wartości użyte do wykreślenia wykresu @fig:", chart_hash, " i @fig:", chart_hash2, ". ")

    if(db_obj$is_grouped()) {
      groupby<-'gv'
    } else {
      groupby<-NULL
    }

    tab<-mycrosstabs(df=dt, zz='dv', zn='iv', groupby=groupby, group_lab = db_obj$groupvar_label(TRUE), caption=caption,
                     flag_include_zz_percent=TRUE, flag_include_zn_percent=FALSE, flag_include_percent=TRUE)
    chapter$insert_table(caption=caption, table_df=tab, tags=c('mozaic_plot', 'crosstab'))

  }



  return(chapter)
}








mycrosstabs<-function(df, zz, zn, groupby, group_lab, caption, ...) {
  #browser()
  if(!is.null(groupby)) {
    wzor<-expand.grid(setdiff(unique(df[[zz]]), NA), setdiff(unique(df[[zn]]), NA), stringsAsFactors = FALSE)
    colnames(wzor)<-c(zz,zn)
    wzor$n2<-0
    a<-df %>% select_(.dots=c(groupby,zn,zz,'n')) %>% data.frame() %>% group_by_(groupby) %>% tidyr::nest()

    b<-a %>% mutate(tab = purrr::map(data,
                                     ~dplyr::left_join(wzor, ., by=c(zn,zz)) %>% mutate(n = coalesce(n+n2,0)) %>% select(-n2) %>%
                                       tidyr::spread_(key_col = c(zz), value_col = 'n', fill=0)))

    b<-b %>% mutate(tab = purrr::map(tab, function(a) {
      x<-as.matrix(a[,-1])
      rownames(x) <- as.character(a[[1]])
      return(x)
    } )) %>% select(-data)
    counts<-setNames(b$tab, b[[groupby]])
  } else {
    a<-df %>% select_(.dots=c(zn,zz,'n')) %>% tidyr::spread_(key_col = c(zz), value_col = 'n', fill=0)
    counts<-as.matrix(a[,-1])
    rownames(counts) <- as.character(a[[1]])
    counts<-list(counts)
  }
  ret <- mycrosstab_1(counts=counts, zz=zz, zn=zn, group_lab=group_lab, caption=caption, ...)
  #  ret <- counts %>% purrr::map(mycrosstab_1, zz=zz, zn=zn, group_lab=group_lab, ...)
  return(ret)
  mycrosstab_1() #Tylko po to, aby do digest dołączyło się źródło
}



#Funkcja rysująca wszystkie możliwe warianty tabeli i zwracająca optymalny
mycrosstab_1<-function(counts, zz, zn, group_lab, caption, ...) {
  flag_include_row_percent=TRUE
  flag_include_col_percent=TRUE
  flag_include_percent=TRUE

  tabsidx<-1:3

  tabs1<-purrr::map(tabsidx, mycrosstab_2, counts=counts, group_lab=group_lab, caption=caption,
                    flag_groups_on_top=TRUE, flag_transpose=FALSE, ...)

  if(length(counts)>1) {
    tabs3 <-purrr::map(tabsidx, mycrosstab_2, counts=counts, group_lab=group_lab, caption=caption,
                       flag_groups_on_top=FALSE, flag_transpose=FALSE, ...)
    tabs1 <- c(tabs1, tabs3)
  }

  tabs2<-purrr::map(tabsidx, mycrosstab_2,  counts=counts, group_lab=group_lab, caption=caption,
                    flag_groups_on_top=TRUE, flag_transpose=TRUE, ...)
  if(length(counts)>1) {
    tabs4 <-purrr::map(tabsidx, mycrosstab_2,  counts=counts, group_lab=group_lab, caption=caption,
                       flag_groups_on_top=FALSE, flag_transpose=TRUE, ...)
    tabs2 <- c(tabs2, tabs4)
  }
  tabs<-c(tabs1, tabs2)

  which_exist <- tabs %>% purrr::map('pantab') %>% purrr::map_lgl(~!is.null(.))

  tabs <- tabs[which_exist]
  quals <-tabs %>% purrr::map('tab') %>% purrr::map_dbl(evaluate_qual_of_table)
  which_best <- which.max(quals==max(quals))
  besttab <- tabs[[which_best]]
  if(quals[which_best]==-Inf){
    browser()
  }

  return(besttab$pantab)
  mycrosstab_2() #Tylko po to, aby do digest dołączyło się źródło
}

#Funkcja, która do tabeli dwu-zmiennowej dodaje zmienną grupującą.
mycrosstab_2<-function(type, counts, group_lab, caption, flag_groups_on_top, flag_transpose,
                       flag_include_zz_percent=FALSE, flag_include_zn_percent=TRUE, ...) {

  if(flag_transpose) {
    tab1 <- mycrosstab_3(type = type, counts =  t(counts[[1]]),
                         flag_include_row_percent=flag_include_zn_percent,
                         flag_include_col_percent=flag_include_zz_percent, ...)
  } else {
    tab1 <- mycrosstab_3(type = type, counts =  counts[[1]],
                         flag_include_row_percent=flag_include_zz_percent,
                         flag_include_col_percent=flag_include_zn_percent, ...)
  }
  if(length(counts)>1) {
    if (flag_groups_on_top) {
      tab_width <- ncol(tab1)-1
      tab_height <- nrow(tab1)

      out <- matrix('', nrow = tab_height+1, ncol = 1 + tab_width * length(counts))
      out[2:nrow(out), 1] <- tab1[,1]
      out[1,] <- c(group_lab, as.character(flatten(purrr::map(names(counts), ~c(., rep('', tab_width - 1))))))

      offsetx <- 2
      offsety <- 1
      out[2:nrow(out), offsetx:(offsetx + tab_width-1)] <- tab1[,2:ncol(tab1)]

      for(i in 2:length(counts)){
        offsetx <- 2 + (i-1)*tab_width
        offsety <- 1
        if(flag_transpose) {
          tab <- mycrosstab_3(type = type, counts = t(counts[[i]]),
                              flag_include_row_percent=flag_include_zn_percent,
                              flag_include_col_percent=flag_include_zz_percent, ...)

        } else {
          tab <- mycrosstab_3(type = type, counts = counts[[i]],
                              flag_include_row_percent=flag_include_zz_percent,
                              flag_include_col_percent=flag_include_zn_percent, ...)

        }
        out[2:nrow(out), offsetx:(offsetx + tab_width-1)] <- tab[,2:ncol(tab)]
      }
    } else {
      tab_width <- ncol(tab1)
      tab_height <- nrow(tab1) - 1

      out <- out<-matrix('', nrow = 1 + tab_height * length(counts), ncol = tab_width + 1)
      out[1, 2:ncol(out)] <- tab1[1,]
      out[,1] <- c(group_lab, as.character(flatten(purrr::map(names(counts), ~c(., rep('', tab_height - 1))))))

      offsetx <- 1
      offsety <- 2
      out[offsety:(offsety + tab_height - 1), 2:ncol(out)] <- tab1[2:nrow(tab1),]

      for(i in 2:length(counts)){
        offsetx <- 1
        offsety <- 2 + (i-1)*tab_height
        if(flag_transpose) {
          tab <- mycrosstab_3(type = type, counts = t(counts[[i]]),
                              flag_include_row_percent=flag_include_zn_percent,
                              flag_include_col_percent=flag_include_zz_percent, ...)
        } else {
          tab <- mycrosstab_3(type = type, counts = counts[[i]],
                              flag_include_row_percent=flag_include_zz_percent,
                              flag_include_col_percent=flag_include_zn_percent, ...)
        }
        out[offsety:(offsety + tab_height - 1), 2:ncol(out)] <- tab[2:nrow(tab),]
      }
    }

    ret <- out
  } else {
    ret <- tab1
  }

  emph_cols <- 1
  emph_rows <- 1
  strong_rows <- NULL
  strong_cols <- NULL
  if(length(counts)>1) {
    if (flag_groups_on_top) {
      emph_rows <- 2
      strong_rows <- 1
      emph_cols <- 1
    } else {
      emph_cols <- 2
      strong_cols <- 1
      emph_rows <- 1
    }
  } else {
    emph_cols <- 1
    emph_rows <- 1
  }

  ret_df <- data.frame(ret[2:nrow(ret),])
  names(ret_df) <- ret[1,]

  ans<-tryCatch(
    pantab <- pander::pandoc.table.return(ret_df, caption = caption, split.tables = 100000, use.hyphening = TRUE,
                                          emphasize.italics.rows = emph_rows, emphasize.italics.cols = emph_cols,
                                          emphasize.strong.rows = strong_rows, emphasize.strong.cols = strong_cols),
    error=function(e) e
  )
  if('error' %in% class(ans)) {
    return(list(tab=ret, pantab=NULL))
  }
  return(list(tab=ret, pantab=pantab))
  mycrosstab_3() #Tylko po to, aby do digest dołączyło się źródło
}


#row_percent - wiersz z procentami taki, że sumują się do 100% w wierszu. Row percent wymaga sumowania po prawej stronie, inaczej nie ma sensu.
mycrosstab_3<-function(type=c(1,2,3), counts, flag_include_row_percent, flag_include_col_percent, flag_include_percent) {
  hlabs <- colnames(counts)
  vlabs <- rownames(counts)

  if(type == 1 ) {
    ile_cells_v <- flag_include_row_percent + flag_include_col_percent + flag_include_percent + 1
    ile_cells_h <- 1

    cells_h <- ''
    cells_v <- ''
    pos<-list(val=list(offsetx=1, offsety=1, margin='BR'))
    if(flag_include_col_percent) {
      cells_v <- c(cells_v, '% kolumny')
      pos <- c(pos, list(colp=list(offsetx=1, offsety=length(cells_v), margin='')))
    }
    if(flag_include_row_percent) {
      cells_v <- c(cells_v, '% wiersza')
      pos <- c(pos, list(rowp=list(offsetx=1, offsety=length(cells_v), margin='R')))
    }
    if(flag_include_percent) {
      cells_v <- c(cells_v, '% całości')
      pos <- c(pos, list(perc=list(offsetx=1, offsety=length(cells_v), margin='R')))
    }


  } else if (type ==2) {
    ile_cells_v <- 1
    ile_cells_h <- flag_include_row_percent + flag_include_col_percent + flag_include_percent + 1

    cells_h <- ''
    cells_v <- ''
    pos<-list(val=list(offsetx=1, offsety=1, margin='BR'))
    if(flag_include_col_percent) {
      cells_h <- c(cells_h, '% kolumny')
      pos <- c(pos, list(colp=list(offsetx=length(cells_h), offsety=1, margin='B')))
    }
    if(flag_include_row_percent) {
      cells_h <- c(cells_h, '% wiersza')
      pos <- c(pos, list(rowp=list(offsetx=length(cells_h), offsety=1, margin='')))
    }
    if(flag_include_percent) {
      cells_h <- c(cells_h, '% całości')
      pos <- c(pos, list(perc=list(offsetx=length(cells_h), offsety=1, margin='B')))
    }

  } else if (type == 3) {
    ile_cells_v <- max(1 + flag_include_row_percent, flag_include_col_percent + flag_include_percent)
    ile_cells_h <- max(1 + flag_include_col_percent, flag_include_row_percent + flag_include_percent)

    cells_h <- ''
    cells_v <- ''
    pos<-list(val=list(offsetx=1, offsety=1, margin='BR'))
    if(flag_include_col_percent) {
      cells_h <- c(cells_h, '% kolumny')
      pos <- c(pos, list(colp=list(offsetx=length(cells_h), offsety=1, margin='B')))
    }
    if(flag_include_row_percent) {
      cells_v <- c(cells_v, '% wiersza')
      pos <- c(pos, list(rowp=list(offsetx=1, offsety=length(cells_v), margin='R')))
    }
    if(flag_include_percent) {
      if(length(cells_v)==1) {
        cells_v <- c(cells_v, '% całości')
      } else if (length(cells_h)==1) {
        cells_h <- c(cells_h, '% całości')
      }
      pos <- c(pos, list(perc=list(offsetx=ile_cells_h, offsety=ile_cells_v, margin='')))
    }
  }

  ile_rows <- length(vlabs) * ile_cells_v + 1 + flag_include_col_percent
  ile_cols <- length(hlabs) * ile_cells_h + 1 + flag_include_row_percent
  out<-matrix('', nrow = ile_rows, ncol = ile_cols)


  if(ile_cells_v>1) {
    my_vlabs<-c('Poziomy', unlist(map(vlabs, function(vlab) c(vlab, cells_v[2:ile_cells_v]) )))
  } else {
    my_vlabs<-c('Poziomy', unlist(map(vlabs, function(vlab) vlab )))
  }
  if(flag_include_col_percent) {
    my_vlabs<-c(my_vlabs, 'Suma')
  }
  if(flag_include_col_percent) {
    vlabsEx<-c(vlabs, 'Suma')
  } else {
    vlabsEx<-vlabs
  }

  if(ile_cells_h>1) {
    my_hlabs<-c('Poziomy', unlist(map(hlabs, function(hlab) c(hlab, list(cells_h[2:ile_cells_h]) ))))
  } else {
    my_hlabs<-c('Poziomy', unlist(map(hlabs, function(hlab) hlab)))
  }
  if(flag_include_row_percent) {
    my_hlabs<-c(my_hlabs, 'Suma')
  }
  if(flag_include_row_percent) {
    hlabsEx<-c(hlabs, 'Suma')
  } else {
    hlabsEx<-hlabs
  }

  out[,1] <- my_vlabs
  out[1,] <- my_hlabs

  countsdf<-cbind(Var2=rownames(counts), setNames(data.frame(counts), colnames(counts))) %>% tidyr::gather(key = Var1,  value = n, -Var2) %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

  fill_table<-function(counts, offsetx, offsety, stepx, stepy){
    for(k in seq(nrow(counts))) {
      i <- which(hlabsEx==counts$Var1[[k]])
      j <- which(vlabsEx==counts$Var2[[k]])
      #      cat(paste0("Writing to ",offsetx + (i-1)*stepx,",",offsety + (j-1)*stepy,"\n"))
      out[1+offsety + (j-1)*stepy, 1+offsetx + (i-1)*stepx]<<-counts[[3]][[k]]
    }
  }
  if(flag_include_col_percent) {
    countsEx <- rbind(countsdf, cbind(Var2='Suma', countsdf %>% group_by(Var1)%>% tidyr::nest() %>% mutate(n=purrr::map_dbl(data, function(d) sum(d$n))) %>% select(-data)))
  } else {
    countsEx<-countsdf
  }

  add_sums<-function(df, margin) {
    if(margin=='R' || margin=='BR') {
      ret <- rbind(df%>%ungroup(),
                   cbind(Var1='Suma',
                         df %>%
                           group_by(Var2)%>%
                           tidyr::nest() %>%
                           mutate(n=purrr::map_dbl(data, function(d) sum(d$n))) %>% select(-data)))
    } else if (margin=='B' || margin=='BR') {
      ret <- rbind(df%>%ungroup(),
                   cbind(Var2='Suma',
                         df %>%
                           group_by(Var1)%>%
                           tidyr::nest() %>%
                           mutate(n=purrr::map_dbl(data, function(d) sum(d$n))) %>% select(-data)))
    } else {
      ret<-df
    }
    return(ret)
  }

  countsEx <- add_sums(countsEx, pos$val$margin)
  counts2<-countsEx %>% mutate(n=format(n, scientific=FALSE, big.mark="\u0A0"))
  fill_table(counts2, pos$val$offsetx, pos$val$offsety, ile_cells_h, ile_cells_v)

  if(flag_include_percent)  {
    counts2<-countsdf %>% mutate(n=n/sum(n))
    counts2 <- add_sums(counts2, pos$perc$margin)
    counts2<-counts2 %>% mutate(n=paste0(format(n*100, scientific=FALSE, digits=0, nsmall=2),'%'))
    fill_table(counts2, pos$perc$offsetx, pos$perc$offsety, ile_cells_h, ile_cells_v)
  }

  if(flag_include_col_percent) {
    counts2<-countsdf %>% group_by(Var1) %>%  mutate(n=n/sum(n))
    counts2 <- add_sums(counts2, pos$colp$margin)
    counts2<-counts2 %>% mutate(n=paste0(format(n*100, scientific=FALSE, digits=0, nsmall=2),'%'))
    fill_table(counts2, pos$colp$offsetx, pos$colp$offsety, ile_cells_h, ile_cells_v)
  }

  if(flag_include_row_percent) {
    counts2<-countsdf %>% group_by(Var2) %>%  mutate(n=n/sum(n))
    counts2 <- add_sums(counts2, pos$rowp$margin)
    counts2<-counts2 %>% mutate(n=paste0(format(n*100, scientific=FALSE, digits=0, nsmall=2),'%'))
    fill_table(counts2, pos$rowp$offsetx, pos$rowp$offsety, ile_cells_h, ile_cells_v)
  }

  return(out)

}

