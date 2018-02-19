get_filters<-function() {
  ans<-list()
  ans$filtr_dzieci_zywe<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony'",
    label="Dzieci żywo urodzone"
  )

  ans$filtr_wszystkie_dzieci<-relationshipMatrix::Filter$new(
    filterstring="",
    label="Cały zbiór danych"
  )

  ans$filtr_dziecko2<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregn_num == 3",
    label="Dzieci żywo urodzone z trzeciej ciąży"
  )

  ans$filtr_dziecko3<-relationshipMatrix::Filter$new(
    filterstring="",
    label="Cały zbiór danych"
  )

  ans$filtr_dziecko4<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregn_num_gr == '4 i więcej'",
    label="Dzieci żywo urodzone z czwartej lub późniejszej ciąży"
  )

  ans$filtr_ciaze_pojedyncze<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype == 1",
    label="Dzieci żywo urodzone w pojedynczej ciąży"
  )

  ans$filtr_ciaze_pojedyncze_chlopcy<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype == 1 & sex =='♂'",
    label="Chłopcy żywo urodzeni w pojedynczej ciąży"
  )

  ans$filtr_ciaze_pojedyncze_dziewczynki<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype == 1 & sex =='♀'",
    label="Dziewczynki żywo urodzone w pojedynczej ciąży"
  )

  ans$filtr_pierwsze_dziecko<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregn_num == 1",
    label="Pierworodne dzieci żywo urodzone"
  )

  ans$filtr_pierwsze_dziecko_chlopcy<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregn_num == 1 & sex =='♂'",
    label="Pierworodni chłopcy żywo urodzoneni"
  )

  ans$filtr_pierwsze_dziecko_dziewczynki<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregn_num == 1 & sex =='♀'",
    label="Pierworodne dziewczynki żywo urodzone"
  )

  ans$filtr_bliznieta<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype == 2",
    label="Dzieci żywo urodzone w dwupłodowej ciąży"
  )

  ans$filtr_bliznieta_chlopcy<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype == 2 & sex == '♂'",
    label="Chłopcy żywo urodzoneni w dwupłodowej ciąży"
  )

  ans$filtr_bliznieta_dziewczynki<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype == 2 & sex == '♀'",
    label="Dziewczynki żywo urodzone w dwupłodowej ciąży"
  )

  ans$filtr_wieloraka<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype > 1",
    label="Dzieci żywo urodzone w wielopłodowej ciąży"
  )

  ans$filtr_wieloraka_chlopcy<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype > 1 & sex == '♂'",
    label="Czhłopcy żywo urodzone w conajmniej trzypłodowej ciąży"
  )

  ans$filtr_wieloraka_dziewczynki<-relationshipMatrix::Filter$new(
    filterstring="death == 'Żywo urodzony' & pregtype > 1 & sex == '♀'",
    label="Dziewczynki żywo urodzone w conajmniej trzypłodowej ciąży"
  )

  ans$filtr_bliznieta_all<-relationshipMatrix::Filter$new(
    filterstring="pregtype > 1",
    label="Dzieci żywe lub martwe urodzone w wielorakiej ciąży"
  )
  return(ans)
}
