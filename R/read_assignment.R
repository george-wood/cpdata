read_assignment <- function(path) {

  d <-
    clean_names(
      rbindlist(
        lapply(path,
               fread,
               header = TRUE,
               drop = c(5, 10, 19:21),
               na.strings = ""),
        use.names = FALSE
      )
    )[present_for_duty == TRUE & is.na(absence_cd),
      .(aid        = 1:.N,
        dt_start   = get_dt(aa_date, start_time),
        dt_end     = fifelse(start_time < end_time,
                             get_dt(aa_date, end_time),
                             get_dt(aa_date, end_time) %m+% days(1)),
        watch,
        unit       = str_pad(unit, width = 3, side = "l", pad = "0"),
        beat,
        last_name  = last_nme,
        first_name = first_nme,
        appointed  = as_date(appointment_date),
        birth      = year_of_birth,
        star       = as.character(star_no),
        race       = recode_race(race),
        gender     = recode_gender(gender),
        rank,
        modified_date)
    ]
  
  # for duplicate rows, take the latest modified
  setkey(d, last_name, first_name, appointed, birth, dt_start)
  d <- 
    d[d[, .I[which.max(modified_date)], by = key(d)]$V1]
  
  # generate officer identifier
  d[, oid := .GRP, by = setdiff(key(d), "dt_start")]
  
  return(d)
  
}

