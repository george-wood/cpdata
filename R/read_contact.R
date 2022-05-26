read_contact <- function(path) {

  cols <- 
    colnames(
      read_excel(
        last(path),
        sheet        = 2,
        n_max        = 0,
        .name_repair = make_clean_names
      )
    )

  d <- 
    nesting(
      path  = path,
      sheet = map_dbl(path, \(x) str_which(excel_sheets(x), "port")),
      skip  = c(4, 4, 1, 1)
    ) |> 
    rowwise() |> 
    mutate(
      data = list(
        read_excel(path,
                   sheet     = sheet,
                   skip      = skip,
                   col_names = cols) |>
          mutate(across(everything(), as.character))
      )
    ) |> 
    pull(data) |> 
    rbindlist(fill = TRUE) |> 
    distinct(
      eid = paste0("c", 1:n()),
      dt  = parse_date_time(paste(contact_date, str_sub(time_of_stop, -8)),
                            orders = c("ymd_HMS", "dmy_HM")),
      first_poln,
      first_pofn,
      first_po_age_on_date_of_stop,
      sec_poln,
      sec_pofn,
      sec_po_age_on_date_of_stop,
      c_race    = recode_race(sub_race),
      c_gender  = recode_gender(sub_sex),
      type      = contact_type,
      suspicion = fifelse(type == "Suspicious Person", "Y", "N"),
      traffic   = fifelse(type == "Traffic Related",   "Y", "N")
    ) |> 
    filter(
      !str_detect(type, "CCDOC|Victim")
    ) |> 
    pivot_longer(
      cols           = starts_with("first") | starts_with("sec"),
      names_to       = c("officer", "variable"),
      names_sep      = "_",
      values_drop_na = TRUE
    ) |> 
    pivot_wider(
      names_from  = variable,
      values_from = value
    ) |> 
    mutate(
      dt,
      last_name   = poln,
      first_name  = pofn,
      birth_lower = (year(dt) - as.numeric(po)) - 1,
      birth_upper = (year(dt) - as.numeric(po)) + 1,
      role        = fifelse(officer == "first", "first", "second"),
      type        = "cc",
      .keep       = "unused"
    ) |> 
    drop_na(dt)
  
  setDT(d)
  return(d)
  
}
