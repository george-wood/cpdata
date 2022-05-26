read_isr <- function(path) {
  
  d <-
    read_excel(
      path,
      sheet        = 2, 
      col_types    = "text",
      .name_repair = make_clean_names
    ) |> 
    distinct(
      eid          = contact_card_id,
      contact_date_time,
      fo_last,
      fo_first,
      fo_appointed_dt,
      fo_birth_yr,
      so_last,
      so_first,
      so_appointed_date,
      so_birth_yr,
      c_race       = recode_race(race),
      c_gender     = recode_gender(sex_code_cd),
      enforcement  = enforcement_action_taken_i,
      suspicion    = fifelse(other_reasonable_suspicion_i == "Y" |
                               suspect_narcotic_activity_i == "Y" |
                               suspicious_object_i == "Y",
                             "Y", "N"),
      pat_down     = pat_down_i,
      search       = search_i,
      traffic      = vehicle_involved_i
    ) |> 
    pivot_longer(
      cols           = starts_with("fo") | starts_with("so"),
      names_to       = c("officer", "variable"),
      names_sep      = "_",
      values_drop_na = TRUE
    ) |> 
    pivot_wider(
      names_from  = variable,
      values_from = value
    ) |> 
    mutate(
      dt         = dmy_hm(contact_date_time),
      last_name  = last,
      first_name = first,
      appointed  = convert_to_date(appointed),
      birth      = as.numeric(birth),
      role       = fifelse(officer == "fo", "first", "second"),
      type       = "isr",
      .keep = "unused"
    ) |> 
    replace_na(list(suspicion = "N"))
  
  setDT(d)
  return(d)
  
}
