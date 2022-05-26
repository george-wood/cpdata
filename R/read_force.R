read_force <- function(path) {

  d <-
    left_join(
      read_sheet(path, expression = "TRR|Sheet"),
      read_sheet(path, expression = "Action"),
      by = "trr_report_id"
    ) |>
    transmute(
      eid         = trr_report_id,
      # rd_no,
      dt          = coalesce(ymd_hm(datetime),
                             ymd_hm(paste(convert_to_date(dte),
                                          str_pad(tmemil, 4, pad = "0")))),
      on_duty     = fifelse(str_detect(dutystatus, "Y"), "Y", "N"),
      last_name   = polast,
      first_name  = pofirst,
      appointed   = convert_to_date(appointed_date,
                                    string_conversion_failure = "warning"),
      c_injured   = fifelse(str_detect(subject_injured, "Y"), "Y", "N"),
      c_race      = recode_race(subrace),
      c_gender    = recode_gender(subgndr),
      officer     = fifelse(str_detect(person, "Member"), "Y", "N"),
      action,
      description = otherdescr
    )

  setDT(d)
  return(d)

}
