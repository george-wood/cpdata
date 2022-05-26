read_arrest <- function(path) {

  p701162 <-
    tibble(sheet = excel_sheets(path[str_detect(path, "17972")])) |>
    filter(str_detect(sheet, "Data")) |>
    rowwise() |>
    mutate(
      data = list(
        read_excel(
          path         = path[str_detect(path, "17972")],
          sheet        = sheet,
          .name_repair = make_clean_names,
          col_names    = ifelse(sheet == "Arrest Data I", TRUE, FALSE)
        )
      )
    ) |>
    pull(data) |>
    rbindlist(use.names = FALSE) |>
    filter(cb != "J")

  p708085 <-
    tibble(sheet = excel_sheets(path[str_detect(path, "18089")])) |>
    filter(str_detect(sheet, "Sheet")) |>
    rowwise() |>
    mutate(
      data = list(
        read_excel(
          path         = path[str_detect(path, "18089")],
          sheet        = sheet,
          .name_repair = make_clean_names
        )
      )
    ) |>
    pull(data) |>
    rbindlist(use.names = TRUE) |>
    filter(cb_no != "J")

  d <-
    p708085[, datetime := NULL][
      p701162,
      on = c(cb_no         = "cb",
             employee_role = "role",
             first_name    = "po_first_name",
             last_name     = "po_last_name")
    ][,
      .(eid       = cb_no,
        dt        = dmy_hm(arrest_date_time),
        last_name,
        first_name,
        appointed = ymd(appointed_date),
        birth     = as.numeric(yob),
        role      = tolower(
          str_split(employee_role, " ", simplify = TRUE)[, 1]
        ),
        c_race    = recode_race(arrestee_race),
        c_gender  = recode_gender(arrestee_gender),
        statute,
        type      = charge_type)
    ]
  d <- distinct(d)

  setDT(d)
  return(d)

}
