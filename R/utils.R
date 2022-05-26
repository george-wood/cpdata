read_sheet <- function(path, expression) {
  nesting(path,
          sheet = map(path, excel_sheets)) |> 
    unnest(cols = sheet) |> 
    filter(str_detect(sheet, expression)) |> 
    rowwise() |> 
    transmute(
      data = list(
        read_excel(path,
                   sheet,
                   .name_repair = make_clean_names,
                   col_types = "text")
      )
    ) |> 
    pull(data) |> 
    rbindlist(fill = TRUE)
}

recode_race <- function(x) {
  x <- str_to_lower(x)
  case_when(
    str_detect(x, "asian")    ~ "asian_pacific_islander",
    str_detect(x, "alaskan")  ~ "indigenous",
    str_detect(x, "hispanic") ~ "hispanic",
    str_detect(x, "black")    ~ "black",
    str_detect(x, "white")    ~ "white",
    TRUE ~ NA_character_
  )
}

recode_gender <- function(x) {
  x <- str_to_lower(x)
  case_when(
    x %in% c("f", "female") ~ "female",
    x %in% c("m", "male")   ~ "male",
    TRUE ~ NA_character_
  )
}

get_dt <- function(date, time, format = "%d-%b-%Y %H%M") {
  time <- str_pad(time, side = "l", width = 4, pad = "0")
  fast_strptime(paste(date, time), lt = FALSE, format = format)
}

get_response <- function(query) {
  case_when(
    str_detect(query, "qa") ~ rlang::syms("arrest"),
    str_detect(query, "qf") ~ rlang::syms("force"),
    str_detect(query, "qs") ~ rlang::syms("stop"),
    str_detect(query, "qt") ~ rlang::syms("ticket")
  )
}

