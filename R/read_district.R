read_district <- function(path) {

  d <- 
    read_sf(path[str_detect(path, "districts.geojson")]) |> 
    transmute(unit = str_pad(dist_num, width = 3, side = "l", pad = "0"))
  
  demographics <- 
    get_acs(
      geography = "block group",
      state     = 17,
      county    = 031,
      year      = 2016,
      geometry  = TRUE,
      output    = "wide",
      variables = c(
        population = "B03002_001", # total
        white      = "B03002_003", # not hispanic or latino; white alone
        black      = "B03002_004", # not hispanic or latino; black alone
        hispanic   = "B03002_012"  # hispanic or latino
      )
    ) |> 
    rename_with(
      .cols = ends_with("E"),
      .fn   = \(x) str_remove(x, "E")
    )
  
  interpolated <-
    st_interpolate_aw(
      demographics[c("population", "white", "black", "hispanic")],
      to = st_transform(d, crs = st_crs(demographics)),
      extensive = TRUE
    ) |> 
    mutate(
      across(c(white, black, hispanic), \(x) x / population),
      unit     = d$unit,
      majority = case_when(
        black > 0.5    ~ "black",
        hispanic > 0.5 ~ "hispanic",
        white > 0.5    ~ "white",
        TRUE           ~ "none"
      )
    )
  
  # return sf
  #return(interpolated)
 
  # return dt
  interpolated <- st_drop_geometry(interpolated)
  setDT(interpolated)
  return(interpolated)
  
}

