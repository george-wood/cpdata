repair_beats <- function(d, beats) {

  if (!"beat" %in% names(d)) {
    stop("data does not contain column named beat")
  }
  if (!"unit" %in% names(d)) {
    stop("data does not contain column named unit")
  }
  
  setDT(d)[
    !beat %in% beats$beat &
      substring(unit, 1, 2) == "00" &
      substring(beat, 1, 1) == substring(unit, 3, 3),
    beat := paste0("0", beat)
  ]
  
  return(d)

}
