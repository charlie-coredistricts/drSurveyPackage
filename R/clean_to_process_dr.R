#' Title
#'
#' @param district
#' @param period
#'
#' @return
#' @export
#'
#' @examples
clean_to_process_dr <- function(district,period){

  #functions that start with clean_to_process are for files that are used to build other files

  raw_file_xwalk <- open_xwalk(district,period,"xwalk","raw_files")
  print(raw_file_xwalk$CORE_Value)
  # survey_clean_to_process() - two branches - 1st is wide and 2nd is long

  ## wrapper - if file is wide (taken from what's written to G Drive), call appropriate fxn below
  if (raw_file_xwalk$CORE_Value == "dr_student_information"){
    clean_to_process_dr_student_information(district,period)
    ### TO DO: Add lines to bind_rows of all districts and add code from 110_create_google_sheet
  }  else if (raw_file_xwalk$CORE_Value == "dr_panorama_wide_survey"){
    clean_to_process_dr_panorama_wide(district,period,raw_file_xwalk)
  } else if(raw_file_xwalk$CORE_Value == "dr_panorama_long_survey"){
    clean_to_process_dr_panorama_long(district,period,raw_file_xwalk)
  } else if(raw_file_xwalk$CORE_Value == "dr_lausd_survey"){
    clean_to_process_dr_lausd(district,period,raw_file_xwalk)
  } else {
    clean_to_process_dr_google_forms(district,period,raw_file_xwalk)
    }
}
