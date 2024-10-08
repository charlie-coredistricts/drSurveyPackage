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
  # print(raw_file_xwalk$CORE_Value)

  if (raw_file_xwalk$CORE_Value == "dr_student_information"){
    clean_to_process_dr_student_information(district,period)
  } else if (raw_file_xwalk$CORE_Value == "dr_panorama_wide_survey"){
    clean_to_process_dr_panorama_wide(district,period,raw_file_xwalk)
  } else if(raw_file_xwalk$CORE_Value == "dr_panorama_long_survey"){
    clean_to_process_dr_panorama_long(district,period,raw_file_xwalk)
  } else if(raw_file_xwalk$CORE_Value == "dr_lausd_survey"){
    clean_to_process_dr_lausd(district,period,raw_file_xwalk)
  } else {
    clean_to_process_dr_google_forms(district,period,raw_file_xwalk)
  }

  ### integrate code from 110_create_google_sheet?

  # library("googlesheets4")
  #
  # rosters <- demographics_grade9_BTSC %>%
  #   select(first_name_demo,
  #          middle_name_demo,
  #          last_name_demo,
  #          email_address_demo,
  #          local_id_demo,
  #          ssid_demo,
  #          school_name_demo)
  #
  # #here is the URL of the google sheet that contains the macros for managing
  # #the school level rosters
  #
  # sheet_URL_string <- "https://docs.google.com/spreadsheets/d/1-J23teAPOqTt8j1ph91vtX57I1dpMnP9hP1yDg4gb5M/edit?usp=sharing"
  #
  # range_write(sheet_URL_string,rosters,col_names = FALSE,range = "a2")

}
