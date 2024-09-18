
# currently in onTrack, but eventually move to its own DR package
# helper_load_google_survey -- in loading instructions, first load googlesheets4 API to load whole thing and run through process below
# output of this will be what goes to next phase

# might want to change cleanGoogle function ex. changing column names
# use Google Sheets templates to change values

# xwalk helper that copies xwalks -- in another helper script? not to worry about this extra step right now
# possible to copy from period to period, or district to district

clean_to_process_dr_google_forms <- function(district,period,raw_file_xwalk) {
  writeLines("\n Calling DR Google Forms - clean_to_process")

  # print(raw_file_xwalk$CORE_Value)
  # if(staticValues$clean_files$dr_google_forms_survey %in% raw_file_xwalk$CORE_Value){
  #
  #   writeLines("\n Calling DR Google Forms - clean_to_process")

    # dr_google_forms_df <- open_file(
    #   district = district,
    #   period = period,
    #   location = staticValues$locations$egnyte_clean,
    #   file_type = staticValues$clean_files$dr_google_forms_survey
    # )
    #
    # print(dr_google_forms_df)

    ################################################################################
  google_sheet <- paste0('https://docs.google.com/spreadsheets/d/', staticValues$google_urls[[district]], '/edit?usp=sharing')

  google_sheet_read <- lapply(google_sheet,read_sheet,col_types = "c") %>%
    Reduce(f = rbind)

  dr_google_forms_df <- cleanGoogle(google_sheet_read)

  dr_google_forms_df <- dr_google_forms_df %>%
    mutate(timestamp_utc = format(as.POSIXct(timestamp_utc, format = "%m/%d/%Y %H:%M:%S"), "%Y-%m-%d %H:%M:%S"))


    ################################################################################

    dr_google_forms_df_process_path <- universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
      ### TO DO across all clean_to_process
      file = staticValues$data_elements$dr_survey # data_elements instead of clean_files; dr_survey should be the only clean_files name
    )
    saveOrArchive(dr_google_forms_df,dr_google_forms_df_process_path)
  }
# }
