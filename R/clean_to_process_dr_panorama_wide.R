clean_to_process_dr_panorama_wide <- function(district,period,raw_file_xwalk) {
  # print(raw_file_xwalk$CORE_Value)
  if(staticValues$clean_files$dr_panorama_wide_survey %in% raw_file_xwalk$CORE_Value){

    writeLines("\n Calling DR Panorama Wide - clean_to_process")

    dr_panorama_wide_df <- open_file(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_clean,
      file_type = staticValues$clean_files$dr_panorama_wide_survey
    )

    print(dr_panorama_wide_df)

    # clean here


    ################ TO DO: clean 06_roster_panorama to work here ################
    fusd_surveyed <- fresno_conformed %>%
      filter(district_name_survey == "Fresno Unified") %>%
      select(student_id_number_survey,school_name_survey,timestamp_utc)

    # already updated for Fall 2024
    fusd_sheet <- 'https://docs.google.com/spreadsheets/d/1c-SusPB4I56Z5SbE9SKmm27BxRaQSobYHEWgK6Uo7KA/edit?usp=sharing'


    range_write(fusd_sheet,fusd_surveyed,col_names = FALSE,range = "a2")

    ################################################################################

    dr_panorama_wide_df_process_path <- universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
      ### TO DO across all clean_to_process
      file = staticValues$data_elements$dr_survey # data_elements instead of clean_files; dr_survey should be the only clean_files name
    )
    saveOrArchive(dr_panorama_wide_df,dr_panorama_wide_df_process_path)
  }
}
